/* host_webview.cpp - Native webview host implementation
 *
 * Provides a native window with embedded web content using the webview library.
 * Uses the same xterm.js-based UI as host_web.c but without requiring a browser.
 *
 * Event flow:
 *   User Input -> JS event handler -> psnd.send() binding
 *     -> Queue event in C++ -> webview_host_read_event() returns
 *     -> editor_session_handle_event() -> state update
 *     -> webview_host_render() -> webview_eval("psndHandleMessage(viewmodel)")
 */

#ifdef LOKI_WEBVIEW_HOST

// Define WEBVIEW_STATIC before including the amalgamated header
#define WEBVIEW_STATIC
#include "webview_amalgamation.h"

extern "C" {
#include "host_webview.h"
#include "host.h"
#include "session.h"
#include "event.h"
#include "json.h"
#include "jsonrpc.h"
#include "lang_bridge.h"
#include "host_web_ui.h"
}

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include <queue>
#include <string>
#include <atomic>

/* ======================= Constants ========================================= */

#define WEBVIEW_HOST_QUEUE_SIZE 256
#define WEBVIEW_HOST_DEFAULT_WIDTH 1024
#define WEBVIEW_HOST_DEFAULT_HEIGHT 768

/* ======================= Webview Host Data ================================= */

struct WebviewHostData {
    webview_t webview;
    EditorSession *session;

    // Event queue from JS
    std::queue<EditorEvent> event_queue;
    std::mutex queue_mutex;

    // Configuration
    std::string title;
    int width;
    int height;

    // Lifecycle flags (atomic for thread safety during shutdown)
    std::atomic<bool> running;
    std::atomic<bool> shutting_down;

    // Render state
    int needs_render;

    WebviewHostData()
        : webview(nullptr)
        , session(nullptr)
        , width(WEBVIEW_HOST_DEFAULT_WIDTH)
        , height(WEBVIEW_HOST_DEFAULT_HEIGHT)
        , running(true)
        , shutting_down(false)
        , needs_render(0)
    {}
};

/* ======================= Event Queue ======================================= */

static int webview_host_queue_event(WebviewHostData *data, const EditorEvent *event) {
    if (!data || data->shutting_down.load()) return -1;

    std::lock_guard<std::mutex> lock(data->queue_mutex);

    if (data->event_queue.size() >= WEBVIEW_HOST_QUEUE_SIZE) {
        return -1; // Queue full
    }

    data->event_queue.push(*event);
    return 0;
}

static int webview_host_dequeue_event(WebviewHostData *data, EditorEvent *event) {
    if (!data || data->shutting_down.load()) return -1;

    std::lock_guard<std::mutex> lock(data->queue_mutex);

    if (data->event_queue.empty()) {
        return -1; // Queue empty
    }

    *event = data->event_queue.front();
    data->event_queue.pop();
    return 0;
}

/* ======================= JS Binding Callback =============================== */

// Called from JavaScript: window.psnd(json_string)
static void webview_host_js_callback(const char *id, const char *req, void *arg) {
    WebviewHostData *data = static_cast<WebviewHostData*>(arg);
    if (!data || data->shutting_down.load() || !data->running.load() || !data->webview) {
        return; // Shutting down, ignore
    }


    // Parse the JSON request - req is a JSON array of arguments
    // We expect: ["{ json message }"]
    JsonValue args = json_parse(req);
    if (args.type != JSON_ARRAY || args.data.array_val.count == 0) {
        webview_return(data->webview, id, 0, "");
        json_value_free(&args);
        return;
    }

    // Get the first argument (the JSON string from JS)
    JsonValue *first_arg = &args.data.array_val.items[0];
    if (first_arg->type != JSON_STRING) {
        webview_return(data->webview, id, 0, "");
        json_value_free(&args);
        return;
    }

    // Parse the actual message
    JsonValue msg = json_parse(first_arg->data.string_val.str);
    if (msg.type == JSON_ERROR) {
        webview_return(data->webview, id, 0, "");
        json_value_free(&args);
        return;
    }

    const char *cmd = json_object_get_string(&msg, "cmd");
    if (!cmd) {
        json_value_free(&msg);
        json_value_free(&args);
        webview_return(data->webview, id, 0, "");
        return;
    }

    EditorEvent event = {};

    if (strcmp(cmd, "event") == 0) {
        const char *type = json_object_get_string(&msg, "type");
        if (type && strcmp(type, "key") == 0) {
            int code = json_object_get_int(&msg, "code", 0);
            int mods = json_object_get_int(&msg, "modifiers", 0);

            event.type = EVENT_KEY;
            event.data.key.keycode = code;
            event.data.key.modifiers = static_cast<uint8_t>(mods);
            webview_host_queue_event(data, &event);
        } else if (type && strcmp(type, "mouse") == 0) {
            int x = json_object_get_int(&msg, "x", 0);
            int y = json_object_get_int(&msg, "y", 0);
            int button = json_object_get_int(&msg, "button", 0);
            int pressed = json_object_get_int(&msg, "pressed", 1);
            int mods = json_object_get_int(&msg, "modifiers", 0);

            event.type = EVENT_MOUSE;
            event.data.mouse.x = x;
            event.data.mouse.y = y;
            event.data.mouse.button = button;
            event.data.mouse.pressed = pressed;
            event.data.mouse.modifiers = static_cast<uint8_t>(mods);
            webview_host_queue_event(data, &event);
        }
    } else if (strcmp(cmd, "resize") == 0) {
        int rows = json_object_get_int(&msg, "rows", 24);
        int cols = json_object_get_int(&msg, "cols", 80);

        event.type = EVENT_RESIZE;
        event.data.resize.rows = rows;
        event.data.resize.cols = cols;
        webview_host_queue_event(data, &event);
    } else if (strcmp(cmd, "snapshot") == 0) {
        data->needs_render = 1;
    } else if (strcmp(cmd, "quit") == 0) {
        event.type = EVENT_QUIT;
        webview_host_queue_event(data, &event);
    }

    json_value_free(&msg);
    json_value_free(&args);
    webview_return(data->webview, id, 0, "");
}

/* ======================= Send ViewModel to JS ============================== */

static void webview_host_send_snapshot(WebviewHostData *data) {
    if (!data || data->shutting_down.load() || !data->webview || !data->session || !data->running.load()) {
        return;
    }

    EditorViewModel *vm = editor_session_snapshot(data->session);
    if (!vm) {
        return;
    }

    char *vm_json = jsonrpc_serialize_viewmodel(vm);
    editor_viewmodel_free(vm);

    if (!vm_json) {
        return;
    }

    // Build JS call: psndHandleMessage({type: "update", viewmodel: ...})
    std::string js = "if(window.psndHandleMessage){window.psndHandleMessage({type:\"update\",viewmodel:";
    js += vm_json;
    js += "});} else { console.log('psndHandleMessage not ready'); }";

    webview_eval(data->webview, js.c_str());

    free(vm_json);
}

/* ======================= EditorHost Interface ============================== */

static int webview_host_read_event(EditorHost *host, EditorEvent *event, int timeout_ms) {
    WebviewHostData *data = static_cast<WebviewHostData*>(host->data);
    (void)timeout_ms; // Webview handles its own event loop timing

    // Check if we have a queued event from JS
    if (webview_host_dequeue_event(data, event) == 0) {
        data->needs_render = 1;
        return 0; // Got event
    }

    return 1; // Timeout / no event
}

static void webview_host_render(EditorHost *host, EditorSession *session) {
    WebviewHostData *data = static_cast<WebviewHostData*>(host->data);

    // Store session reference for snapshot requests
    data->session = session;

    // Send snapshot when flagged
    if (data->needs_render) {
        webview_host_send_snapshot(data);
        data->needs_render = 0;
    }
}

static int webview_host_should_continue(EditorHost *host) {
    WebviewHostData *data = static_cast<WebviewHostData*>(host->data);
    return data && !data->shutting_down.load() && data->running.load() ? 1 : 0;
}

static void webview_host_destroy(EditorHost *host) {
    if (!host) return;

    WebviewHostData *data = static_cast<WebviewHostData*>(host->data);
    if (data) {
        // Signal shutdown first to stop all callbacks
        data->shutting_down.store(true);
        data->running.store(false);

        if (data->webview) {
            webview_destroy(data->webview);
            data->webview = nullptr;
        }
        delete data;
    }
    free(host);
}

/* ======================= Custom Event Loop ================================= */

// Dispatch function for webview_dispatch
struct DispatchData {
    EditorHost *host;
    EditorSession *session;
};

static void webview_tick_callback(webview_t w, void *arg) {
    DispatchData *dd = static_cast<DispatchData*>(arg);
    if (!dd || !dd->host || !dd->session) return;

    EditorHost *host = dd->host;
    EditorSession *session = dd->session;
    WebviewHostData *data = static_cast<WebviewHostData*>(host->data);

    // Early exit if shutting down
    if (!data || data->shutting_down.load() || !data->running.load()) return;

    // Process one event if available
    EditorEvent event;
    if (webview_host_dequeue_event(data, &event) == 0) {
        // Check again before processing
        if (data->shutting_down.load()) return;

        int result = editor_session_handle_event(session, &event);
        data->needs_render = 1;

        // Check for quit (handle_event returns 1 when quit is requested)
        if (result == 1) {
            data->running.store(false);
            webview_terminate(w);
            return; // Don't schedule another tick
        }
    }

    // Render if needed (check shutdown again)
    if (!data->shutting_down.load() && data->needs_render) {
        webview_host_send_snapshot(data);
        data->needs_render = 0;
    }

    // Schedule next tick if still running and not shutting down
    if (!data->shutting_down.load() && data->running.load()) {
        webview_dispatch(w, webview_tick_callback, arg);
    }
}

/* ======================= Public API ======================================== */

extern "C" {

EditorHost *editor_host_webview_create(const char *title, int width, int height) {
    EditorHost *host = static_cast<EditorHost*>(calloc(1, sizeof(EditorHost)));
    if (!host) return nullptr;

    WebviewHostData *data = new (std::nothrow) WebviewHostData();
    if (!data) {
        free(host);
        return nullptr;
    }

    data->title = title ? title : "psnd";
    data->width = width > 0 ? width : WEBVIEW_HOST_DEFAULT_WIDTH;
    data->height = height > 0 ? height : WEBVIEW_HOST_DEFAULT_HEIGHT;

    // Create webview (debug=1 to enable developer tools for debugging)
    data->webview = webview_create(1, nullptr);
    if (!data->webview) {
        fprintf(stderr, "Error: Failed to create webview\n");
        delete data;
        free(host);
        return nullptr;
    }

    webview_set_title(data->webview, data->title.c_str());
    webview_set_size(data->webview, data->width, data->height, WEBVIEW_HINT_NONE);

    // Bind JS function: window.psnd(json)
    webview_bind(data->webview, "psnd", webview_host_js_callback, data);

    // Load the embedded HTML UI
    webview_set_html(data->webview, EMBEDDED_HTML);

    // Set up host interface
    host->read_event = webview_host_read_event;
    host->render = webview_host_render;
    host->should_continue = webview_host_should_continue;
    host->destroy = webview_host_destroy;
    host->data = data;

    return host;
}

int editor_host_webview_run(const char *title, int width, int height,
                            const EditorConfig *config) {
    // Register all languages before creating session
    loki_lang_init();

    EditorHost *host = editor_host_webview_create(title, width, height);
    if (!host) {
        return 1;
    }

    WebviewHostData *data = static_cast<WebviewHostData*>(host->data);

    // Create session
    EditorSession *session = editor_session_new(config);
    if (!session) {
        host->destroy(host);
        return 1;
    }

    // Store session for rendering
    data->session = session;

    // Request initial render
    data->needs_render = 1;

    // Set up dispatch data for the tick callback (must persist during event loop)
    DispatchData *dd = new DispatchData();
    dd->host = host;
    dd->session = session;

    // Schedule initial tick
    webview_dispatch(data->webview, webview_tick_callback, dd);

    // Run the webview event loop (blocks until window closed)
    webview_run(data->webview);

    // Signal shutdown IMMEDIATELY to stop all callbacks
    data->shutting_down.store(true);
    data->running.store(false);

    // Null out session pointer first (callbacks check this)
    EditorSession *sess_to_free = data->session;
    data->session = nullptr;

    // Clean up dispatch data (invalidate the pointers)
    dd->host = nullptr;
    dd->session = nullptr;
    delete dd;
    dd = nullptr;

    // Now safe to free session
    if (sess_to_free) {
        editor_session_free(sess_to_free);
    }

    // Note: Do NOT call webview_destroy here.
    // When the window is closed by the user, the webview is already being destroyed
    // by the underlying framework (WebKit on macOS). Calling webview_destroy again
    // causes a double-free or use-after-free crash.
    // The webview_run() returning indicates cleanup has already started.
    data->webview = nullptr;

    // Free host data (don't call host->destroy as webview is already destroyed)
    host->data = nullptr;
    delete data;
    free(host);

    return 0;
}

} // extern "C"

#endif /* LOKI_WEBVIEW_HOST */
