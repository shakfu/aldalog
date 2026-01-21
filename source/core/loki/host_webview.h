/* host_webview.h - Native webview host interface for running the editor
 *
 * This module provides a native webview-based host, enabling:
 * - Self-contained native window with embedded web UI
 * - Same xterm.js-based UI as the web host, but without needing a browser
 * - Direct JS bindings for event handling (no WebSocket needed)
 *
 * Build with -DBUILD_WEBVIEW_HOST=ON to enable this module.
 *
 * Platform requirements:
 * - macOS: WebKit framework (always available)
 * - Linux: GTK3 + WebKit2GTK dev packages
 * - Windows: WebView2 runtime
 */

#ifndef LOKI_HOST_WEBVIEW_H
#define LOKI_HOST_WEBVIEW_H

#include "host.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef LOKI_WEBVIEW_HOST

/**
 * Create a native webview host for self-contained editing.
 *
 * The webview host:
 * - Opens a native window with embedded WebKit/WebView2
 * - Loads the same xterm.js-based UI used by the web host
 * - Binds C functions callable from JavaScript
 * - Translates JS calls to EditorEvents
 * - Pushes ViewModel snapshots to the UI via JS evaluation
 *
 * @param title   Window title (e.g., "psnd - filename.alda")
 * @param width   Initial window width in pixels
 * @param height  Initial window height in pixels
 * @return Host instance, or NULL on error
 */
EditorHost *editor_host_webview_create(const char *title, int width, int height);

/**
 * Run the webview host main loop.
 *
 * This is a convenience function that creates a session and runs until
 * the session quits or the window is closed.
 *
 * @param title   Window title
 * @param width   Initial window width
 * @param height  Initial window height
 * @param config  Session configuration
 * @return Exit code (0 on success)
 */
int editor_host_webview_run(const char *title, int width, int height,
                            const EditorConfig *config);

#endif /* LOKI_WEBVIEW_HOST */

#ifdef __cplusplus
}
#endif

#endif /* LOKI_HOST_WEBVIEW_H */
