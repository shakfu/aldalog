/* host.h - Abstract host interface for running the editor
 *
 * This module defines an abstract interface for editor hosts, enabling:
 * - Terminal-based editing (default)
 * - HTTP server for web-based editing
 * - Headless scripting harness for automation
 * - Testing harness with programmatic input
 *
 * Each host type implements the EditorHost interface, handling:
 * - Input event sourcing (terminal, network, queue)
 * - Output rendering (terminal, JSON, null)
 * - Lifecycle management (setup, teardown)
 * - Platform-specific concerns (signals, threading)
 */

#ifndef LOKI_HOST_H
#define LOKI_HOST_H

#include "session.h"
#include "cli.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declaration */
typedef struct EditorHost EditorHost;

/**
 * EditorHostCallbacks - Callbacks for host lifecycle events.
 *
 * These are optional hooks that hosts can implement for customization.
 */
typedef struct {
    /* Called before main loop starts (after session created) */
    void (*on_start)(EditorHost *host, EditorSession *session);

    /* Called after each event is processed */
    void (*on_tick)(EditorHost *host, EditorSession *session);

    /* Called when session should quit */
    void (*on_quit)(EditorHost *host, EditorSession *session);

    /* Called on error */
    void (*on_error)(EditorHost *host, const char *message);
} EditorHostCallbacks;

/**
 * EditorHost - Abstract host for running an editor session.
 *
 * Hosts are responsible for:
 * - Creating and configuring the session
 * - Running the main event loop
 * - Handling platform-specific I/O
 * - Cleanup on exit
 */
struct EditorHost {
    /* Read next event (blocking with optional timeout).
     * Returns 0 on success, 1 on timeout, -1 on error.
     * Timeout of -1 means block indefinitely. */
    int (*read_event)(EditorHost *host, EditorEvent *event, int timeout_ms);

    /* Render current state. Called after each event.
     * Implementation depends on host type (terminal, HTTP response, etc.) */
    void (*render)(EditorHost *host, EditorSession *session);

    /* Check if host should continue running.
     * Returns 1 to continue, 0 to quit. */
    int (*should_continue)(EditorHost *host);

    /* Cleanup host resources. */
    void (*destroy)(EditorHost *host);

    /* Optional callbacks */
    EditorHostCallbacks callbacks;

    /* Host-specific data */
    void *data;
};

/**
 * Run an editor session with the given host.
 *
 * This is the main entry point for running the editor.
 * It creates a session, runs the main loop, and cleans up.
 *
 * @param host    Host to use for I/O
 * @param config  Session configuration
 * @return Exit code (0 on success)
 */
int editor_host_run(EditorHost *host, const EditorConfig *config);

/**
 * Run the main event loop.
 *
 * Called by editor_host_run() but can also be used directly
 * for more control over the loop.
 *
 * @param host     Host to use for I/O
 * @param session  Session to run
 * @return Exit code (0 on success, 1 on quit request)
 */
int editor_host_loop(EditorHost *host, EditorSession *session);

/* ======================= Terminal Host ===================================== */

/**
 * Create a terminal host for interactive editing.
 *
 * The terminal host:
 * - Enables raw mode on the terminal
 * - Reads keys from stdin
 * - Renders to stdout using VT100 sequences
 * - Handles SIGWINCH for resize
 *
 * @param input_fd  File descriptor for input (usually STDIN_FILENO)
 * @return Host instance, or NULL on error
 */
EditorHost *editor_host_terminal_create(int input_fd);

/* ======================= Headless Host ===================================== */

/**
 * Create a headless host for scripted/automated editing.
 *
 * The headless host:
 * - Does not render output
 * - Reads events from a queue (for programmatic input)
 * - Useful for testing and automation
 *
 * @return Host instance, or NULL on error
 */
EditorHost *editor_host_headless_create(void);

/**
 * Queue an event for the headless host.
 *
 * @param host   Headless host instance
 * @param event  Event to queue
 * @return 0 on success, -1 on error
 */
int editor_host_headless_queue_event(EditorHost *host, const EditorEvent *event);

/**
 * Queue a quit event for the headless host.
 *
 * @param host  Headless host instance
 */
void editor_host_headless_quit(EditorHost *host);

#ifdef __cplusplus
}
#endif

#endif /* LOKI_HOST_H */
