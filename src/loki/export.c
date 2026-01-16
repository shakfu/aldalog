/* export.c - Editor-level MIDI export control
 *
 * Language-agnostic MIDI export implementation.
 * Detects which language has exportable events and exports accordingly.
 */

#include "export.h"
#include "alda.h"
#include "loki/midi_export.h"

static const char *g_export_error = NULL;

int loki_export_available(editor_ctx_t *ctx) {
    /* Check Alda - has event-based model */
    if (loki_alda_is_initialized(ctx)) {
        int event_count = 0;
        const void *events = loki_alda_get_events(ctx, &event_count);
        if (events && event_count > 0) {
            return 1;
        }
    }

    /* Joy uses immediate playback - no exportable events */
    /* Future languages could be checked here */

    return 0;
}

int loki_export_midi(editor_ctx_t *ctx, const char *filename) {
    g_export_error = NULL;

    if (!filename || !*filename) {
        g_export_error = "No filename specified";
        return -1;
    }

    /* Try Alda export */
    if (loki_alda_is_initialized(ctx)) {
        int event_count = 0;
        const void *events = loki_alda_get_events(ctx, &event_count);
        if (events && event_count > 0) {
            int result = loki_midi_export(ctx, filename);
            if (result != 0) {
                g_export_error = loki_midi_export_error();
            }
            return result;
        }
    }

    /* No exportable content found */
    g_export_error = "No events to export (play music code first)";
    return -1;
}

const char *loki_export_error(void) {
    return g_export_error;
}
