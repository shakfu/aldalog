/* export.c - Editor-level MIDI export control
 *
 * Language-agnostic MIDI export implementation.
 * Languages populate the shared MIDI event buffer before export.
 */

#include "export.h"
#include "loki/midi_export.h"
#include "shared/midi/events.h"
#include "lang_bridge.h"
#include <stddef.h>  /* NULL */

static const char *g_export_error = NULL;

int loki_export_available(editor_ctx_t *ctx) {
    /* Check if any language has exportable events */
    if (loki_lang_has_events(ctx)) {
        return 1;
    }

    /* Check shared buffer - might have events from other sources */
    return shared_midi_events_count() > 0;
}

int loki_export_midi(editor_ctx_t *ctx, const char *filename) {
    g_export_error = NULL;

    if (!filename || !*filename) {
        g_export_error = "No filename specified";
        return -1;
    }

    /* Populate shared buffer from language if available */
    loki_lang_populate_shared_buffer(ctx);

    /* Check if we have events to export */
    if (shared_midi_events_count() == 0) {
        g_export_error = "No events to export (play music code first)";
        return -1;
    }

    /* Export from shared buffer */
    int result = loki_midi_export_shared(filename);
    if (result != 0) {
        g_export_error = loki_midi_export_error();
    }
    return result;
}

const char *loki_export_error(void) {
    return g_export_error;
}
