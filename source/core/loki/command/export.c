/* export.c - MIDI export command (:export)
 *
 * Export compositions to Standard MIDI Files.
 *
 * Uses the editor-level loki_export_* functions which detect which
 * language has exportable events. Currently Alda supports export
 * (event-based model), while Joy uses immediate playback.
 */

#include "command_impl.h"
#include "loki/export.h"

/* :export - Export to MIDI file */
int cmd_export(editor_ctx_t *ctx, const char *args) {
    if (!args || !args[0]) {
        editor_set_status_msg(ctx, "Usage: :export <filename.mid>");
        return 0;
    }

    /* Check if any language has exportable events */
    if (!loki_export_available(ctx)) {
        editor_set_status_msg(ctx, "No events to export (play music code first)");
        return 0;
    }

    /* Export to MIDI file */
    if (loki_export_midi(ctx, args) == 0) {
        editor_set_status_msg(ctx, "%s exported", args);
        return 1;
    } else {
        const char *err = loki_export_error();
        editor_set_status_msg(ctx, "Export failed: %s", err ? err : "unknown error");
        return 0;
    }
}
