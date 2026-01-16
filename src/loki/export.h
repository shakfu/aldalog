/* export.h - Editor-level MIDI export control
 *
 * Language-agnostic MIDI export interface.
 * Currently supports Alda (event-based model).
 * Joy uses immediate playback and doesn't have exportable events.
 */

#ifndef LOKI_EXPORT_H
#define LOKI_EXPORT_H

/* Forward declarations */
struct editor_ctx;
typedef struct editor_ctx editor_ctx_t;

/**
 * Check if MIDI export is available.
 * Returns true if any language has exportable events.
 *
 * @param ctx Editor context
 * @return 1 if export available, 0 if not
 */
int loki_export_available(editor_ctx_t *ctx);

/**
 * Export to a Standard MIDI File.
 * Exports events from whichever language has exportable content.
 *
 * @param ctx Editor context
 * @param filename Output filename (should end in .mid)
 * @return 0 on success, -1 on error
 */
int loki_export_midi(editor_ctx_t *ctx, const char *filename);

/**
 * Get the last error message from a failed export.
 *
 * @return Error message string, or NULL if no error
 */
const char *loki_export_error(void);

#endif /* LOKI_EXPORT_H */
