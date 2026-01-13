/* loki/midi_export.h - MIDI file export for Alda compositions
 *
 * Provides functionality to export Alda music notation to Standard MIDI Files.
 * Uses the midifile library (BSD-2-Clause) for MIDI file generation.
 *
 * Usage:
 *   - From ex-command: :export filename.mid
 *   - From Lua: loki.midi.export("filename.mid")
 */

#ifndef LOKI_MIDI_EXPORT_H
#define LOKI_MIDI_EXPORT_H

/* Forward declarations */
struct editor_ctx;
typedef struct editor_ctx editor_ctx_t;

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Export current Alda events to a Standard MIDI File.
 *
 * Converts the AldaScheduledEvent array from the current Alda context
 * into a MIDI file. Exports as Type 0 (single track) for single-channel
 * compositions, or Type 1 (multi-track) for multi-channel compositions.
 *
 * @param ctx Editor context (used for Alda state access)
 * @param filename Output filename (should end in .mid)
 * @return 0 on success, -1 on error
 */
int loki_midi_export(editor_ctx_t *ctx, const char *filename);

/**
 * Get the last error message from a failed export.
 *
 * @return Error message string, or NULL if no error
 * @note String is valid until next loki_midi_export() call
 */
const char *loki_midi_export_error(void);

#ifdef __cplusplus
}
#endif

#endif /* LOKI_MIDI_EXPORT_H */
