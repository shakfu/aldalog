/* live_loop.h - Live looping for concurrent multi-language playback
 *
 * Enables buffers to continuously re-evaluate their content on beat boundaries,
 * synchronized via Ableton Link. Multiple buffers can loop independently,
 * each sending MIDI to different channels.
 *
 * Usage:
 *   :loop 4     - Re-evaluate current buffer every 4 beats
 *   :stop       - Stop the loop
 */

#ifndef LOKI_LIVE_LOOP_H
#define LOKI_LIVE_LOOP_H

#include "loki/core.h"

/* Maximum number of concurrent loops */
#define LIVE_LOOP_MAX 16

/* Start loop for current buffer.
 * Re-evaluates buffer content every `beats` beats, synced to Link.
 *
 * @param ctx Editor context
 * @param beats Interval in beats (e.g., 4.0 for every bar in 4/4)
 * @return 0 on success, -1 if max loops reached
 */
int live_loop_start(editor_ctx_t *ctx, double beats);

/* Stop loop for current buffer.
 *
 * @param ctx Editor context
 */
void live_loop_stop(editor_ctx_t *ctx);

/* Stop loop for specific buffer by ID.
 *
 * @param buffer_id Buffer ID
 */
void live_loop_stop_buffer(int buffer_id);

/* Check if current buffer has an active loop.
 *
 * @param ctx Editor context
 * @return 1 if looping, 0 otherwise
 */
int live_loop_is_active(editor_ctx_t *ctx);

/* Check if specific buffer has an active loop.
 *
 * @param buffer_id Buffer ID
 * @return 1 if looping, 0 otherwise
 */
int live_loop_is_active_buffer(int buffer_id);

/* Get loop interval for current buffer.
 *
 * @param ctx Editor context
 * @return Beat interval, or 0 if not looping
 */
double live_loop_get_interval(editor_ctx_t *ctx);

/* Called from main loop - checks Link beat and fires due loops.
 * Should be called frequently (e.g., every frame/tick).
 */
void live_loop_tick(void);

/* Cleanup all loops. Called on editor shutdown. */
void live_loop_shutdown(void);

#endif /* LOKI_LIVE_LOOP_H */
