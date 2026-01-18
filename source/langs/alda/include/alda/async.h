/**
 * @file async.h
 * @brief Asynchronous event playback for Alda interpreter.
 *
 * This module enables non-blocking playback, allowing the REPL
 * to remain responsive while music plays.
 */

#ifndef ALDA_ASYNC_H
#define ALDA_ASYNC_H

#include "context.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Async Scheduler API
 * ============================================================================ */

/**
 * @brief Initialize the async playback system.
 *
 * Creates a libuv event loop in a background thread.
 * Must be called before any async playback.
 *
 * @return 0 on success, -1 on error.
 */
int alda_async_init(void);

/**
 * @brief Cleanup the async playback system.
 *
 * Stops all playback and shuts down the background thread.
 */
void alda_async_cleanup(void);

/**
 * @brief Completion callback type for async playback.
 *
 * @param slot_id The slot that completed.
 * @param stopped Non-zero if stopped by user, zero if completed naturally.
 * @param userdata User-provided data from alda_events_play_async_ex().
 */
typedef void (*AldaAsyncCompletionCallback)(int slot_id, int stopped, void *userdata);

/**
 * @brief Play events asynchronously.
 *
 * Copies the current events from context and plays them
 * in the background, returning immediately.
 *
 * @param ctx Alda context with scheduled events.
 * @return 0 on success, -1 on error.
 */
int alda_events_play_async(AldaContext* ctx);

/**
 * @brief Play events asynchronously with completion callback.
 *
 * Like alda_events_play_async() but with notification when playback completes.
 *
 * @param ctx Alda context with scheduled events.
 * @param callback Function to call when playback completes (can be NULL).
 * @param userdata User data passed to callback.
 * @return Slot ID on success (0 to MAX_SLOTS-1), -1 on error.
 */
int alda_events_play_async_ex(AldaContext* ctx, AldaAsyncCompletionCallback callback, void *userdata);

/**
 * @brief Stop all async playback.
 */
void alda_async_stop(void);

/**
 * @brief Check if async playback is active.
 * @return Non-zero if events are still playing.
 */
int alda_async_is_playing(void);

/**
 * @brief Wait for current async playback to complete.
 * @param timeout_ms Maximum time to wait in milliseconds, 0 = infinite.
 * @return 0 if playback completed, -1 if timed out.
 */
int alda_async_wait(int timeout_ms);

/**
 * @brief Get count of active playback slots.
 * @return Number of slots currently playing.
 */
int alda_async_active_count(void);

/**
 * @brief Enable or disable concurrent playback mode.
 *
 * In concurrent mode, new playback starts immediately without waiting
 * for previous playback to complete. This allows polyphonic playback
 * of multiple parts entered separately in the REPL.
 *
 * @param enabled Non-zero to enable concurrent mode.
 */
void alda_async_set_concurrent(int enabled);

/**
 * @brief Check if concurrent mode is enabled.
 * @return Non-zero if concurrent mode is active.
 */
int alda_async_get_concurrent(void);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_ASYNC_H */
