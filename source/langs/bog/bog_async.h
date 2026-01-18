/**
 * @file bog_async.h
 * @brief Asynchronous scheduler tick thread for Bog REPL.
 *
 * Runs the Bog scheduler in a background thread so the REPL can remain
 * responsive while the scheduler continuously evaluates rules and fires events.
 */

#ifndef BOG_ASYNC_H
#define BOG_ASYNC_H

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
struct BogScheduler;
struct BogTransitionManager;

/**
 * @brief Initialize the async tick system.
 *
 * Must be called before starting the tick thread.
 *
 * @return 0 on success, -1 on error.
 */
int bog_async_init(void);

/**
 * @brief Cleanup the async tick system.
 *
 * Stops the tick thread if running and releases resources.
 */
void bog_async_cleanup(void);

/**
 * @brief Start the background tick thread.
 *
 * Begins ticking the scheduler in a background thread at ~10ms intervals.
 * The scheduler must be started (bog_scheduler_start) before calling this.
 *
 * @param scheduler The Bog scheduler to tick.
 * @param transition The transition manager (may be NULL).
 * @return 0 on success, -1 on error.
 */
int bog_async_start(struct BogScheduler *scheduler,
                    struct BogTransitionManager *transition);

/**
 * @brief Stop the background tick thread.
 *
 * Signals the thread to stop and waits for it to exit.
 */
void bog_async_stop(void);

/**
 * @brief Check if the tick thread is running.
 * @return Non-zero if running.
 */
int bog_async_is_running(void);

#ifdef __cplusplus
}
#endif

#endif /* BOG_ASYNC_H */
