/**
 * @file bog_async.c
 * @brief Asynchronous scheduler tick thread for Bog REPL.
 *
 * Runs a background thread that continuously ticks the Bog scheduler,
 * allowing the REPL to remain responsive during live coding.
 */

#include "bog_async.h"
#include "scheduler.h"
#include "livecoding.h"

#include <pthread.h>
#include <unistd.h>
#include <stdatomic.h>

/* ============================================================================
 * Thread State
 * ============================================================================ */

static pthread_t g_tick_thread;
static atomic_int g_running = 0;
static atomic_int g_initialized = 0;

static BogScheduler *g_scheduler = NULL;
static BogTransitionManager *g_transition = NULL;

/* Tick interval in microseconds (10ms) */
#define TICK_INTERVAL_US 10000

/* ============================================================================
 * Tick Thread
 * ============================================================================ */

static void *tick_thread_func(void *arg) {
    (void)arg;

    while (atomic_load(&g_running)) {
        /* Tick scheduler */
        if (g_scheduler) {
            bog_scheduler_tick(g_scheduler);
        }

        /* Process transitions */
        if (g_transition && g_scheduler) {
            double now = bog_scheduler_now(g_scheduler);
            bog_transition_manager_process(g_transition, now);
        }

        /* Sleep before next tick - check running flag periodically for quick shutdown */
        for (int i = 0; i < 10 && atomic_load(&g_running); i++) {
            usleep(TICK_INTERVAL_US / 10);
        }
    }

    return NULL;
}

/* ============================================================================
 * Public API
 * ============================================================================ */

int bog_async_init(void) {
    if (atomic_load(&g_initialized)) {
        return 0;  /* Already initialized */
    }

    g_scheduler = NULL;
    g_transition = NULL;
    atomic_store(&g_running, 0);
    atomic_store(&g_initialized, 1);

    return 0;
}

void bog_async_cleanup(void) {
    if (!atomic_load(&g_initialized)) {
        return;
    }

    bog_async_stop();

    g_scheduler = NULL;
    g_transition = NULL;
    atomic_store(&g_initialized, 0);
}

int bog_async_start(BogScheduler *scheduler, BogTransitionManager *transition) {
    if (!atomic_load(&g_initialized)) {
        if (bog_async_init() != 0) {
            return -1;
        }
    }

    if (atomic_load(&g_running)) {
        /* Already running - update references */
        g_scheduler = scheduler;
        g_transition = transition;
        return 0;
    }

    g_scheduler = scheduler;
    g_transition = transition;
    atomic_store(&g_running, 1);

    if (pthread_create(&g_tick_thread, NULL, tick_thread_func, NULL) != 0) {
        atomic_store(&g_running, 0);
        return -1;
    }

    return 0;
}

void bog_async_stop(void) {
    if (!atomic_load(&g_running)) {
        return;
    }

    atomic_store(&g_running, 0);
    pthread_join(g_tick_thread, NULL);
}

int bog_async_is_running(void) {
    return atomic_load(&g_running);
}
