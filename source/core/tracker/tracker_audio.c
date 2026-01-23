/**
 * tracker_audio.c - Audio integration for the tracker engine
 *
 * Implements the adapter between TrackerOutput callbacks and the
 * shared audio backend.
 */

#include "tracker_audio.h"
#include "../shared/link/link.h"
#include <string.h>

/*============================================================================
 * Output Callback Implementations
 *
 * These functions are called by the tracker engine and forward events
 * to the shared audio backend.
 *
 * Note: Tracker uses 0-based channels, shared backend uses 1-based.
 *============================================================================*/

static void output_note_on(void* user_data, uint8_t channel,
                           uint8_t note, uint8_t velocity) {
    SharedContext* ctx = (SharedContext*)user_data;
    if (!ctx) return;

    /* Convert 0-based to 1-based channel */
    shared_send_note_on(ctx, channel + 1, note, velocity);
}

static void output_note_off(void* user_data, uint8_t channel,
                            uint8_t note, uint8_t velocity) {
    SharedContext* ctx = (SharedContext*)user_data;
    if (!ctx) return;
    (void)velocity;  /* Shared backend doesn't use release velocity */

    shared_send_note_off(ctx, channel + 1, note);
}

static void output_cc(void* user_data, uint8_t channel,
                      uint8_t cc_number, uint8_t value) {
    SharedContext* ctx = (SharedContext*)user_data;
    if (!ctx) return;

    shared_send_cc(ctx, channel + 1, cc_number, value);
}

static void output_program_change(void* user_data, uint8_t channel,
                                  uint8_t program) {
    SharedContext* ctx = (SharedContext*)user_data;
    if (!ctx) return;

    shared_send_program(ctx, channel + 1, program);
}

static void output_all_notes_off(void* user_data, uint8_t channel) {
    SharedContext* ctx = (SharedContext*)user_data;
    if (!ctx) return;
    (void)channel;  /* Shared backend's panic is global */

    shared_send_panic(ctx);
}

/*============================================================================
 * Connection Functions
 *============================================================================*/

bool tracker_audio_connect(TrackerEngine* engine, SharedContext* ctx) {
    if (!engine || !ctx) return false;

    TrackerOutput output = {0};

    output.user_data = ctx;
    output.note_on = output_note_on;
    output.note_off = output_note_off;
    output.cc = output_cc;
    output.program_change = output_program_change;
    output.all_notes_off = output_all_notes_off;

    /* Note: pitch_bend, aftertouch, poly_aftertouch not supported by
     * the shared backend currently - leave as NULL */

    tracker_engine_set_output(engine, &output);

    return true;
}

void tracker_audio_disconnect(TrackerEngine* engine) {
    if (!engine) return;

    /* Get current context and send panic */
    const TrackerOutput* output = tracker_engine_get_output(engine);
    if (output && output->user_data) {
        SharedContext* ctx = (SharedContext*)output->user_data;
        shared_send_panic(ctx);
    }

    /* Clear output */
    TrackerOutput empty = {0};
    tracker_engine_set_output(engine, &empty);
}

bool tracker_audio_is_connected(TrackerEngine* engine) {
    if (!engine) return false;

    const TrackerOutput* output = tracker_engine_get_output(engine);
    return output && output->user_data && output->note_on;
}

SharedContext* tracker_audio_get_context(TrackerEngine* engine) {
    if (!engine) return NULL;

    const TrackerOutput* output = tracker_engine_get_output(engine);
    if (!output) return NULL;

    return (SharedContext*)output->user_data;
}

/*============================================================================
 * Convenience Functions
 *============================================================================*/

TrackerEngine* tracker_audio_engine_new(SharedContext* ctx) {
    TrackerEngine* engine = tracker_engine_new();
    if (!engine) return NULL;

    if (!tracker_audio_connect(engine, ctx)) {
        tracker_engine_free(engine);
        return NULL;
    }

    /* Sync tempo from context */
    if (ctx) {
        int tempo = shared_effective_tempo(ctx);
        if (tempo > 0) {
            tracker_engine_set_bpm(engine, tempo);
        }
    }

    return engine;
}

TrackerEngine* tracker_audio_engine_new_with_config(
    const TrackerEngineConfig* config,
    SharedContext* ctx
) {
    TrackerEngine* engine = tracker_engine_new_with_config(config);
    if (!engine) return NULL;

    if (!tracker_audio_connect(engine, ctx)) {
        tracker_engine_free(engine);
        return NULL;
    }

    /* Sync tempo from context */
    if (ctx) {
        int tempo = shared_effective_tempo(ctx);
        if (tempo > 0) {
            tracker_engine_set_bpm(engine, tempo);
        }
    }

    return engine;
}

/*============================================================================
 * Link Sync Integration
 *============================================================================*/

bool tracker_audio_enable_link_sync(TrackerEngine* engine, SharedContext* ctx) {
    if (!engine || !ctx) return false;

    /* Check if Link is available and enabled */
    if (!ctx->link_enabled || !shared_link_is_enabled()) {
        return false;
    }

    /* Set engine to external Link sync mode */
    tracker_engine_set_sync_mode(engine, TRACKER_SYNC_EXTERNAL_LINK);

    /* Initial sync */
    tracker_audio_link_poll(engine, ctx);

    return true;
}

void tracker_audio_disable_link_sync(TrackerEngine* engine) {
    if (!engine) return;

    tracker_engine_set_sync_mode(engine, TRACKER_SYNC_INTERNAL);
}

void tracker_audio_link_poll(TrackerEngine* engine, SharedContext* ctx) {
    if (!engine || !ctx) return;
    if (!ctx->link_enabled || !shared_link_is_enabled()) return;

    /* Get current Link state */
    /* Use quantum of 4 for 4/4 time (standard for most music) */
    double quantum = 4.0;
    double tempo = shared_link_get_tempo();
    double beat = shared_link_get_beat(quantum);
    bool playing = shared_link_is_playing();

    /* Update engine */
    tracker_engine_link_update(engine, beat, tempo, playing);
}
