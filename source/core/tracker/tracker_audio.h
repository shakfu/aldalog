/**
 * tracker_audio.h - Audio integration for the tracker engine
 *
 * Connects the tracker engine to the shared audio backend, enabling
 * playback through TinySoundFont, FluidSynth, Csound, Minihost plugins,
 * or hardware MIDI.
 *
 * Usage:
 *   SharedContext* ctx = ...;
 *   TrackerEngine* engine = tracker_engine_new();
 *   tracker_audio_connect(engine, ctx);
 *
 *   // Now engine output goes to the shared backend
 *   tracker_engine_play(engine);
 */

#ifndef TRACKER_AUDIO_H
#define TRACKER_AUDIO_H

#include "tracker_engine.h"
#include "../shared/context.h"

#ifdef __cplusplus
extern "C" {
#endif

/*============================================================================
 * Connection Functions
 *============================================================================*/

/**
 * Connect the tracker engine to a shared audio context.
 *
 * This configures the engine's output callbacks to route through the
 * shared backend's priority system (Minihost > Csound > TSF > MIDI).
 *
 * @param engine  The tracker engine
 * @param ctx     Shared context for audio output (must remain valid)
 * @return        true on success
 */
bool tracker_audio_connect(TrackerEngine* engine, SharedContext* ctx);

/**
 * Disconnect the tracker engine from the shared audio context.
 *
 * Sends all notes off and clears the output callbacks.
 *
 * @param engine  The tracker engine
 */
void tracker_audio_disconnect(TrackerEngine* engine);

/**
 * Check if engine is connected to a shared context.
 *
 * @param engine  The tracker engine
 * @return        true if connected
 */
bool tracker_audio_is_connected(TrackerEngine* engine);

/**
 * Get the shared context connected to an engine.
 *
 * @param engine  The tracker engine
 * @return        The connected context, or NULL if not connected
 */
SharedContext* tracker_audio_get_context(TrackerEngine* engine);

/*============================================================================
 * Convenience Functions
 *============================================================================*/

/**
 * Create a new engine already connected to a shared context.
 *
 * @param ctx  Shared context for audio output
 * @return     New engine, or NULL on failure
 */
TrackerEngine* tracker_audio_engine_new(SharedContext* ctx);

/**
 * Create a new engine with config, connected to a shared context.
 *
 * @param config  Engine configuration
 * @param ctx     Shared context for audio output
 * @return        New engine, or NULL on failure
 */
TrackerEngine* tracker_audio_engine_new_with_config(
    const TrackerEngineConfig* config,
    SharedContext* ctx
);

/*============================================================================
 * Sync Integration
 *============================================================================*/

/**
 * Enable Ableton Link sync for the tracker engine.
 *
 * When enabled, the engine will sync its tempo and transport with
 * other Link-enabled applications.
 *
 * @param engine  The tracker engine
 * @param ctx     Shared context with Link enabled
 * @return        true if Link sync started successfully
 */
bool tracker_audio_enable_link_sync(TrackerEngine* engine, SharedContext* ctx);

/**
 * Disable Ableton Link sync.
 *
 * @param engine  The tracker engine
 */
void tracker_audio_disable_link_sync(TrackerEngine* engine);

/**
 * Update engine state from Link.
 *
 * Call this regularly (e.g., from a timer or audio callback) when
 * Link sync is enabled. Updates tempo and transport state.
 *
 * @param engine  The tracker engine
 * @param ctx     Shared context with Link enabled
 */
void tracker_audio_link_poll(TrackerEngine* engine, SharedContext* ctx);

#ifdef __cplusplus
}
#endif

#endif /* TRACKER_AUDIO_H */
