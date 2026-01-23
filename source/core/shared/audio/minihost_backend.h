/**
 * @file minihost_backend.h
 * @brief VST3/AU plugin host backend using libminihost.
 *
 * Provides plugin loading, MIDI event dispatch, and audio processing
 * for software instruments and effects. Supports up to 8 plugin slots
 * (slot 0 for instrument, slots 1-7 for effects chain).
 *
 * Build with -DBUILD_MINIHOST_BACKEND=ON to enable.
 */

#ifndef MINIHOST_BACKEND_H
#define MINIHOST_BACKEND_H

#ifdef __cplusplus
extern "C" {
#endif

/** Maximum number of plugin slots (1 instrument + 7 effects) */
#define MINIHOST_MAX_PLUGINS 8

/* ============================================================================
 * Initialization and Cleanup
 * ============================================================================ */

/**
 * @brief Initialize the minihost backend.
 * @return 0 on success, -1 on error.
 */
int shared_minihost_init(void);

/**
 * @brief Cleanup minihost resources and unload all plugins.
 */
void shared_minihost_cleanup(void);

/**
 * @brief Check if minihost backend is available (compiled in).
 * @return Non-zero if available, 0 if not compiled in.
 */
int shared_minihost_is_available(void);

/**
 * @brief Set log file for plugin debug output.
 * By default, plugin stderr is redirected to /dev/null.
 * Call this before loading plugins to capture debug output.
 * @param path Path to log file, or NULL to suppress output.
 */
void shared_minihost_set_log_file(const char* path);

/* ============================================================================
 * Plugin Loading
 * ============================================================================ */

/**
 * @brief Load a VST3 or AU plugin into a slot.
 * @param slot Plugin slot (0-7). Slot 0 is typically the instrument.
 * @param path Path to the plugin bundle (.vst3 or .component).
 * @return 0 on success, -1 on error.
 */
int shared_minihost_load(int slot, const char* path);

/**
 * @brief Unload plugin from a slot.
 * @param slot Plugin slot (0-7).
 */
void shared_minihost_unload(int slot);

/**
 * @brief Check if a plugin is loaded in a slot.
 * @param slot Plugin slot (0-7).
 * @return Non-zero if plugin is loaded, 0 if empty.
 */
int shared_minihost_has_plugin(int slot);

/**
 * @brief Get the name of the loaded plugin.
 * @param slot Plugin slot (0-7).
 * @return Plugin name, or NULL if no plugin loaded.
 */
const char* shared_minihost_get_plugin_name(int slot);

/* ============================================================================
 * Enable/Disable
 * ============================================================================ */

/**
 * @brief Enable the minihost backend (starts audio output).
 * @return 0 on success, -1 on error.
 */
int shared_minihost_enable(void);

/**
 * @brief Disable the minihost backend (stops audio output).
 */
void shared_minihost_disable(void);

/**
 * @brief Check if minihost is enabled.
 * @return Non-zero if enabled, 0 if disabled.
 */
int shared_minihost_is_enabled(void);

/* ============================================================================
 * MIDI Event Dispatch
 * ============================================================================ */

/**
 * @brief Send a note-on message to the instrument plugin (slot 0).
 * @param channel MIDI channel (1-16).
 * @param pitch Note pitch (0-127).
 * @param velocity Note velocity (0-127).
 */
void shared_minihost_send_note_on(int channel, int pitch, int velocity);

/**
 * @brief Send a note-off message to the instrument plugin (slot 0).
 * @param channel MIDI channel (1-16).
 * @param pitch Note pitch (0-127).
 */
void shared_minihost_send_note_off(int channel, int pitch);

/**
 * @brief Send a control change to the instrument plugin (slot 0).
 * @param channel MIDI channel (1-16).
 * @param cc Controller number (0-127).
 * @param value Controller value (0-127).
 */
void shared_minihost_send_cc(int channel, int cc, int value);

/**
 * @brief Send a program change to the instrument plugin (slot 0).
 * @param channel MIDI channel (1-16).
 * @param program Program number (0-127).
 */
void shared_minihost_send_program(int channel, int program);

/**
 * @brief Send pitch bend to the instrument plugin (slot 0).
 * @param channel MIDI channel (1-16).
 * @param bend Pitch bend value (-8192 to 8191, 0 = center).
 */
void shared_minihost_send_pitch_bend(int channel, int bend);

/**
 * @brief Send all notes off to the instrument plugin.
 */
void shared_minihost_all_notes_off(void);

/* ============================================================================
 * Parameter Control
 * ============================================================================ */

/**
 * @brief Get the number of parameters for a plugin.
 * @param slot Plugin slot (0-7).
 * @return Number of parameters, or 0 if no plugin loaded.
 */
int shared_minihost_get_num_params(int slot);

/**
 * @brief Get a parameter value.
 * @param slot Plugin slot (0-7).
 * @param index Parameter index.
 * @return Parameter value (0.0-1.0), or 0 on error.
 */
float shared_minihost_get_param(int slot, int index);

/**
 * @brief Set a parameter value.
 * @param slot Plugin slot (0-7).
 * @param index Parameter index.
 * @param value Parameter value (0.0-1.0).
 * @return 0 on success, -1 on error.
 */
int shared_minihost_set_param(int slot, int index, float value);

/**
 * @brief Get a parameter name.
 * @param slot Plugin slot (0-7).
 * @param index Parameter index.
 * @param buf Buffer to write name into.
 * @param size Buffer size.
 * @return 0 on success, -1 on error.
 */
int shared_minihost_get_param_name(int slot, int index, char* buf, int size);

/* ============================================================================
 * Preset/Program Control
 * ============================================================================ */

/**
 * @brief Get the number of presets for a plugin.
 * @param slot Plugin slot (0-7).
 * @return Number of presets, or 0 if no plugin loaded.
 */
int shared_minihost_get_num_presets(int slot);

/**
 * @brief Get the current preset index.
 * @param slot Plugin slot (0-7).
 * @return Current preset index, or -1 on error.
 */
int shared_minihost_get_preset(int slot);

/**
 * @brief Set the current preset by index.
 * @param slot Plugin slot (0-7).
 * @param index Preset index.
 * @return 0 on success, -1 on error.
 */
int shared_minihost_set_preset(int slot, int index);

/**
 * @brief Get a preset name by index.
 * @param slot Plugin slot (0-7).
 * @param index Preset index.
 * @param buf Buffer to write name into.
 * @param size Buffer size.
 * @return 0 on success, -1 on error.
 */
int shared_minihost_get_preset_name(int slot, int index, char* buf, int size);

/* ============================================================================
 * State Persistence
 * ============================================================================ */

/**
 * @brief Save plugin state to a file.
 * @param slot Plugin slot (0-7).
 * @param path Path to save state file.
 * @return 0 on success, -1 on error.
 */
int shared_minihost_save_state(int slot, const char* path);

/**
 * @brief Load plugin state from a file.
 * @param slot Plugin slot (0-7).
 * @param path Path to state file.
 * @return 0 on success, -1 on error.
 */
int shared_minihost_load_state(int slot, const char* path);

/* ============================================================================
 * Plugin Scanning
 * ============================================================================ */

/**
 * @brief Callback type for plugin scanning.
 * @param name Plugin name.
 * @param path Full path to plugin bundle.
 * @param userdata User-provided data.
 */
typedef void (*minihost_scan_callback)(const char* name, const char* path, void* userdata);

/**
 * @brief Scan a directory for VST3/AU plugins.
 * @param path Directory to scan.
 * @param callback Function called for each plugin found.
 * @param userdata User data passed to callback.
 * @return Number of plugins found, or -1 on error.
 */
int shared_minihost_scan_directory(const char* path,
    minihost_scan_callback callback, void* userdata);

/* ============================================================================
 * Audio Processing (called from audio callback)
 * ============================================================================ */

/**
 * @brief Process audio block through the plugin chain.
 * Called from the miniaudio callback to mix plugin output.
 * @param output Interleaved stereo output buffer.
 * @param frames Number of frames to process.
 */
void shared_minihost_process_block(float* output, int frames);

/**
 * @brief Get the current sample rate.
 * @return Sample rate in Hz.
 */
int shared_minihost_get_sample_rate(void);

/**
 * @brief Set the sample rate (must be called before loading plugins).
 * @param rate Sample rate in Hz.
 */
void shared_minihost_set_sample_rate(int rate);

#ifdef __cplusplus
}
#endif

#endif /* MINIHOST_BACKEND_H */
