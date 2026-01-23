// minihost.h
// Minimal audio plugin host library using JUCE
//
// Thread Safety:
//   - mh_process, mh_process_midi, mh_process_midi_io: Call from audio thread only.
//     These functions do NOT lock to avoid blocking the realtime audio thread.
//   - All other functions are thread-safe and can be called from any thread.
//     They use internal locking to protect plugin state.
//   - Do not call mh_close while another thread is using the plugin.
//
#pragma once
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct MH_Plugin MH_Plugin;

// Plugin metadata (available without full instantiation via mh_probe)
typedef struct MH_PluginDesc {
    char name[256];
    char vendor[256];
    char version[64];
    char format[16];            // "VST3" or "AU"
    char unique_id[64];         // for state compatibility checking
    char path[1024];            // full path to plugin file (populated by mh_scan_directory)
    int accepts_midi;
    int produces_midi;
    int num_inputs;             // default input channel count
    int num_outputs;            // default output channel count
} MH_PluginDesc;

typedef struct MH_Info {
    int num_params;
    int num_input_ch;
    int num_output_ch;
    int latency_samples;
} MH_Info;

typedef struct MH_MidiEvent {
    int sample_offset;           // sample position within the block (0 to nframes-1)
    unsigned char status;        // MIDI status byte (e.g., 0x90 = note on, 0x80 = note off)
    unsigned char data1;         // first data byte (e.g., note number)
    unsigned char data2;         // second data byte (e.g., velocity)
} MH_MidiEvent;

#define MH_PARAM_NAME_LEN 128

typedef struct MH_ParamInfo {
    char name[MH_PARAM_NAME_LEN];          // parameter name
    char label[MH_PARAM_NAME_LEN];         // unit label (e.g., "dB", "Hz", "%")
    char current_value_str[MH_PARAM_NAME_LEN]; // current value as display string
    float min_value;                       // minimum normalized value (usually 0.0)
    float max_value;                       // maximum normalized value (usually 1.0)
    float default_value;                   // default normalized value
    int num_steps;                         // number of discrete steps (0 = continuous)
    int is_automatable;                    // 1 if parameter can be automated
    int is_boolean;                        // 1 if parameter is a toggle/switch
} MH_ParamInfo;

typedef struct MH_TransportInfo {
    double bpm;                            // tempo in beats per minute
    int time_sig_numerator;                // time signature numerator (e.g., 4 for 4/4)
    int time_sig_denominator;              // time signature denominator (e.g., 4 for 4/4)
    long long position_samples;            // playhead position in samples
    double position_beats;                 // playhead position in quarter notes
    int is_playing;                        // 1 if transport is playing
    int is_recording;                      // 1 if transport is recording
    int is_looping;                        // 1 if loop is enabled
    long long loop_start_samples;          // loop start in samples
    long long loop_end_samples;            // loop end in samples
} MH_TransportInfo;

// Sample-accurate parameter automation
typedef struct MH_ParamChange {
    int sample_offset;                     // sample position within the block (0 to nframes-1)
    int param_index;                       // parameter index
    float value;                           // normalized value (0.0 to 1.0)
} MH_ParamChange;

// Bus information for understanding plugin I/O topology
typedef struct MH_BusInfo {
    char name[64];                         // bus name (e.g., "Main", "Sidechain")
    int num_channels;                      // number of channels in this bus
    int is_main;                           // 1 if main bus, 0 if aux/sidechain
    int is_enabled;                        // 1 if bus is currently enabled
} MH_BusInfo;

// plugin_path: .vst3 bundle on macOS, .vst3 folder on Win/Linux, .component for AU (mac)
// returns NULL on failure
MH_Plugin* mh_open(const char* plugin_path,
                   double sample_rate,
                   int max_block_size,
                   int requested_in_ch,
                   int requested_out_ch,
                   char* err_buf,
                   size_t err_buf_size);

void mh_close(MH_Plugin* p);

int mh_get_info(MH_Plugin* p, MH_Info* out_info);

// Non-interleaved buffers: inputs[ch][nframes], outputs[ch][nframes]
// If in/out pointers are NULL, the host will supply silence / discard output.
int mh_process(MH_Plugin* p,
               const float* const* inputs,
               float* const* outputs,
               int nframes);

// Process with MIDI input
// midi_events: array of MIDI events to send to the plugin (can be NULL if num_midi_events is 0)
// num_midi_events: number of events in the array
int mh_process_midi(MH_Plugin* p,
                    const float* const* inputs,
                    float* const* outputs,
                    int nframes,
                    const MH_MidiEvent* midi_events,
                    int num_midi_events);

// Process with MIDI input and output
// midi_in/num_midi_in: input MIDI events (can be NULL/0)
// midi_out: buffer to receive output MIDI events (can be NULL to ignore)
// midi_out_capacity: size of midi_out buffer
// num_midi_out: receives actual number of output events (can be NULL)
// Returns 1 on success, 0 on failure
int mh_process_midi_io(MH_Plugin* p,
                       const float* const* inputs,
                       float* const* outputs,
                       int nframes,
                       const MH_MidiEvent* midi_in,
                       int num_midi_in,
                       MH_MidiEvent* midi_out,
                       int midi_out_capacity,
                       int* num_midi_out);

// Params by index (JUCE parameter ordering)
int   mh_get_num_params(MH_Plugin* p);
float mh_get_param(MH_Plugin* p, int index);
int   mh_set_param(MH_Plugin* p, int index, float normalized_0_1);

// Get parameter metadata (returns 1 on success, 0 on failure)
int   mh_get_param_info(MH_Plugin* p, int index, MH_ParamInfo* out_info);

// State save/load (for presets and session recall)
// Returns size in bytes needed to store state, or 0 on error
int mh_get_state_size(MH_Plugin* p);

// Copy state into buffer. Returns 1 on success, 0 on failure.
// buffer must be at least mh_get_state_size() bytes
int mh_get_state(MH_Plugin* p, void* buffer, int buffer_size);

// Restore state from buffer. Returns 1 on success, 0 on failure.
int mh_set_state(MH_Plugin* p, const void* data, int data_size);

// Set transport info (call before mh_process to update tempo/position for plugins)
// Pass NULL to clear transport info (plugins will see "no transport")
int mh_set_transport(MH_Plugin* p, const MH_TransportInfo* transport);

// Get plugin tail length in seconds (for reverbs, delays - time needed after input stops)
// Returns 0.0 if plugin has no tail or on error
double mh_get_tail_seconds(MH_Plugin* p);

// Bypass control
// When bypassed, plugin passes audio through unchanged
int mh_get_bypass(MH_Plugin* p);                    // Returns 1 if bypassed, 0 if not
int mh_set_bypass(MH_Plugin* p, int bypass);        // Set bypass state (1 = bypassed)

// Latency compensation
// Returns plugin latency in samples (use to align with other signals)
// Note: Latency may change after parameter changes - re-query if needed
int mh_get_latency_samples(MH_Plugin* p);

// Process with sample-accurate parameter automation
// param_changes: array of parameter changes sorted by sample_offset
// num_param_changes: number of parameter changes
// Splits processing at change points for sample-accurate automation
// Also supports MIDI I/O (pass NULL/0 to ignore)
int mh_process_auto(MH_Plugin* p,
                    const float* const* inputs,
                    float* const* outputs,
                    int nframes,
                    const MH_MidiEvent* midi_in,
                    int num_midi_in,
                    MH_MidiEvent* midi_out,
                    int midi_out_capacity,
                    int* num_midi_out,
                    const MH_ParamChange* param_changes,
                    int num_param_changes);

// Reset plugin internal state (clears delay lines, reverb tails, filter states)
// Call between unrelated audio segments to avoid artifacts
// Returns 1 on success, 0 on failure
int mh_reset(MH_Plugin* p);

// Set non-realtime mode for offline/batch processing
// When non_realtime=1, plugins may use higher-quality algorithms
// Returns 1 on success, 0 on failure
int mh_set_non_realtime(MH_Plugin* p, int non_realtime);

// Get plugin metadata without full instantiation
// Useful for validation, inventory, state file compatibility checks
// Returns 1 on success, 0 on failure (with error message in err_buf)
int mh_probe(const char* plugin_path,
             MH_PluginDesc* out_desc,
             char* err_buf,
             size_t err_buf_size);

// Parameter value text conversion
// Convert normalized value (0-1) to display string (e.g., "2500 Hz", "-6.0 dB")
// Returns 1 on success, 0 on failure
int mh_param_to_text(MH_Plugin* p, int index, float value, char* buf, size_t buf_size);

// Convert display string to normalized value (0-1)
// Returns 1 on success, 0 on failure (e.g., invalid text format)
// Note: Not all plugins implement text-to-value conversion
int mh_param_from_text(MH_Plugin* p, int index, const char* text, float* out_value);

// Factory preset (program) access
// Returns number of factory presets, or 0 if none
int mh_get_num_programs(MH_Plugin* p);

// Get name of factory preset at index
// Returns 1 on success, 0 on failure
int mh_get_program_name(MH_Plugin* p, int index, char* buf, size_t buf_size);

// Get currently selected program index
// Returns -1 if no program selected or on error
int mh_get_program(MH_Plugin* p);

// Select a factory preset by index
// Returns 1 on success, 0 on failure
int mh_set_program(MH_Plugin* p, int index);

// Bus layout query
// Get number of input or output buses
// is_input: 1 for input buses, 0 for output buses
int mh_get_num_buses(MH_Plugin* p, int is_input);

// Get information about a specific bus
// Returns 1 on success, 0 on failure
int mh_get_bus_info(MH_Plugin* p, int is_input, int bus_index, MH_BusInfo* out_info);

// Extended open with sidechain support
// sidechain_in_ch: number of sidechain input channels (0 to disable)
// returns NULL on failure
MH_Plugin* mh_open_ex(const char* plugin_path,
                      double sample_rate,
                      int max_block_size,
                      int main_in_ch,
                      int main_out_ch,
                      int sidechain_in_ch,
                      char* err_buf,
                      size_t err_buf_size);

// Process with sidechain input
// main_in: main input channels [main_in_ch][nframes]
// main_out: main output channels [main_out_ch][nframes]
// sidechain_in: sidechain input channels [sidechain_ch][nframes] (can be NULL if no sidechain)
// Returns 1 on success, 0 on failure
int mh_process_sidechain(MH_Plugin* p,
                         const float* const* main_in,
                         float* const* main_out,
                         const float* const* sidechain_in,
                         int nframes);

// Get number of sidechain input channels configured for this plugin
// Returns 0 if no sidechain or plugin opened with mh_open() instead of mh_open_ex()
int mh_get_sidechain_channels(MH_Plugin* p);

// Change sample rate without reloading the plugin
// Preserves parameter state across the change
// Returns 1 on success, 0 on failure
int mh_set_sample_rate(MH_Plugin* p, double new_sample_rate);

// Get current sample rate
double mh_get_sample_rate(MH_Plugin* p);

// Plugin directory scanning callback
// Called for each valid plugin found in the directory
// desc: plugin metadata (includes path field with full path to plugin)
// user_data: user-provided context pointer
typedef void (*MH_ScanCallback)(const MH_PluginDesc* desc, void* user_data);

// Scan a directory for plugins
// Recursively searches for .vst3 and .component (AU) files
// Calls callback for each valid plugin found (invalid plugins are silently skipped)
// Returns number of plugins found, or -1 on error (e.g., directory doesn't exist)
int mh_scan_directory(const char* directory_path,
                      MH_ScanCallback callback,
                      void* user_data);

// Double precision processing
// Process audio using 64-bit floating point samples
// Returns 1 on success, 0 on failure
// Note: Internally converts to/from float if plugin doesn't support double precision
int mh_process_double(MH_Plugin* p,
                      const double* const* inputs,
                      double* const* outputs,
                      int nframes);

// Check if plugin supports double precision processing natively
// Returns 1 if plugin supports double precision, 0 otherwise
int mh_supports_double(MH_Plugin* p);

// Async plugin loading callback
// Called when plugin loading completes (on success or failure)
// plugin: the loaded plugin (NULL on failure)
// error: error message (NULL on success, non-NULL on failure)
// user_data: user-provided context pointer
typedef void (*MH_LoadCallback)(MH_Plugin* plugin, const char* error, void* user_data);

// Asynchronously load a plugin in a background thread
// Callback is invoked from the background thread when loading completes
// Returns 1 if async load started successfully, 0 if failed to start
// Note: On success, caller must NOT use the plugin until callback is invoked
int mh_open_async(const char* plugin_path,
                  double sample_rate,
                  int max_block_size,
                  int requested_in_ch,
                  int requested_out_ch,
                  MH_LoadCallback callback,
                  void* user_data);

#ifdef __cplusplus
}
#endif

