// minihost.cpp
#include "minihost.h"

#include <juce_core/juce_core.h>
#include <juce_audio_basics/juce_audio_basics.h>
#include <juce_audio_processors/juce_audio_processors.h>
#include <mutex>
#include <thread>

using namespace juce;

class MH_PlayHead : public AudioPlayHead
{
public:
    bool hasTransport = false;
    double bpm = 120.0;
    int timeSigNum = 4;
    int timeSigDenom = 4;
    int64_t positionSamples = 0;
    double positionBeats = 0.0;
    bool isPlaying = false;
    bool isRecording = false;
    bool isLooping = false;
    int64_t loopStartSamples = 0;
    int64_t loopEndSamples = 0;
    double sampleRate = 44100.0;

    Optional<PositionInfo> getPosition() const override
    {
        if (!hasTransport)
            return nullopt;

        PositionInfo info;
        info.setBpm(bpm);
        info.setTimeSignature(TimeSignature{timeSigNum, timeSigDenom});
        info.setTimeInSamples(positionSamples);
        info.setTimeInSeconds(static_cast<double>(positionSamples) / sampleRate);
        info.setPpqPosition(positionBeats);
        info.setIsPlaying(isPlaying);
        info.setIsRecording(isRecording);
        info.setIsLooping(isLooping);
        if (isLooping)
        {
            info.setLoopPoints(LoopPoints{
                static_cast<double>(loopStartSamples) / sampleRate * (bpm / 60.0),
                static_cast<double>(loopEndSamples) / sampleRate * (bpm / 60.0)
            });
        }
        return info;
    }
};

struct MH_Plugin
{
    AudioPluginFormatManager fm;
    std::unique_ptr<AudioPluginInstance> inst;
    MH_PlayHead playHead;

    double sampleRate = 0.0;
    int maxBlockSize = 0;
    int inCh = 0;
    int outCh = 0;
    int sidechainCh = 0;  // sidechain input channels (0 if none)

    AudioBuffer<float> inBuf;
    AudioBuffer<float> outBuf;
    AudioBuffer<float> sidechainBuf;  // sidechain input buffer
    MidiBuffer midi;

    // Mutex for thread-safe access to plugin state from non-audio threads
    // Note: mh_process* functions do NOT lock (audio thread must not block)
    // Use this for param/state access from UI or control threads
    std::mutex stateMutex;

    MH_Plugin()
    {
        fm.addFormat(new VST3PluginFormat());
       #if JUCE_MAC
        fm.addFormat(new AudioUnitPluginFormat());
       #endif
    }
};

static void setErr(char* buf, size_t n, const String& msg)
{
    if (buf == nullptr || n == 0) return;
    auto s = msg.toRawUTF8();
    std::snprintf(buf, n, "%s", s ? s : "Unknown error");
}

static bool findFirstTypeForFile(AudioPluginFormatManager& fm,
                                 const File& pluginFile,
                                 PluginDescription& outDesc,
                                 String& err)
{
    KnownPluginList tempList;
    bool foundAny = false;

    for (int i = 0; i < fm.getNumFormats(); ++i)
    {
        auto* format = fm.getFormat(i);
        OwnedArray<PluginDescription> types;
        format->findAllTypesForFile(types, pluginFile.getFullPathName());

        if (types.size() > 0)
        {
            outDesc = *types[0]; // pick first
            foundAny = true;
            break;
        }
    }

    if (! foundAny)
        err = "No compatible plugin types found for file: " + pluginFile.getFullPathName();

    return foundAny;
}

static void tryConfigureBuses(AudioPluginInstance& inst, int reqIn, int reqOut)
{
    // Minimal, best-effort bus/channel config.
    // Many plugins will accept this; some need more complex routing.
    inst.enableAllBuses();

    // Main buses (bus 0)
    if (inst.getBusCount(true) > 0)
    {
        auto inLayout = inst.getChannelLayoutOfBus(true, 0);
        if (reqIn > 0)
            inst.setChannelLayoutOfBus(true, 0, AudioChannelSet::canonicalChannelSet(reqIn));
    }

    if (inst.getBusCount(false) > 0)
    {
        auto outLayout = inst.getChannelLayoutOfBus(false, 0);
        if (reqOut > 0)
            inst.setChannelLayoutOfBus(false, 0, AudioChannelSet::canonicalChannelSet(reqOut));
    }

    // Apply combined layout if possible
    auto layout = inst.getBusesLayout();
    inst.setBusesLayout(layout);
}

static void tryConfigureBusesEx(AudioPluginInstance& inst, int reqIn, int reqOut, int reqSidechain)
{
    // Extended bus configuration with sidechain support
    inst.enableAllBuses();

    // Main buses (bus 0)
    if (inst.getBusCount(true) > 0)
    {
        if (reqIn > 0)
            inst.setChannelLayoutOfBus(true, 0, AudioChannelSet::canonicalChannelSet(reqIn));
    }

    if (inst.getBusCount(false) > 0)
    {
        if (reqOut > 0)
            inst.setChannelLayoutOfBus(false, 0, AudioChannelSet::canonicalChannelSet(reqOut));
    }

    // Sidechain bus (typically bus 1 for inputs)
    if (reqSidechain > 0 && inst.getBusCount(true) > 1)
    {
        auto* bus = inst.getBus(true, 1);
        if (bus != nullptr)
        {
            bus->enable(true);
            inst.setChannelLayoutOfBus(true, 1, AudioChannelSet::canonicalChannelSet(reqSidechain));
        }
    }

    // Apply combined layout
    auto layout = inst.getBusesLayout();
    inst.setBusesLayout(layout);
}

extern "C" MH_Plugin* mh_open(const char* plugin_path,
                              double sample_rate,
                              int max_block_size,
                              int requested_in_ch,
                              int requested_out_ch,
                              char* err_buf,
                              size_t err_buf_size)
{
    if (plugin_path == nullptr || plugin_path[0] == '\0')
    {
        setErr(err_buf, err_buf_size, "plugin_path is empty");
        return nullptr;
    }

    std::unique_ptr<MH_Plugin> p(new MH_Plugin());
    p->sampleRate = sample_rate;
    p->maxBlockSize = max_block_size;

    File f(String::fromUTF8(plugin_path));
    if (! f.exists())
    {
        setErr(err_buf, err_buf_size, "Plugin file does not exist: " + f.getFullPathName());
        return nullptr;
    }

    PluginDescription desc;
    String err;
    if (! findFirstTypeForFile(p->fm, f, desc, err))
    {
        setErr(err_buf, err_buf_size, err);
        return nullptr;
    }

    String createErr;
    std::unique_ptr<AudioPluginInstance> inst(
        p->fm.createPluginInstance(desc, sample_rate, max_block_size, createErr)
    );

    if (! inst)
    {
        setErr(err_buf, err_buf_size, "createPluginInstance failed: " + createErr);
        return nullptr;
    }

    // Best-effort channel/bus layout
    tryConfigureBuses(*inst, requested_in_ch, requested_out_ch);

    p->inCh  = jmax(1, requested_in_ch  > 0 ? requested_in_ch  : inst->getTotalNumInputChannels());
    p->outCh = jmax(1, requested_out_ch > 0 ? requested_out_ch : inst->getTotalNumOutputChannels());

    inst->setRateAndBufferSizeDetails(sample_rate, max_block_size);
    inst->prepareToPlay(sample_rate, max_block_size);

    p->inBuf.setSize(p->inCh, max_block_size, false, false, true);
    p->outBuf.setSize(p->outCh, max_block_size, false, false, true);

    // Set up playhead for transport info
    p->playHead.sampleRate = sample_rate;
    inst->setPlayHead(&p->playHead);

    p->inst = std::move(inst);
    return p.release();
}

extern "C" void mh_close(MH_Plugin* p)
{
    if (! p) return;
    if (p->inst) p->inst->releaseResources();
    delete p;
}

extern "C" int mh_get_info(MH_Plugin* p, MH_Info* out_info)
{
    if (!p || !out_info || !p->inst) return 0;

    out_info->num_params      = (int) p->inst->getParameters().size();
    out_info->num_input_ch    = p->inst->getTotalNumInputChannels();
    out_info->num_output_ch   = p->inst->getTotalNumOutputChannels();
    out_info->latency_samples = p->inst->getLatencySamples();
    return 1;
}

extern "C" int mh_process_midi_io(MH_Plugin* p,
                                  const float* const* inputs,
                                  float* const* outputs,
                                  int nframes,
                                  const MH_MidiEvent* midi_in,
                                  int num_midi_in,
                                  MH_MidiEvent* midi_out,
                                  int midi_out_capacity,
                                  int* num_midi_out)
{
    if (!p || !p->inst) return 0;
    if (nframes < 0 || nframes > p->maxBlockSize) return 0;

    // Wrap caller buffers (no copy). If inputs/outputs are null, use internal buffers.
    // Input
    if (inputs)
    {
        // setDataToReferTo expects non-const; we only read, so cast is acceptable here.
        p->inBuf.setDataToReferTo(const_cast<float**>(inputs), p->inCh, nframes);
    }
    else
    {
        p->inBuf.clear(0, nframes);
    }

    // Output
    if (outputs)
    {
        p->outBuf.setDataToReferTo(outputs, p->outCh, nframes);
    }
    else
    {
        p->outBuf.clear(0, nframes);
    }

    // Many plugins do in-place; we pass output buffer to processBlock.
    // If you want "separate in/out", copy input -> output first.
    if (inputs && outputs)
    {
        // Copy min(inCh,outCh)
        auto minCh = jmin(p->inCh, p->outCh);
        for (int ch = 0; ch < minCh; ++ch)
            std::memcpy(outputs[ch], inputs[ch], sizeof(float) * (size_t)nframes);

        // If more outs than ins, clear extra outs
        for (int ch = minCh; ch < p->outCh; ++ch)
            std::memset(outputs[ch], 0, sizeof(float) * (size_t)nframes);
    }

    // Build MIDI input buffer from events
    p->midi.clear();
    if (midi_in && num_midi_in > 0)
    {
        for (int i = 0; i < num_midi_in; ++i)
        {
            const auto& ev = midi_in[i];
            int samplePos = jlimit(0, nframes - 1, ev.sample_offset);
            p->midi.addEvent(MidiMessage(ev.status, ev.data1, ev.data2), samplePos);
        }
    }

    p->inst->processBlock(p->outBuf, p->midi);

    // Extract MIDI output events
    if (num_midi_out)
        *num_midi_out = 0;

    if (midi_out && midi_out_capacity > 0)
    {
        int outIdx = 0;
        for (const auto metadata : p->midi)
        {
            if (outIdx >= midi_out_capacity)
                break;

            auto msg = metadata.getMessage();
            if (msg.getRawDataSize() >= 1)
            {
                const auto* data = msg.getRawData();
                midi_out[outIdx].sample_offset = metadata.samplePosition;
                midi_out[outIdx].status = data[0];
                midi_out[outIdx].data1 = msg.getRawDataSize() > 1 ? data[1] : 0;
                midi_out[outIdx].data2 = msg.getRawDataSize() > 2 ? data[2] : 0;
                ++outIdx;
            }
        }
        if (num_midi_out)
            *num_midi_out = outIdx;
    }

    return 1;
}

extern "C" int mh_process_midi(MH_Plugin* p,
                               const float* const* inputs,
                               float* const* outputs,
                               int nframes,
                               const MH_MidiEvent* midi_events,
                               int num_midi_events)
{
    return mh_process_midi_io(p, inputs, outputs, nframes,
                              midi_events, num_midi_events,
                              nullptr, 0, nullptr);
}

extern "C" int mh_process(MH_Plugin* p,
                          const float* const* inputs,
                          float* const* outputs,
                          int nframes)
{
    return mh_process_midi_io(p, inputs, outputs, nframes,
                              nullptr, 0, nullptr, 0, nullptr);
}

extern "C" int mh_get_num_params(MH_Plugin* p)
{
    if (!p || !p->inst) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);
    return (int) p->inst->getParameters().size();
}

extern "C" float mh_get_param(MH_Plugin* p, int index)
{
    if (!p || !p->inst) return 0.0f;
    std::lock_guard<std::mutex> lock(p->stateMutex);
    auto& params = p->inst->getParameters();
    if (index < 0 || index >= params.size()) return 0.0f;
    return params.getUnchecked(index)->getValue();
}

extern "C" int mh_set_param(MH_Plugin* p, int index, float normalized_0_1)
{
    if (!p || !p->inst) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);
    auto& params = p->inst->getParameters();
    if (index < 0 || index >= params.size()) return 0;

    normalized_0_1 = jlimit(0.0f, 1.0f, normalized_0_1);
    params.getUnchecked(index)->setValueNotifyingHost(normalized_0_1);
    return 1;
}

extern "C" int mh_get_param_info(MH_Plugin* p, int index, MH_ParamInfo* out_info)
{
    if (!p || !p->inst || !out_info) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);
    auto& params = p->inst->getParameters();
    if (index < 0 || index >= params.size()) return 0;

    auto* param = params.getUnchecked(index);

    // Name
    auto name = param->getName(MH_PARAM_NAME_LEN - 1);
    std::snprintf(out_info->name, MH_PARAM_NAME_LEN, "%s", name.toRawUTF8());

    // Label (unit)
    auto label = param->getLabel();
    std::snprintf(out_info->label, MH_PARAM_NAME_LEN, "%s", label.toRawUTF8());

    // Current value as display string
    auto valueStr = param->getCurrentValueAsText();
    std::snprintf(out_info->current_value_str, MH_PARAM_NAME_LEN, "%s", valueStr.toRawUTF8());

    // Normalized range (always 0-1 for JUCE parameters)
    out_info->min_value = 0.0f;
    out_info->max_value = 1.0f;
    out_info->default_value = param->getDefaultValue();

    // Discrete/continuous info
    out_info->num_steps = param->isDiscrete() ? param->getNumSteps() : 0;
    out_info->is_automatable = param->isAutomatable() ? 1 : 0;
    out_info->is_boolean = param->isBoolean() ? 1 : 0;

    return 1;
}

extern "C" int mh_get_state_size(MH_Plugin* p)
{
    if (!p || !p->inst) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);

    MemoryBlock mb;
    p->inst->getStateInformation(mb);
    return (int) mb.getSize();
}

extern "C" int mh_get_state(MH_Plugin* p, void* buffer, int buffer_size)
{
    if (!p || !p->inst || !buffer || buffer_size <= 0) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);

    MemoryBlock mb;
    p->inst->getStateInformation(mb);

    if ((int) mb.getSize() > buffer_size)
        return 0;

    std::memcpy(buffer, mb.getData(), mb.getSize());
    return 1;
}

extern "C" int mh_set_state(MH_Plugin* p, const void* data, int data_size)
{
    if (!p || !p->inst || !data || data_size <= 0) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);

    p->inst->setStateInformation(data, data_size);
    return 1;
}

extern "C" int mh_set_transport(MH_Plugin* p, const MH_TransportInfo* transport)
{
    if (!p) return 0;

    if (!transport)
    {
        p->playHead.hasTransport = false;
        return 1;
    }

    p->playHead.hasTransport = true;
    p->playHead.bpm = transport->bpm;
    p->playHead.timeSigNum = transport->time_sig_numerator;
    p->playHead.timeSigDenom = transport->time_sig_denominator;
    p->playHead.positionSamples = transport->position_samples;
    p->playHead.positionBeats = transport->position_beats;
    p->playHead.isPlaying = transport->is_playing != 0;
    p->playHead.isRecording = transport->is_recording != 0;
    p->playHead.isLooping = transport->is_looping != 0;
    p->playHead.loopStartSamples = transport->loop_start_samples;
    p->playHead.loopEndSamples = transport->loop_end_samples;

    return 1;
}

extern "C" double mh_get_tail_seconds(MH_Plugin* p)
{
    if (!p || !p->inst) return 0.0;
    std::lock_guard<std::mutex> lock(p->stateMutex);
    return p->inst->getTailLengthSeconds();
}

extern "C" int mh_get_bypass(MH_Plugin* p)
{
    if (!p || !p->inst) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);

    // Check if plugin has a bypass parameter
    auto* bypassParam = p->inst->getBypassParameter();
    if (bypassParam)
        return bypassParam->getValue() > 0.5f ? 1 : 0;

    return 0;
}

extern "C" int mh_set_bypass(MH_Plugin* p, int bypass)
{
    if (!p || !p->inst) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);

    auto* bypassParam = p->inst->getBypassParameter();
    if (bypassParam)
    {
        bypassParam->setValueNotifyingHost(bypass ? 1.0f : 0.0f);
        return 1;
    }

    // Plugin doesn't support bypass parameter - could implement manual bypass here
    // by skipping processBlock, but that's host-level behavior
    return 0;
}

extern "C" int mh_get_latency_samples(MH_Plugin* p)
{
    if (!p || !p->inst) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);
    return p->inst->getLatencySamples();
}

extern "C" int mh_process_auto(MH_Plugin* p,
                               const float* const* inputs,
                               float* const* outputs,
                               int nframes,
                               const MH_MidiEvent* midi_in,
                               int num_midi_in,
                               MH_MidiEvent* midi_out,
                               int midi_out_capacity,
                               int* num_midi_out,
                               const MH_ParamChange* param_changes,
                               int num_param_changes)
{
    if (!p || !p->inst) return 0;
    if (nframes < 0 || nframes > p->maxBlockSize) return 0;

    // Initialize MIDI output count
    if (num_midi_out)
        *num_midi_out = 0;
    int midi_out_idx = 0;

    // If no param changes, just use regular processing
    if (!param_changes || num_param_changes <= 0)
    {
        return mh_process_midi_io(p, inputs, outputs, nframes,
                                  midi_in, num_midi_in,
                                  midi_out, midi_out_capacity, num_midi_out);
    }

    // Process in chunks, applying parameter changes at the right sample positions
    int current_sample = 0;
    int midi_idx = 0;
    int param_idx = 0;

    while (current_sample < nframes)
    {
        // Find next chunk boundary (next param change or end of buffer)
        int chunk_end = nframes;
        if (param_idx < num_param_changes)
        {
            int next_change = param_changes[param_idx].sample_offset;
            if (next_change > current_sample && next_change < chunk_end)
                chunk_end = next_change;
        }

        // Apply any parameter changes at current_sample
        while (param_idx < num_param_changes &&
               param_changes[param_idx].sample_offset <= current_sample)
        {
            const auto& pc = param_changes[param_idx];
            auto& params = p->inst->getParameters();
            if (pc.param_index >= 0 && pc.param_index < params.size())
            {
                float val = jlimit(0.0f, 1.0f, pc.value);
                params.getUnchecked(pc.param_index)->setValueNotifyingHost(val);
            }
            ++param_idx;
        }

        int chunk_size = chunk_end - current_sample;
        if (chunk_size <= 0)
            break;

        // Prepare input/output pointers for this chunk
        const float* chunk_inputs[64];
        float* chunk_outputs[64];
        for (int ch = 0; ch < p->inCh && ch < 64; ++ch)
            chunk_inputs[ch] = inputs ? inputs[ch] + current_sample : nullptr;
        for (int ch = 0; ch < p->outCh && ch < 64; ++ch)
            chunk_outputs[ch] = outputs ? outputs[ch] + current_sample : nullptr;

        // Collect MIDI events for this chunk
        p->midi.clear();
        while (midi_idx < num_midi_in)
        {
            const auto& ev = midi_in[midi_idx];
            if (ev.sample_offset >= chunk_end)
                break;
            if (ev.sample_offset >= current_sample)
            {
                int local_offset = ev.sample_offset - current_sample;
                p->midi.addEvent(MidiMessage(ev.status, ev.data1, ev.data2), local_offset);
            }
            ++midi_idx;
        }

        // Set up buffers for this chunk
        if (inputs)
            p->inBuf.setDataToReferTo(const_cast<float**>(chunk_inputs), p->inCh, chunk_size);
        else
            p->inBuf.clear(0, chunk_size);

        if (outputs)
            p->outBuf.setDataToReferTo(chunk_outputs, p->outCh, chunk_size);
        else
            p->outBuf.clear(0, chunk_size);

        // Copy input to output for in-place processing
        if (inputs && outputs)
        {
            auto minCh = jmin(p->inCh, p->outCh);
            for (int ch = 0; ch < minCh; ++ch)
                std::memcpy(chunk_outputs[ch], chunk_inputs[ch], sizeof(float) * chunk_size);
            for (int ch = minCh; ch < p->outCh; ++ch)
                std::memset(chunk_outputs[ch], 0, sizeof(float) * chunk_size);
        }

        // Process this chunk
        p->inst->processBlock(p->outBuf, p->midi);

        // Collect MIDI output from this chunk
        if (midi_out && midi_out_capacity > 0)
        {
            for (const auto metadata : p->midi)
            {
                if (midi_out_idx >= midi_out_capacity)
                    break;
                auto msg = metadata.getMessage();
                if (msg.getRawDataSize() >= 1)
                {
                    const auto* data = msg.getRawData();
                    midi_out[midi_out_idx].sample_offset = metadata.samplePosition + current_sample;
                    midi_out[midi_out_idx].status = data[0];
                    midi_out[midi_out_idx].data1 = msg.getRawDataSize() > 1 ? data[1] : 0;
                    midi_out[midi_out_idx].data2 = msg.getRawDataSize() > 2 ? data[2] : 0;
                    ++midi_out_idx;
                }
            }
        }

        current_sample = chunk_end;
    }

    if (num_midi_out)
        *num_midi_out = midi_out_idx;

    return 1;
}

extern "C" int mh_reset(MH_Plugin* p)
{
    if (!p || !p->inst) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);
    p->inst->reset();
    return 1;
}

extern "C" int mh_set_non_realtime(MH_Plugin* p, int non_realtime)
{
    if (!p || !p->inst) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);
    p->inst->setNonRealtime(non_realtime != 0);
    return 1;
}

extern "C" int mh_probe(const char* plugin_path,
                        MH_PluginDesc* out_desc,
                        char* err_buf,
                        size_t err_buf_size)
{
    if (!plugin_path || plugin_path[0] == '\0')
    {
        setErr(err_buf, err_buf_size, "plugin_path is empty");
        return 0;
    }

    if (!out_desc)
    {
        setErr(err_buf, err_buf_size, "out_desc is null");
        return 0;
    }

    // Zero out the descriptor
    std::memset(out_desc, 0, sizeof(MH_PluginDesc));

    File f(String::fromUTF8(plugin_path));
    if (!f.exists())
    {
        setErr(err_buf, err_buf_size, "Plugin file does not exist: " + f.getFullPathName());
        return 0;
    }

    // Create a temporary format manager to scan the plugin
    AudioPluginFormatManager fm;
    fm.addFormat(new VST3PluginFormat());
   #if JUCE_MAC
    fm.addFormat(new AudioUnitPluginFormat());
   #endif

    // Find plugin description without instantiating
    PluginDescription desc;
    bool found = false;
    String formatName;

    for (int i = 0; i < fm.getNumFormats(); ++i)
    {
        auto* format = fm.getFormat(i);
        OwnedArray<PluginDescription> types;
        format->findAllTypesForFile(types, f.getFullPathName());

        if (types.size() > 0)
        {
            desc = *types[0];
            formatName = format->getName();
            found = true;
            break;
        }
    }

    if (!found)
    {
        setErr(err_buf, err_buf_size, "No compatible plugin types found for file: " + f.getFullPathName());
        return 0;
    }

    // Fill in the descriptor
    std::snprintf(out_desc->name, sizeof(out_desc->name), "%s", desc.name.toRawUTF8());
    std::snprintf(out_desc->vendor, sizeof(out_desc->vendor), "%s", desc.manufacturerName.toRawUTF8());
    std::snprintf(out_desc->version, sizeof(out_desc->version), "%s", desc.version.toRawUTF8());
    std::snprintf(out_desc->format, sizeof(out_desc->format), "%s", formatName.toRawUTF8());

    // uniqueId is an int, convert to hex string for portability
    std::snprintf(out_desc->unique_id, sizeof(out_desc->unique_id), "%08X", desc.uniqueId);

    // isInstrument indicates the plugin accepts MIDI (synthesizers, samplers)
    // Note: PluginDescription doesn't expose producesMidiOutput directly
    out_desc->accepts_midi = desc.isInstrument ? 1 : 0;
    out_desc->produces_midi = 0;  // Cannot determine from description alone
    out_desc->num_inputs = desc.numInputChannels;
    out_desc->num_outputs = desc.numOutputChannels;

    return 1;
}

extern "C" int mh_param_to_text(MH_Plugin* p, int index, float value, char* buf, size_t buf_size)
{
    if (!p || !p->inst || !buf || buf_size == 0) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);

    auto& params = p->inst->getParameters();
    if (index < 0 || index >= params.size()) return 0;

    auto* param = params.getUnchecked(index);
    value = jlimit(0.0f, 1.0f, value);

    // getText returns the display string for a given normalized value
    // Second parameter is maximum string length hint
    String text = param->getText(value, static_cast<int>(buf_size) - 1);
    std::snprintf(buf, buf_size, "%s", text.toRawUTF8());

    return 1;
}

extern "C" int mh_param_from_text(MH_Plugin* p, int index, const char* text, float* out_value)
{
    if (!p || !p->inst || !text || !out_value) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);

    auto& params = p->inst->getParameters();
    if (index < 0 || index >= params.size()) return 0;

    auto* param = params.getUnchecked(index);

    // getValueForText converts display string to normalized value
    // Note: Many plugins don't implement this properly and return 0
    float value = param->getValueForText(String::fromUTF8(text));

    // Clamp to valid range
    *out_value = jlimit(0.0f, 1.0f, value);

    return 1;
}

extern "C" int mh_get_num_programs(MH_Plugin* p)
{
    if (!p || !p->inst) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);
    return p->inst->getNumPrograms();
}

extern "C" int mh_get_program_name(MH_Plugin* p, int index, char* buf, size_t buf_size)
{
    if (!p || !p->inst || !buf || buf_size == 0) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);

    int numPrograms = p->inst->getNumPrograms();
    if (index < 0 || index >= numPrograms) return 0;

    String name = p->inst->getProgramName(index);
    std::snprintf(buf, buf_size, "%s", name.toRawUTF8());

    return 1;
}

extern "C" int mh_get_program(MH_Plugin* p)
{
    if (!p || !p->inst) return -1;
    std::lock_guard<std::mutex> lock(p->stateMutex);
    return p->inst->getCurrentProgram();
}

extern "C" int mh_set_program(MH_Plugin* p, int index)
{
    if (!p || !p->inst) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);

    int numPrograms = p->inst->getNumPrograms();
    if (index < 0 || index >= numPrograms) return 0;

    p->inst->setCurrentProgram(index);
    return 1;
}

extern "C" int mh_get_num_buses(MH_Plugin* p, int is_input)
{
    if (!p || !p->inst) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);
    return p->inst->getBusCount(is_input != 0);
}

extern "C" int mh_get_bus_info(MH_Plugin* p, int is_input, int bus_index, MH_BusInfo* out_info)
{
    if (!p || !p->inst || !out_info) return 0;
    std::lock_guard<std::mutex> lock(p->stateMutex);

    bool input = (is_input != 0);
    int numBuses = p->inst->getBusCount(input);
    if (bus_index < 0 || bus_index >= numBuses) return 0;

    auto* bus = p->inst->getBus(input, bus_index);
    if (!bus) return 0;

    // Clear output struct
    std::memset(out_info, 0, sizeof(MH_BusInfo));

    // Bus name
    String name = bus->getName();
    std::snprintf(out_info->name, sizeof(out_info->name), "%s", name.toRawUTF8());

    // Channel count
    out_info->num_channels = bus->getNumberOfChannels();

    // Is main bus (bus 0 is typically the main bus)
    out_info->is_main = (bus_index == 0) ? 1 : 0;

    // Is enabled
    out_info->is_enabled = bus->isEnabled() ? 1 : 0;

    return 1;
}

extern "C" MH_Plugin* mh_open_ex(const char* plugin_path,
                                 double sample_rate,
                                 int max_block_size,
                                 int main_in_ch,
                                 int main_out_ch,
                                 int sidechain_in_ch,
                                 char* err_buf,
                                 size_t err_buf_size)
{
    if (plugin_path == nullptr || plugin_path[0] == '\0')
    {
        setErr(err_buf, err_buf_size, "plugin_path is empty");
        return nullptr;
    }

    std::unique_ptr<MH_Plugin> p(new MH_Plugin());
    p->sampleRate = sample_rate;
    p->maxBlockSize = max_block_size;

    File f(String::fromUTF8(plugin_path));
    if (! f.exists())
    {
        setErr(err_buf, err_buf_size, "Plugin file does not exist: " + f.getFullPathName());
        return nullptr;
    }

    PluginDescription desc;
    String err;
    if (! findFirstTypeForFile(p->fm, f, desc, err))
    {
        setErr(err_buf, err_buf_size, err);
        return nullptr;
    }

    String createErr;
    std::unique_ptr<AudioPluginInstance> inst(
        p->fm.createPluginInstance(desc, sample_rate, max_block_size, createErr)
    );

    if (! inst)
    {
        setErr(err_buf, err_buf_size, "createPluginInstance failed: " + createErr);
        return nullptr;
    }

    // Extended bus/channel layout with sidechain
    tryConfigureBusesEx(*inst, main_in_ch, main_out_ch, sidechain_in_ch);

    p->inCh  = jmax(1, main_in_ch  > 0 ? main_in_ch  : inst->getTotalNumInputChannels());
    p->outCh = jmax(1, main_out_ch > 0 ? main_out_ch : inst->getTotalNumOutputChannels());

    // Determine actual sidechain channels
    p->sidechainCh = 0;
    if (sidechain_in_ch > 0 && inst->getBusCount(true) > 1)
    {
        auto* scBus = inst->getBus(true, 1);
        if (scBus && scBus->isEnabled())
        {
            p->sidechainCh = scBus->getNumberOfChannels();
        }
    }

    inst->setRateAndBufferSizeDetails(sample_rate, max_block_size);
    inst->prepareToPlay(sample_rate, max_block_size);

    p->inBuf.setSize(p->inCh, max_block_size, false, false, true);
    p->outBuf.setSize(p->outCh, max_block_size, false, false, true);
    if (p->sidechainCh > 0)
    {
        p->sidechainBuf.setSize(p->sidechainCh, max_block_size, false, false, true);
    }

    // Set up playhead for transport info
    p->playHead.sampleRate = sample_rate;
    inst->setPlayHead(&p->playHead);

    p->inst = std::move(inst);
    return p.release();
}

extern "C" int mh_process_sidechain(MH_Plugin* p,
                                    const float* const* main_in,
                                    float* const* main_out,
                                    const float* const* sidechain_in,
                                    int nframes)
{
    if (!p || !p->inst) return 0;
    if (nframes < 0 || nframes > p->maxBlockSize) return 0;

    // Total channels needed = max(main_in + sidechain, main_out)
    // JUCE processBlock uses a single buffer for in-place processing
    int totalInCh = p->inCh + p->sidechainCh;
    int totalCh = jmax(totalInCh, p->outCh);

    // Create a combined buffer large enough for all channels
    AudioBuffer<float> buffer(totalCh, nframes);

    // Copy main input to first channels
    if (main_in)
    {
        for (int ch = 0; ch < p->inCh; ++ch)
            buffer.copyFrom(ch, 0, main_in[ch], nframes);
    }
    else
    {
        for (int ch = 0; ch < p->inCh; ++ch)
            buffer.clear(ch, 0, nframes);
    }

    // Copy sidechain input to subsequent channels
    if (sidechain_in && p->sidechainCh > 0)
    {
        for (int ch = 0; ch < p->sidechainCh; ++ch)
            buffer.copyFrom(p->inCh + ch, 0, sidechain_in[ch], nframes);
    }
    else if (p->sidechainCh > 0)
    {
        for (int ch = p->inCh; ch < totalInCh; ++ch)
            buffer.clear(ch, 0, nframes);
    }

    // Clear any remaining output channels
    for (int ch = totalInCh; ch < totalCh; ++ch)
        buffer.clear(ch, 0, nframes);

    // Clear MIDI buffer
    p->midi.clear();

    // Process
    p->inst->processBlock(buffer, p->midi);

    // Copy output back to caller's buffer
    if (main_out)
    {
        for (int ch = 0; ch < p->outCh; ++ch)
            std::memcpy(main_out[ch], buffer.getReadPointer(ch), sizeof(float) * nframes);
    }

    return 1;
}

extern "C" int mh_get_sidechain_channels(MH_Plugin* p)
{
    if (!p) return 0;
    return p->sidechainCh;
}

extern "C" int mh_set_sample_rate(MH_Plugin* p, double new_sample_rate)
{
    if (!p || !p->inst) return 0;
    if (new_sample_rate <= 0.0) return 0;

    std::lock_guard<std::mutex> lock(p->stateMutex);

    // Save current state
    MemoryBlock stateData;
    p->inst->getStateInformation(stateData);

    // Release current resources
    p->inst->releaseResources();

    // Update sample rate
    p->sampleRate = new_sample_rate;
    p->playHead.sampleRate = new_sample_rate;

    // Re-prepare with new sample rate
    p->inst->setRateAndBufferSizeDetails(new_sample_rate, p->maxBlockSize);
    p->inst->prepareToPlay(new_sample_rate, p->maxBlockSize);

    // Restore state
    if (stateData.getSize() > 0)
    {
        p->inst->setStateInformation(stateData.getData(), static_cast<int>(stateData.getSize()));
    }

    return 1;
}

extern "C" double mh_get_sample_rate(MH_Plugin* p)
{
    if (!p) return 0.0;
    return p->sampleRate;
}

extern "C" int mh_scan_directory(const char* directory_path,
                                 MH_ScanCallback callback,
                                 void* user_data)
{
    if (!directory_path || directory_path[0] == '\0' || !callback)
        return -1;

    File dir(String::fromUTF8(directory_path));
    if (!dir.exists() || !dir.isDirectory())
        return -1;

    int count = 0;

    // Find all .vst3 and .component files recursively
    Array<File> pluginFiles;

    // VST3 plugins (.vst3 bundles)
    dir.findChildFiles(pluginFiles, File::findDirectories, true, "*.vst3");

    // AudioUnit plugins (.component bundles) - macOS only
   #if JUCE_MAC
    dir.findChildFiles(pluginFiles, File::findDirectories, true, "*.component");
   #endif

    // Process each plugin file
    for (const auto& pluginFile : pluginFiles)
    {
        MH_PluginDesc desc;
        char errBuf[256];

        // Try to probe the plugin
        if (mh_probe(pluginFile.getFullPathName().toRawUTF8(), &desc, errBuf, sizeof(errBuf)))
        {
            // Fill in the path field
            std::snprintf(desc.path, sizeof(desc.path), "%s",
                          pluginFile.getFullPathName().toRawUTF8());

            // Call the callback
            callback(&desc, user_data);
            ++count;
        }
        // Invalid plugins are silently skipped
    }

    return count;
}

extern "C" int mh_supports_double(MH_Plugin* p)
{
    if (!p || !p->inst) return 0;
    return p->inst->supportsDoublePrecisionProcessing() ? 1 : 0;
}

extern "C" int mh_process_double(MH_Plugin* p,
                                 const double* const* inputs,
                                 double* const* outputs,
                                 int nframes)
{
    if (!p || !p->inst) return 0;
    if (nframes < 0 || nframes > p->maxBlockSize) return 0;

    // Check if plugin supports native double precision
    if (p->inst->supportsDoublePrecisionProcessing())
    {
        // Use native double precision processing
        AudioBuffer<double> inBuf(p->inCh, nframes);
        AudioBuffer<double> outBuf(p->outCh, nframes);

        // Copy input to buffer
        if (inputs)
        {
            for (int ch = 0; ch < p->inCh; ++ch)
                std::memcpy(inBuf.getWritePointer(ch), inputs[ch], sizeof(double) * nframes);
        }
        else
        {
            inBuf.clear();
        }

        // Copy input to output for in-place processing
        if (inputs && outputs)
        {
            int minCh = jmin(p->inCh, p->outCh);
            for (int ch = 0; ch < minCh; ++ch)
                std::memcpy(outBuf.getWritePointer(ch), inputs[ch], sizeof(double) * nframes);
            for (int ch = minCh; ch < p->outCh; ++ch)
                outBuf.clear(ch, 0, nframes);
        }
        else if (outputs)
        {
            outBuf.clear();
        }

        // Process with double precision
        MidiBuffer midi;
        p->inst->processBlock(outBuf, midi);

        // Copy output back
        if (outputs)
        {
            for (int ch = 0; ch < p->outCh; ++ch)
                std::memcpy(outputs[ch], outBuf.getReadPointer(ch), sizeof(double) * nframes);
        }
    }
    else
    {
        // Convert to float, process, convert back
        AudioBuffer<float> inBuf(p->inCh, nframes);
        AudioBuffer<float> outBuf(p->outCh, nframes);

        // Convert double input to float
        if (inputs)
        {
            for (int ch = 0; ch < p->inCh; ++ch)
            {
                auto* dest = inBuf.getWritePointer(ch);
                for (int i = 0; i < nframes; ++i)
                    dest[i] = static_cast<float>(inputs[ch][i]);
            }
        }
        else
        {
            inBuf.clear();
        }

        // Copy input to output for in-place processing
        if (inputs && outputs)
        {
            int minCh = jmin(p->inCh, p->outCh);
            for (int ch = 0; ch < minCh; ++ch)
            {
                auto* dest = outBuf.getWritePointer(ch);
                for (int i = 0; i < nframes; ++i)
                    dest[i] = static_cast<float>(inputs[ch][i]);
            }
            for (int ch = minCh; ch < p->outCh; ++ch)
                outBuf.clear(ch, 0, nframes);
        }
        else if (outputs)
        {
            outBuf.clear();
        }

        // Process with float
        MidiBuffer midi;
        p->inst->processBlock(outBuf, midi);

        // Convert float output back to double
        if (outputs)
        {
            for (int ch = 0; ch < p->outCh; ++ch)
            {
                const auto* src = outBuf.getReadPointer(ch);
                for (int i = 0; i < nframes; ++i)
                    outputs[ch][i] = static_cast<double>(src[i]);
            }
        }
    }

    return 1;
}

extern "C" int mh_open_async(const char* plugin_path,
                             double sample_rate,
                             int max_block_size,
                             int requested_in_ch,
                             int requested_out_ch,
                             MH_LoadCallback callback,
                             void* user_data)
{
    if (!plugin_path || plugin_path[0] == '\0' || !callback)
        return 0;

    // Copy parameters for the thread
    std::string path(plugin_path);

    // Launch loading thread (detached - it manages its own lifetime)
    std::thread loadThread([=]() {
        char errBuf[1024] = {0};
        MH_Plugin* plugin = mh_open(path.c_str(), sample_rate, max_block_size,
                                     requested_in_ch, requested_out_ch,
                                     errBuf, sizeof(errBuf));

        if (plugin)
        {
            callback(plugin, nullptr, user_data);
        }
        else
        {
            callback(nullptr, errBuf, user_data);
        }
    });

    loadThread.detach();
    return 1;
}

