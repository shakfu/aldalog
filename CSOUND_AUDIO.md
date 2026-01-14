# Csound Audio Integration - Debug Notes

**Status**: FIXED - Audio working
**Date**: 2026-01-14

## Summary

Csound synthesis backend is now fully working. The issue was that `async.c` (the playback event dispatcher) was routing MIDI events only to TSF, completely ignoring the Csound backend.

### Root Cause

In `thirdparty/alda-midi/lib/src/async.c`, the `send_event()` function only checked for TSF:
```c
if (async_sys.ctx && async_sys.ctx->tsf_enabled && alda_tsf_is_enabled()) {
    // routes to TSF...
}
```

It never checked `csound_enabled` or routed to `alda_csound_send_note_on()`.

### Fix

Added Csound routing to `async.c:send_event()` with highest priority (before TSF check), mirroring the pattern in `midi_backend.c`.

## What Works

- Csound 6.18.1 compiles and links correctly
- CSD file loads without errors
- Csound initializes and starts properly
- Message suppression via `csoundCreateMessageBuffer()` works
- Editor opens and shows "ALDA CSD" status
- **Audio output works** when playing Alda code with Ctrl-P
- MIDI events are correctly routed to Csound instruments

## Architecture

### Current Design (after refactoring)

Csound backend now shares TSF's miniaudio audio device instead of creating its own:

1. `tsf_backend.c` contains `MINIAUDIO_IMPLEMENTATION` and owns the `ma_device`
2. `csound_backend.c` includes `tsf_backend.h` and calls `alda_tsf_enable()` to start audio
3. TSF's audio callback (`tsf_audio_callback`) checks `alda_csound_is_enabled()`:
   - If Csound is enabled, it calls `alda_csound_render()`
   - Otherwise, it renders TSF audio

### Key Files

- `thirdparty/alda-midi/lib/src/csound_backend.c` - Csound synthesis backend
- `thirdparty/alda-midi/lib/src/tsf_backend.c` - TSF backend with shared audio device
- `thirdparty/alda-midi/lib/include/alda/csound_backend.h` - Csound API
- `.aldalog/csound/default.csd` - Default Csound instruments

## Attempted Fixes

### 1. Separate miniaudio device for Csound
- Created own `ma_device` in csound_backend.c
- Device reported as initialized and started (state=2)
- **Result**: Audio callback never fired
- **Theory**: macOS CoreAudio may not support multiple miniaudio devices

### 2. Shared audio device with TSF
- Refactored to use TSF's miniaudio device
- TSF callback delegates to `alda_csound_render()` when Csound enabled
- Modified `alda_tsf_enable()` to not require soundfont when Csound is active
- **Result**: Still no audio (current state)

### 3. Csound initialization order
- Moved `csoundStart()` to immediately after `csoundCompileCsd()`
- Added `csoundReadScore()` to send infinite duration event
- Added `--daemon` flag to ignore score section during compilation
- **Result**: Fixed "csoundStart() has not been called" warning

### 4. Message suppression
- Added `csoundCreateMessageBuffer(csound, 0)` to capture messages
- Set `-m0` flag to suppress verbose output
- **Result**: Messages suppressed, but audio still not working

## Current Code State

### csound_backend.c enable function:
```c
int alda_csound_enable(void) {
    // ... initialization checks ...

    // Start Csound if not already started
    if (!g_cs.started) {
        csoundStart(g_cs.csound);
        g_cs.spout = csoundGetSpout(g_cs.csound);
        // ... get buffer pointers ...
        g_cs.started = 1;
    }

    g_cs.enabled = 1;

    // Use TSF's audio device for output
    if (alda_tsf_enable() != 0) {
        g_cs.enabled = 0;
        return -1;
    }
    return 0;
}
```

### tsf_backend.c audio callback:
```c
static void tsf_audio_callback(ma_device* device, void* output,
                                const void* input, ma_uint32 frame_count) {
    float* out = (float*)output;

    // Csound takes priority when enabled
    if (alda_csound_is_enabled()) {
        alda_csound_render(out, (int)frame_count);
        return;
    }

    // Otherwise use TSF
    // ...
}
```

## Next Steps to Investigate

1. **Verify TSF audio works independently**
   - Test with `-sf soundfont.sf2` to confirm TSF audio path works
   - If TSF works, the shared audio device approach is valid

2. **Add debug output to TSF callback**
   - Confirm the callback IS being called
   - Confirm `alda_csound_is_enabled()` returns true
   - Confirm `alda_csound_render()` is being called

3. **Check Csound render function**
   - Add debug output to `alda_csound_render()`
   - Verify `csoundPerformKsmps()` is returning samples
   - Check if `g_cs.spout` contains non-zero values

4. **Verify note events are being sent**
   - Add debug to `alda_csound_send_note_on()`
   - Confirm MIDI events reach Csound via `csoundInputMessage()`

5. **Test Csound instruments directly**
   - Create a simple test that plays a note without the editor
   - Isolate whether issue is in Csound or in the integration

## CSD File Notes

The default CSD file (`.aldalog/csound/default.csd`) was fixed for:
- `endop` vs `endin` (opcodes use `endop`, instruments use `endin`)
- FM synthesis instrument 2 (replaced `fmb3` with manual FM)
- Added `--daemon` flag to CsOptions

## Build Command

```bash
make csound  # or: cmake --build build --target alda_bin
```

## Test Command

```bash
./build/aldalog -cs .aldalog/csound/default.csd docs/examples/simple.alda
# Then press Ctrl-P to play
```

## Plugin Warning

The warning about missing plugin directory is harmless:
```
WARNING: Error opening plugin directory '/Users/sa/Library/Frameworks/CsoundLib64.framework/Versions/6.0/Resources/Opcodes64': No such file or directory
```
This is because we're using a statically linked Csound, not the framework version.
