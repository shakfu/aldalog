-- TR7 Module - Lua wrapper for the loki.tr7 C API
--
-- This module provides convenience functions and documentation for the
-- loki.tr7 subsystem which handles TR7 Scheme music programming
-- and MIDI playback via the built-in TinySoundFont synthesizer.
--
-- TR7 is a tiny R7RS-small Scheme interpreter with music extensions.
-- The underlying C API (loki.tr7.*) is automatically available when
-- the editor starts. This module adds higher-level helpers.
--
-- ==============================================================================
-- loki.tr7 C API Reference
-- ==============================================================================
--
-- Initialization:
--   loki.tr7.init()               Initialize the TR7 Scheme interpreter
--                                 Returns: true on success, nil + error on failure
--
--   loki.tr7.is_initialized()     Check if TR7 is ready
--                                 Returns: boolean
--
-- Evaluation:
--   loki.tr7.eval(code)           Evaluate Scheme code
--                                 code: Scheme expression string
--                                 Returns: result string on success, nil + error on failure
--
-- Playback:
--   loki.tr7.stop()               Stop all MIDI playback and send panic
--
-- ==============================================================================
-- TR7 Scheme Music Primitives
-- ==============================================================================
--
-- Note Playback:
--   (play-note pitch [vel] [dur])       Play a single MIDI note
--   (play-chord '(p1 p2 ...) [vel] [dur]) Play notes simultaneously
--   (play-seq '(p1 p2 ...) [vel] [dur])   Play notes in sequence
--   (note-on pitch [vel])               Send note-on message
--   (note-off pitch)                    Send note-off message
--
-- State Control:
--   (set-tempo bpm)                     Set tempo (20-400 BPM)
--   (set-octave n)                      Set default octave (0-9)
--   (set-velocity v)                    Set default velocity (0-127)
--   (set-channel ch)                    Set MIDI channel (0-15)
--   (tempo), (octave), (velocity), (channel)  Get current values
--
-- Note Conversion:
--   (note "c4")                         Convert note name to MIDI pitch
--   (note "c#4"), (note "bb3")          Sharps and flats
--
-- MIDI Control:
--   (midi-list)                         List available MIDI ports
--   (midi-open port)                    Open MIDI port by index
--   (midi-virtual name)                 Create virtual MIDI port
--   (midi-panic)                        Send all notes off
--   (program-change prog)               Change instrument (0-127)
--   (control-change cc val)             Send CC message
--
-- ==============================================================================

local M = {}

-- Detect execution mode
local MODE = nil
local function get_mode()
    if MODE then return MODE end
    MODE = loki.get_lines and "editor" or "repl"
    return MODE
end

-- Status message helper (mode-aware)
local function status(msg)
    if get_mode() == "editor" then
        loki.status(msg)
    else
        print(msg)
    end
end

-- ==============================================================================
-- Initialization Helpers
-- ==============================================================================

-- Initialize TR7 with options
function M.setup(opts)
    opts = opts or {}

    -- Initialize TR7 interpreter
    local ok, err = loki.tr7.init()
    if not ok then
        status("TR7 init failed: " .. (err or "unknown"))
        return false, err
    end

    -- Set initial tempo if specified
    if opts.tempo then
        M.eval(string.format("(set-tempo %d)", opts.tempo))
    end

    -- Open MIDI port if specified
    if opts.port_name then
        M.eval(string.format('(midi-virtual "%s")', opts.port_name))
    elseif opts.port_index then
        M.eval(string.format("(midi-open %d)", opts.port_index))
    end

    status("TR7 ready")
    return true
end

-- ==============================================================================
-- Evaluation Helpers
-- ==============================================================================

-- Evaluate Scheme code with error handling
function M.eval(code)
    if not loki.tr7.is_initialized() then
        status("TR7 not initialized")
        return nil, "not initialized"
    end

    if not code or code == "" then
        status("No code to evaluate")
        return nil, "empty code"
    end

    local result, err = loki.tr7.eval(code)
    if not result and err then
        status("Eval failed: " .. err)
        return nil, err
    end

    return result
end

-- Evaluate current buffer (editor mode)
function M.eval_file()
    if get_mode() ~= "editor" then
        print("tr7.eval_file() requires editor mode")
        return false
    end

    if not loki.tr7.is_initialized() then
        status("TR7 not initialized")
        return false
    end

    -- Get entire buffer content
    local lines = {}
    local num_lines = loki.get_lines()
    for i = 0, num_lines - 1 do
        local line = loki.get_line(i)
        if line then
            table.insert(lines, line)
        end
    end
    local code = table.concat(lines, "\n")

    if code == "" then
        status("Empty buffer")
        return false
    end

    local result, err = loki.tr7.eval(code)
    if not result and err then
        status("Eval failed: " .. err)
        return false
    end

    status("Evaluated")
    return true
end

-- Evaluate current line (editor mode)
function M.eval_line()
    if get_mode() ~= "editor" then
        print("tr7.eval_line() requires editor mode")
        return false
    end

    if not loki.tr7.is_initialized() then
        status("TR7 not initialized")
        return false
    end

    local row, _ = loki.get_cursor()
    local line = loki.get_line(row)

    if not line or line == "" then
        status("Empty line")
        return false
    end

    local result, err = loki.tr7.eval(line)
    if not result and err then
        status("Eval failed: " .. err)
        return false
    end

    status("Evaluated line")
    return true
end

-- Stop all playback
function M.stop()
    loki.tr7.stop()
    status("Stopped")
end

-- ==============================================================================
-- Convenience Functions
-- ==============================================================================

-- Play a note using Scheme
function M.play_note(pitch, velocity, duration)
    velocity = velocity or 80
    duration = duration or 500
    return M.eval(string.format("(play-note %d %d %d)", pitch, velocity, duration))
end

-- Play a chord using Scheme
function M.play_chord(pitches, velocity, duration)
    velocity = velocity or 80
    duration = duration or 1000
    local notes = table.concat(pitches, " ")
    return M.eval(string.format("(play-chord '(%s) %d %d)", notes, velocity, duration))
end

-- Play a sequence using Scheme
function M.play_seq(pitches, velocity, duration)
    velocity = velocity or 80
    duration = duration or 250
    local notes = table.concat(pitches, " ")
    return M.eval(string.format("(play-seq '(%s) %d %d)", notes, velocity, duration))
end

-- Set tempo
function M.set_tempo(bpm)
    return M.eval(string.format("(set-tempo %d)", bpm))
end

-- ==============================================================================
-- Status Helpers
-- ==============================================================================

-- Print full status info
function M.info()
    local initialized = loki.tr7.is_initialized()
    print("TR7 Status:")
    print("  Initialized: " .. (initialized and "yes" or "no"))

    if initialized then
        local tempo = M.eval("(tempo)")
        local vel = M.eval("(velocity)")
        local oct = M.eval("(octave)")
        local ch = M.eval("(channel)")

        if tempo then print("  Tempo:       " .. tempo .. " BPM") end
        if vel then print("  Velocity:    " .. vel) end
        if oct then print("  Octave:      " .. oct) end
        if ch then print("  Channel:     " .. ch) end
    end
end

-- List MIDI ports
function M.ports()
    if not loki.tr7.is_initialized() then
        print("TR7 not initialized")
        return
    end
    M.eval("(midi-list)")
end

-- ==============================================================================
-- Register with REPL help
-- ==============================================================================

if loki.repl and loki.repl.register then
    loki.repl.register("tr7.setup", "Initialize TR7: tr7.setup({tempo=120})")
    loki.repl.register("tr7.eval", "Evaluate Scheme: tr7.eval('(play-note 60 80 500)')")
    loki.repl.register("tr7.eval_file", "Evaluate current buffer (editor mode)")
    loki.repl.register("tr7.eval_line", "Evaluate current line (editor mode)")
    loki.repl.register("tr7.play_note", "Play note: tr7.play_note(60, 80, 500)")
    loki.repl.register("tr7.play_chord", "Play chord: tr7.play_chord({60,64,67})")
    loki.repl.register("tr7.stop", "Stop all playback")
    loki.repl.register("tr7.info", "Show TR7 status info")
end

return M
