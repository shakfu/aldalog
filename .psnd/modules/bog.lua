-- Bog Module - Lua wrapper for the loki.bog C API
--
-- This module provides convenience functions and documentation for the
-- loki.bog subsystem which handles Bog Prolog-based music programming
-- and MIDI playback via the built-in TinySoundFont synthesizer.
--
-- Bog is a Prolog-based live coding language for music where patterns
-- emerge from declarative rules rather than imperative sequencing.
-- The underlying C API (loki.bog.*) is automatically available when
-- the editor starts. This module adds higher-level helpers.
--
-- ==============================================================================
-- loki.bog C API Reference
-- ==============================================================================
--
-- Initialization:
--   loki.bog.init()               Initialize the Bog interpreter
--                                 Returns: true on success, nil + error on failure
--
--   loki.bog.is_initialized()     Check if Bog is ready
--                                 Returns: boolean
--
-- Evaluation:
--   loki.bog.eval(code)           Evaluate Bog code (Prolog-like rules)
--                                 code: Bog rule string
--                                 Returns: true on success, nil + error on failure
--
-- Playback:
--   loki.bog.stop()               Stop all playback
--
--   loki.bog.is_playing()         Check if Bog is playing
--                                 Returns: boolean
--
-- Configuration:
--   loki.bog.set_tempo(bpm)       Set tempo (20-400 BPM)
--                                 bpm: beats per minute
--
--   loki.bog.set_swing(amount)    Set swing amount (0.0-1.0)
--                                 amount: swing factor
--
-- ==============================================================================
-- Bog Event Predicate
-- ==============================================================================
--
-- All Bog patterns produce event/4 facts:
--   event(Voice, Pitch, Velocity, Time)
--
-- Voice: Sound source (kick, snare, hat, clap, noise, sine, square, triangle)
-- Pitch: MIDI note number (0-127)
-- Velocity: Intensity (0.0-1.0)
-- Time: Beat time (bound by timing predicates)
--
-- ==============================================================================
-- Bog Timing Predicates
-- ==============================================================================
--
-- every(T, N)              Fire every N beats
--                          every(T, 0.5) - 8th notes
--
-- beat(T, N)               Fire on beat N of bar
--                          beat(T, 1) - beat 1
--
-- euc(T, K, N, B, R)       Euclidean rhythm
--                          euc(T, 5, 16, 4, 0) - 5 hits over 16 steps
--
-- phase(T, P, L, O)        Phase pattern
--                          phase(T, 3, 8, 0) - 3/8 phase
--
-- ==============================================================================
-- Bog Selection Predicates
-- ==============================================================================
--
-- choose(X, List)          Random selection from list
-- seq(X, List)             Cycle through list in order
-- shuffle(X, List)         Random permutation
-- wrand(X, List)           Weighted random (pairs of [value, weight])
-- chance(P, Goal)          Execute Goal with probability P
--
-- ==============================================================================
-- Examples
-- ==============================================================================
--
-- -- Kick on every beat:
-- bog.eval("event(kick, 36, 0.9, T) :- every(T, 1.0).")
--
-- -- Hi-hat on 8th notes:
-- bog.eval("event(hat, 42, 0.5, T) :- every(T, 0.5).")
--
-- -- Euclidean tresillo:
-- bog.eval("event(clap, 39, 0.8, T) :- euc(T, 3, 8, 4, 0).")
--
-- -- Random melody:
-- bog.eval("event(sine, P, 0.7, T) :- every(T, 0.5), choose(P, [60,64,67,72]).")
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

-- Initialize Bog with options
function M.setup(opts)
    opts = opts or {}

    -- Initialize Bog interpreter
    local ok, err = loki.bog.init()
    if not ok then
        status("Bog init failed: " .. (err or "unknown"))
        return false, err
    end

    -- Set initial tempo if specified
    if opts.tempo then
        loki.bog.set_tempo(opts.tempo)
    end

    -- Set swing if specified
    if opts.swing then
        loki.bog.set_swing(opts.swing)
    end

    status("Bog ready")
    return true
end

-- ==============================================================================
-- Evaluation Helpers
-- ==============================================================================

-- Evaluate Bog code with error handling
function M.eval(code)
    if not loki.bog.is_initialized() then
        status("Bog not initialized")
        return false
    end

    if not code or code == "" then
        status("No code to evaluate")
        return false
    end

    local ok, err = loki.bog.eval(code)
    if not ok then
        status("Eval failed: " .. (err or "unknown"))
        return false
    end

    return true
end

-- Evaluate current buffer (editor mode)
function M.eval_file()
    if get_mode() ~= "editor" then
        print("bog.eval_file() requires editor mode")
        return false
    end

    if not loki.bog.is_initialized() then
        status("Bog not initialized")
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

    local ok, err = loki.bog.eval(code)
    if not ok then
        status("Eval failed: " .. (err or "unknown"))
        return false
    end

    status("Evaluated")
    return true
end

-- Evaluate current line (editor mode)
function M.eval_line()
    if get_mode() ~= "editor" then
        print("bog.eval_line() requires editor mode")
        return false
    end

    if not loki.bog.is_initialized() then
        status("Bog not initialized")
        return false
    end

    local row, _ = loki.get_cursor()
    local line = loki.get_line(row)

    if not line or line == "" then
        status("Empty line")
        return false
    end

    local ok, err = loki.bog.eval(line)
    if not ok then
        status("Eval failed: " .. (err or "unknown"))
        return false
    end

    status("Evaluated line")
    return true
end

-- Stop all playback
function M.stop()
    loki.bog.stop()
    status("Stopped")
end

-- ==============================================================================
-- Configuration Helpers
-- ==============================================================================

-- Set tempo
function M.tempo(bpm)
    if not loki.bog.is_initialized() then
        status("Bog not initialized")
        return false
    end
    loki.bog.set_tempo(bpm)
    status("Tempo: " .. bpm .. " BPM")
    return true
end

-- Set swing
function M.swing(amount)
    if not loki.bog.is_initialized() then
        status("Bog not initialized")
        return false
    end
    loki.bog.set_swing(amount)
    status("Swing: " .. amount)
    return true
end

-- ==============================================================================
-- Pattern Helpers
-- ==============================================================================

-- Define a simple kick pattern
function M.kick(interval)
    interval = interval or 1.0
    return M.eval(string.format("event(kick, 36, 0.9, T) :- every(T, %.2f).", interval))
end

-- Define a hi-hat pattern
function M.hat(interval)
    interval = interval or 0.5
    return M.eval(string.format("event(hat, 42, 0.5, T) :- every(T, %.2f).", interval))
end

-- Define a snare pattern on beats 2 and 4
function M.snare()
    return M.eval("event(snare, 38, 0.8, T) :- beat(T, 2).")
        and M.eval("event(snare, 38, 0.8, T) :- beat(T, 4).")
end

-- Define a euclidean rhythm
function M.euclidean(voice, pitch, hits, steps, bars, rotation)
    bars = bars or 4
    rotation = rotation or 0
    return M.eval(string.format(
        "event(%s, %d, 0.8, T) :- euc(T, %d, %d, %d, %d).",
        voice, pitch, hits, steps, bars, rotation
    ))
end

-- ==============================================================================
-- Status Helpers
-- ==============================================================================

-- Print full status info
function M.info()
    local initialized = loki.bog.is_initialized()
    print("Bog Status:")
    print("  Initialized: " .. (initialized and "yes" or "no"))

    if initialized then
        print("  Playing:     " .. (loki.bog.is_playing() and "yes" or "no"))
    end
end

-- ==============================================================================
-- Register with REPL help
-- ==============================================================================

if loki.repl and loki.repl.register then
    loki.repl.register("bog.setup", "Initialize Bog: bog.setup({tempo=120, swing=0.1})")
    loki.repl.register("bog.eval", "Evaluate Bog: bog.eval('event(kick, 36, 0.9, T) :- every(T, 1.0).')")
    loki.repl.register("bog.eval_file", "Evaluate current buffer (editor mode)")
    loki.repl.register("bog.eval_line", "Evaluate current line (editor mode)")
    loki.repl.register("bog.kick", "Quick kick pattern: bog.kick(1.0)")
    loki.repl.register("bog.hat", "Quick hi-hat pattern: bog.hat(0.5)")
    loki.repl.register("bog.snare", "Quick snare on 2&4: bog.snare()")
    loki.repl.register("bog.euclidean", "Euclidean rhythm: bog.euclidean('clap', 39, 5, 16)")
    loki.repl.register("bog.tempo", "Set tempo: bog.tempo(140)")
    loki.repl.register("bog.swing", "Set swing: bog.swing(0.2)")
    loki.repl.register("bog.stop", "Stop all playback")
    loki.repl.register("bog.info", "Show Bog status info")
end

return M
