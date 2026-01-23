# Psnd TODO

## Known Bugs

### `:plugin presets` buffer switch not working

**Status:** Unresolved

**Description:** The `:plugin presets` command is intended to open a new scratch buffer displaying all plugin presets, allowing users to scroll through and view the full list. The command executes without errors, but the new buffer does not appear - the editor stays on the original buffer.

**Expected behavior:**
1. User types `:plugin presets` and presses Enter
2. A new buffer opens showing:
   - Plugin name header
   - Total preset count and current preset number
   - Full list of presets with indices (e.g., `*   42: Warm Pad` where `*` marks current)
3. User can scroll through presets, then close buffer with `:q`

**Actual behavior:** Command returns success but display doesn't change. The original file buffer remains visible.

**Root cause analysis:**

The issue appears to be related to how buffer switching interacts with command mode exit:

1. In `command/plugin.c`, the `:plugin presets` handler:
   - Calls `buffer_create(NULL)` to create a new empty buffer
   - Calls `buffer_switch(buf_id)` to switch to the new buffer
   - Gets new context via `preset_ctx = buffer_get_current()`
   - Populates the buffer with preset data via `editor_insert_row()`
   - Sets `preset_ctx->view.mode = MODE_NORMAL`
   - Returns success (1)

2. In `command.c:command_mode_handle_key()`, after the command handler returns:
   - `command_mode_exit(ctx)` is called with the ORIGINAL context
   - This sets `ctx->view.mode = MODE_NORMAL` on the OLD buffer
   - Clears the status message on the OLD buffer

3. In `editor.c` main loop:
   - Each iteration calls `ctx = buffer_get_current()` which SHOULD return the new buffer
   - Calls `editor_refresh_screen(ctx)` which SHOULD render the new buffer

**What was tried:**
1. Verified `buffer_create()` returns valid buffer ID (checked object file symbols)
2. Verified `buffer_switch()` updates `buffer_state.current_buffer_id`
3. Added explicit `preset_ctx->view.mode = MODE_NORMAL` after buffer switch
4. Confirmed all code is compiled in (strings present in binary)
5. Clean rebuild with `BUILD_MINIHOST_BACKEND=ON`

**Possible issues to investigate:**
1. `buffer_switch()` may not properly save/restore state between buffers
2. The `ctx` passed to command handler may be cached somewhere else
3. Screen refresh may be using a stale context pointer
4. Buffer initialization may be incomplete (missing screen dimensions, etc.)
5. The command handler's return may trigger additional processing that resets state

**Files involved:**
- `source/core/loki/command/plugin.c` - Command implementation
- `source/core/loki/command.c` - Command execution and mode handling
- `source/core/loki/buffers.c` - Buffer management
- `source/core/loki/editor.c` - Main loop and screen refresh

**Workaround:** Use `:plugin preset <n>` to select presets by number, or `:plugin preset <name>` for partial name matching. The status bar shows current preset info with `:plugin`.

---

## High Priority

### Web Host Enhancements

The web host is functional with xterm.js terminal emulator. Remaining work:

- [ ] Multiple client support
  - Currently supports single WebSocket connection
  - Add connection management for concurrent clients

- [ ] Session persistence
  - Save/restore editor state across server restarts
  - Optional auto-save of open buffers

- [ ] Authentication
  - Add basic auth or token-based access for remote access
  - Required before exposing to network

### Architecture

- [ ] Extract buffer manager to injectable service
  - Remove global `buffer_state` in `buffers.c`
  - Enables multi-editor and better testability

- [ ] Wrap editor core in standalone service process (optional)
  - Small RPC protocol (stdio JSON or gRPC)
  - Commands: load file, save, apply keystroke, get view state
  - Would enable embedding editor in other applications

## Medium Priority

### MHS (Micro Haskell) REPL Enhancements

The MHS language now has full REPL feature parity with Joy/TR7/Bog via PTY-based stdin interposition.
See `docs/LANG_IMPL_COMPARISON.md` for architecture details.

**Implementation:** MicroHs REPL runs in a forked child process with a pseudo-terminal (PTY).
The parent process runs psnd's `repl_readline()` for input with syntax highlighting,
tab completion, and history. psnd commands are handled locally; Haskell code is
forwarded to MicroHs via the PTY. MIDI is initialized in the child process after fork
to ensure proper handle inheritance (libremidi handles don't survive fork).

Completed:

- [x] Implement PTY-based stdin interposition in `mhs_repl_main()`
  - Detects interactive mode with `isatty(STDIN_FILENO)`
  - Creates PTY with `forkpty()`, runs MicroHs in child process
  - Parent uses psnd's REPL loop with syntax-highlighted input
  - Filters commands with `shared_process_command()`
  - MIDI initialized in child after fork for proper handle inheritance

- [x] Add tab completion for Haskell keywords
  - 80+ keywords including MIDI primitives
  - Uses `repl_set_completion()` callback pattern

- [x] Add history persistence
  - Path: `~/.psnd/mhs_history`

- [x] Integrate Ableton Link callbacks
  - Polls `shared_repl_link_check()` in REPL loop

### Testing

- [x] Add synthesis backend tests
  - `test_tsf_backend.c` - 19 tests (init, cleanup, loading, enable/disable, MIDI messages, boundaries)
  - `test_csound_backend.c` - 31 tests (availability, init, loading, enable/disable, MIDI, render, playback)
  - Tests verify API behavior without crashing; full audio testing requires manual verification

- [ ] Add scanner/lexer unit tests for all languages
  - Alda scanner vulnerable to malformed input
  - Joy/Bog/TR7 lexers untested

- [ ] Add fuzzing infrastructure
  - Parser robustness for malformed input
  - Consider AFL or libFuzzer integration

### Ableton Link Integration

- [x] **Beat-Aligned Start** - Playback quantizes to Link beat grid
  - Added `shared_link_ms_to_next_beat(quantum)` function
  - Added `launch_quantize` field to SharedContext and SharedAsyncSchedule
  - Added `loki.link.launch_quantize(quantum)` Lua API (0=immediate, 1=beat, 4=bar)
  - Alda and Joy async playback now respects launch quantization

- [ ] **Full Transport Sync** - Start/stop from any Link peer controls all
  - Wire transport callbacks to actually start/stop playback
  - Requires interruptible playback and a "armed for playback" state
  - Most complex - requires rethinking REPL interaction model

### Editor Features

- [ ] Playback visualization
  - Highlight currently playing region
  - Show playback progress in status bar

- [x] MIDI port selection from editor
  - Added `loki.midi.list_ports()`, `loki.midi.port_count()`, `loki.midi.port_name(index)`
  - Added `loki.midi.open_port(index)`, `loki.midi.open_by_name(name)`
  - Added `loki.midi.open_virtual(name)`, `loki.midi.close()`, `loki.midi.is_open()`

- [ ] Tempo tap
  - Tap key to set tempo

- [ ] Metronome toggle

### Refactoring

### Build System

- [x] Add AddressSanitizer build target
  - `cmake -B build -DPSND_ENABLE_ASAN=ON .`

- [x] Add code coverage target
  - `cmake -B build -DPSND_ENABLE_COVERAGE=ON .`
  - Use lcov/gcov to generate reports

- [x] Add install target
  - `cmake --install build [--prefix /usr/local]`
  - Installs binary to `bin/psnd`
  - Installs config to `share/psnd/`

### Test Framework

- [ ] Refactor CLI tests to avoid shell spawning (`tests/cli/test_play_command.c:29-62`)
  - Tests use `system("rm -rf ...")` for cleanup and `system()` to invoke psnd
  - Couples tests to `/bin/sh`, ignores exit codes in some branches
  - Fix: Use `fork`/`execve` directly for binary invocation, `mkdtemp`/`nftw` for temp directory cleanup

- [ ] Add missing test coverage
  - No tests for: editor bridge, Ableton Link callbacks, shared REPL command processor
  - Pointer/string comparisons can silently truncate in test framework

- [ ] Add `ASSERT_GT`, `ASSERT_LT` macros

- [ ] Add test fixture support (setup/teardown)

- [ ] Add memory leak detection hooks

---

## Low Priority

### Code Consolidation

- [ ] Extract shared REPL loop skeleton
  - ~150 lines of help functions still duplicated per language
  - Interactive loop structure still duplicated (could use callback pattern)

- [ ] Centralize platform CMake logic
  - Platform detection repeated in 6+ CMakeLists.txt files
  - Create `psnd_platform.cmake` module

### Platform Support

- [ ] Windows support
  - Editor uses POSIX headers: `termios.h`, `unistd.h`, `pthread.h`
  - Options: Native Windows console API, or web editor using CodeMirror/WebSockets

### Editor Features

- [ ] Split windows
  - Already designed for in `editor_ctx_t`
  - Requires screen rendering changes

- [ ] Tree-sitter integration
  - Would improve syntax highlighting, code handling
  - Significantly more complex than current system

- [ ] LSP client integration
  - Would provide IDE-like features
  - High complexity undertaking

- [ ] Git integration
  - Gutter diff markers
  - Stage/commit commands

### Documentation

- [x] Architecture diagram
  - Created `docs/architecture.d2` using D2 language
  - Renders to SVG/PNG: `d2 architecture.d2 architecture.svg`

- [ ] API reference generation
  - Consider Doxygen for generated docs

- [ ] Contributing guide

- [ ] Build troubleshooting
  - Platform-specific guidance

### Future Architecture

- [x] Lua-to-language primitive callbacks (Joy implemented)
  - Allow registering Lua functions as language primitives
  - Enables extending languages with Lua's ecosystem (HTTP, JSON, etc.)

  **Joy** (stack-based) - IMPLEMENTED:
  - API: `loki.joy.register_primitive("name", lua_callback)`
  - Callback receives: `function(stack) ... return modified_stack end`
  - Stack is a Lua array (index 1 = bottom, #stack = top)
  - Values: integers, floats, booleans, strings, tables (for lists/quotations)
  - Quotations represented as `{type="quotation", value={...tokens...}}`
  - Return modified stack or `nil, "error message"` on failure
  - Example:

    ```lua
    loki.joy.register_primitive("double", function(stack)
        if #stack < 1 then return nil, "stack underflow" end
        local top = table.remove(stack)
        table.insert(stack, top * 2)
        return stack
    end)
    ```

  - Files: `source/langs/joy/register.c` (lua_joy_register_primitive, joy_lua_primitive_wrapper)

  **TR7** (Scheme) - NOT YET IMPLEMENTED:
  - API: `loki.tr7.register_primitive("name", lua_callback, min_args, max_args)`
  - Callback receives: `function(arg1, arg2, ...) return result end`
  - Uses TR7's `tr7_C_func_def_t` registration with Lua state as closure
  - Type conversion via `TR7_FROM_*`/`TR7_TO_*` macros
  - Return value becomes Scheme result; `nil, "error"` raises exception
  - Implementation requires:
    - C wrapper `lua_proc_wrapper(tr7_engine_t, int nvalues, tr7_t* values, void* L)`
    - Convert `tr7_t` args to Lua values, call Lua function, convert result back
    - Register via `tr7_register_C_func(engine, &def)`
  - Complexity: Moderate (value conversion, error handling)

  **Bog** (Prolog) - NOT YET IMPLEMENTED:
  - API: `loki.bog.register_predicate("name", arity, lua_callback)`
  - Callback receives: `function(args_table, env_table) return solutions end`
  - `args_table`: array of terms (numbers, atoms, compounds, lists)
  - `env_table`: current variable bindings `{X = value, Y = value}`
  - Return: array of solution environments `{{X=1, Y=2}, {X=3, Y=4}}` or empty for failure
  - Implementation requires:
    - Extend `BogBuiltins` to support dynamic registration
    - C wrapper converting `BogTerm**` to Lua tables and back
    - Thread `lua_State*` through `BogContext` or scheduler
    - Handle non-determinism (multiple solutions)
  - Complexity: High (unification semantics, backtracking, term conversion)

- [ ] Plugin architecture for language modules
  - Dynamic loading of language support

- [ ] JACK backend
  - For pro audio workflows

- [ ] Provide a minimal language example

---

## Add Languages

- [ ] bytebeat (see: <https://dollchan.net/bytebeat>)
- [ ] funcbeat
- [ ] drumbeat (see: <https://wavepot.com>)

## Feature Opportunities

### Preset Browser & Layering

- [ ] Add preset browsing UI to editor/REPL
  - TSF already exposes preset metadata via `shared_tsf_get_preset_name()`
  - No UI for browsing, tagging, or layering presets
  - Let musicians audition instruments and build splits/stacks without editing raw program numbers

### Session Capture & Arrangement

- [ ] Elevate shared MIDI event buffer to first-class timeline (`src/shared/midi/events.h`)
  - Currently only feeds export
  - Capture REPL improvisations into clips, arrange them, re-trigger live
  - Similar to Ableton's Session View but text-driven

### Controller & Automation Mapping

- [ ] Map physical MIDI controllers or OSC sources to language variables
  - Tempo, volume, macro parameters
  - Makes Joy/TR7 live-coding sets more expressive
  - Combine with existing Ableton Link transport hooks

### Cross-Language Patch Sharing

- [ ] Create lightweight messaging bus for Alda, Joy, TR7 to exchange motifs
  - Example: Joy macro emits motif that Alda editor picks up and renders with full notation
  - Showcases polyglot nature, keeps multiple buffers in sync

### Real-Time Visualization

- [ ] Expose playback state in loki status bar or via OSC/WebSocket
  - Current measure, active voices, CPU load
  - Visual confirmation when multiple asynchronous schedulers are active

---

## Recently Completed

### MHS Language Improvements

- Added syntax highlighting for Haskell/MHS (`.hs`, `.mhs`, `.lhs` files)
  - Keywords, types, MIDI primitives, Music module functions
  - Created `source/core/loki/syntax/lang_haskell.h`
- Added syntax highlighting for Bog (`.bog` files)
  - Predicates, scales, chords, voice names
  - Created `source/core/loki/syntax/lang_bog.h`
- Added CLI flags for MHS REPL: `--virtual`, `-sf`, `-p`, `-l`, `-v`
- Added SharedContext integration for MHS REPL
  - Routes MIDI through SharedContext for TSF/Csound/Link support
  - Proper initialization and cleanup of MIDI/audio backends
- Created `docs/LANG_IMPL_COMPARISON.md` documenting feature parity across languages

### Parameter Binding System

- Added parameter system for binding named parameters to MIDI CC and OSC
- Thread-safe atomic floats for lock-free access from MIDI/OSC threads
- MIDI input support with CC-to-parameter routing
- OSC endpoints: `/psnd/param/set`, `/psnd/param/get`, `/psnd/param/list`
- Lua API: `loki.param` / `param` module
- Joy primitives: `param`, `param!`, `param-list`
- Files: `source/core/shared/param/param.c`, `source/core/shared/midi/midi_input.c`

### Web Editor Implementation (Phases 1-7)

- Eliminated global state and singletons
- Split model from view in `editor_ctx_t`
- Abstracted input handling with `EditorEvent` struct
- Introduced renderer interface with `EditorViewModel`
- Created host-agnostic `EditorSession` API
- Added event queue for async tasks
- Built web front-end with xterm.js terminal emulator
- Embedded xterm.js in binary (optional, via `LOKI_EMBED_XTERM`)
- Added mouse click-to-position support
- Added language switching commands (`:alda`, `:joy`, `:langs`)
- Added first-line directive support (`#alda`, `#joy`)

### SharedContext Centralization

- EditorModel now owns single SharedContext for all languages
- Languages share context instead of creating separate instances
- Prevents conflicts on singleton backends (TSF, Csound, Link)
- REPLs still own their own SharedContext for standalone mode

### REPL Enhancements

- Added `:lang NAME` command to switch between language REPLs
- Added `:langs` command to list available languages
- Unified MIDI port name to `PSND_MIDI` across all languages

### Other Completed Items

- Standardized error return conventions
- Unified Lua binding pattern across languages
- Extracted shared REPL helper utilities
- Implemented `:play` command with file-type dispatch
- Wired Ableton Link callbacks for tempo sync
- Added TR7 test suite (38 tests)
