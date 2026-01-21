# Language Implementation Comparison

This document compares the feature completeness of each language implementation in psnd.

## Feature Matrix

| Feature | Alda | Joy | TR7 | Bog | MHS |
|---------|:----:|:---:|:---:|:---:|:---:|
| **Editor Integration** |
| Syntax highlighting | Yes | Yes | Yes | Yes | Yes |
| Ctrl-E (eval line) | Yes | Yes | Yes | Yes | Yes |
| Ctrl-P (play file) | Yes | Yes | Yes | Yes | Yes |
| Ctrl-G (stop) | Yes | Yes | Yes | Yes | Yes |
| Lua API (`loki.<lang>.*`) | Yes | Yes | No | Yes | Yes |
| **REPL Features** |
| Custom REPL loop | Yes | Yes | Yes | Yes | **No** |
| Syntax highlighting in input | Yes | Yes | Yes | Yes | **No** |
| Shared commands (`:help`, `:list`, etc.) | Yes | Yes | Yes | Yes | **No** |
| Tab completion | Yes | Yes | Yes | Yes | **No** |
| History persistence | Yes | Yes | Yes | Yes | **No** |
| Piped input support | Yes | Yes | Yes | Yes | **No** |
| Language-specific help | Yes | Yes | Yes | Yes | **No** |
| **CLI Flags** |
| `--virtual NAME` | Yes | Yes | Yes | Yes | Yes |
| `-sf PATH` (soundfont) | Yes | Yes | Yes | Yes | Yes |
| `-p N` (port select) | Yes | Yes | Yes | Yes | Yes |
| `-v` (verbose) | Yes | Yes | Yes | Yes | Yes |
| `-l` (list ports) | Yes | Yes | Yes | Yes | Yes |
| **Backend Integration** |
| Ableton Link callbacks | Yes | Yes | Yes | Yes | **No** |
| Async playback | Yes | Yes | Yes | Yes | Partial |
| SharedContext usage | Yes | Yes | Yes | Yes | Yes |

## Legend

- **Yes**: Feature is fully implemented
- **No**: Feature is missing
- **Partial**: Feature exists but with limitations

## Architecture Notes

### Alda
- Full-featured reference implementation
- Custom parser, interpreter, and async scheduler
- All REPL and editor features implemented

### Joy
- Stack-based concatenative language
- Full REPL with `repl_readline()` and shared commands
- Complete editor integration with Lua API

### TR7
- R7RS-small Scheme interpreter
- Full REPL features but no editor Lua API
- Music primitives added as Scheme procedures

### Bog
- Prolog-based pattern language
- Full REPL with slot management (`:def`, `:undef`, `:slots`)
- Syntax highlighting for predicates, voices, scales, and chords

### MHS (Micro Haskell)
- Wraps MicroHs interpreter directly
- Has editor integration (Lua API)
- Syntax highlighting for Haskell keywords, types, and MIDI primitives
- CLI flags for MIDI setup (`--virtual`, `-sf`, `-p`, `-l`, `-v`)
- Routes MIDI through SharedContext for TSF/Csound/Link support
- REPL delegates to MicroHs's built-in REPL (no custom loop yet)
- Still missing: shared commands, tab completion, history persistence

## Implementation Details

### REPL Loop Comparison

**Joy/TR7/Bog pattern** (full featured):
```c
// Initialize editor context for syntax highlighting
editor_ctx_t ed;
editor_ctx_init(&ed);
syntax_init_default_colors(&ed);

// Set up tab completion
repl_set_completion(ed, completion_callback, user_data);

// Load history
repl_history_load(&ed, history_path);

// Main loop with syntax highlighting
repl_enable_raw_mode();
while ((input = repl_readline(syntax_ctx, &ed, "prompt> "))) {
    if (shared_process_command(input, ctx)) continue;
    // Process language-specific input
    ...
    shared_repl_link_check(ctx);  // Link callbacks
}
repl_disable_raw_mode();
repl_history_save(&ed, history_path);
```

**MHS pattern** (minimal):
```c
// Directly invoke MicroHs main
mhs_main(argc, argv);
// No custom REPL loop, no psnd integration
```

### Shared REPL Commands

All languages except MHS support these commands via `shared_process_command()`:

| Command | Description |
|---------|-------------|
| `:q` `:quit` `:exit` | Exit REPL |
| `:h` `:help` `:?` | Show help |
| `:l` `:list` | List MIDI ports |
| `:s` `:stop` | Stop playback |
| `:p` `:panic` | All notes off |
| `:sf PATH` | Load soundfont |
| `:synth` `:builtin` | Switch to built-in synth |
| `:midi` | Switch to MIDI output |
| `:virtual [NAME]` | Create virtual MIDI port |
| `:link [on\|off]` | Toggle Ableton Link |
| `:link-tempo BPM` | Set Link tempo |
| `:link-status` | Show Link status |
| `:cs PATH` | Load CSD file |
| `:csound` | Enable Csound backend |

### CLI Flag Parsing

Joy/TR7/Bog use `SharedReplArgs` structure via `shared_lang_repl_main()`:
```c
typedef struct {
    const char* soundfont_path;
    const char* virtual_name;
    const char* csound_path;
    int port_index;
    bool verbose;
    // ...
} SharedReplArgs;
```

MHS uses its own `MhsReplArgs` structure with similar fields:
```c
typedef struct {
    const char *virtual_name;
    const char *soundfont_path;
    int port_index;
    int list_ports;
    int verbose;
    int show_help;
    int mhs_argc;      /* Remaining args for MicroHs */
    char **mhs_argv;
} MhsReplArgs;
```

MHS separates psnd flags from MicroHs flags, initializes SharedContext
and MIDI based on the psnd flags, then passes remaining flags to MicroHs.

## Recommendations for MHS

To bring MHS to feature parity:

1. **High Priority**
   - ~~Add syntax highlighting (`lang_haskell.h`)~~ **DONE**
   - ~~Add CLI flags (`--virtual`, `-sf`, `-p`, `-l`, `-v`)~~ **DONE**
   - Implement custom REPL loop with `repl_readline()`
   - Add shared command processing (`:help`, `:list`, `:stop`, etc.)

2. **Medium Priority**
   - Add tab completion for Haskell keywords
   - Add history persistence
   - Add piped input support
   - Integrate Ableton Link callbacks

3. **Low Priority**
   - Add language-specific help text
   - Add verbose flag

## File Locations

| Language | REPL | Register | Dispatch |
|----------|------|----------|----------|
| Alda | `source/langs/alda/repl.c` | `source/langs/alda/register.c` | `source/langs/alda/dispatch.c` |
| Joy | `source/langs/joy/repl.c` | `source/langs/joy/register.c` | `source/langs/joy/dispatch.c` |
| TR7 | `source/langs/tr7/impl/repl.c` | `source/langs/tr7/impl/register.c` | `source/langs/tr7/dispatch.c` |
| Bog | `source/langs/bog/repl.c` | `source/langs/bog/register.c` | `source/langs/bog/dispatch.c` |
| MHS | `source/langs/mhs/repl.c` | `source/langs/mhs/register.c` | `source/langs/mhs/dispatch.c` |

Syntax definitions: `source/core/loki/syntax/lang_*.h`
