# aldev

A unified command-line tool for the [Alda](https://alda.io) music programming language featuring a live-coding editor and interactive REPL.

Built on [loki](https://github.com/shakfu/loki), a lightweight text editor with vim-like modal editing and Lua scripting. Uses [libalda](https://github.com/shakfu/midi-langs/tree/main/alda-midi) for Alda parsing and playback.

## Status

Early development. Core functionality works but the API is evolving.

## Features

- **Editor Mode**: Vim-like modal editor with live-coding support
- **REPL Mode**: Interactive Alda composition - type notation directly
- **Play Mode**: Headless playback for scripts and automation
- **Ableton Link**: Tempo sync with DAWs and other musicians on the network
- Builtin MIDI output using [libremidi](https://github.com/celtera/libremidi).
- Built-in software synthesizer using [TinySoundFont](https://github.com/schellingb/TinySoundFont) and [miniaudio](https://github.com/mackron/miniaudio).
- Async playback (non-blocking)
- Lua scripting for editor customization

## Building

```bash
make
```

## Usage

### REPL Mode (Interactive Composition)

```bash
aldev                    # Start REPL
aldev -sf gm.sf2         # REPL with built-in synth
```

Type Alda notation directly:

```
alda> piano: c d e f g
alda> violin: o5 a b > c d e
alda> :stop
alda> :q
```

REPL commands (use with or without `:` prefix):

| Command | Action |
|---------|--------|
| `:q` `:quit` | Exit REPL |
| `:h` `:help` | Show help |
| `:l` `:list` | List MIDI ports |
| `:s` `:stop` | Stop playback |
| `:p` `:panic` | All notes off |
| `:sf PATH` | Load soundfont |
| `:presets` | List soundfont presets |
| `:midi` | Switch to MIDI output |
| `:synth` | Switch to built-in synth |
| `:link [on\|off]` | Toggle Ableton Link sync |

### Editor Mode (Live-Coding)

```bash
aldev song.alda          # Open file in editor
```

**Keybindings:**

| Key | Action |
|-----|--------|
| `Ctrl-E` | Play current part (or selection) |
| `Ctrl-P` | Play entire file |
| `Ctrl-G` | Stop playback |
| `Ctrl-S` | Save |
| `Ctrl-Q` | Quit |
| `Ctrl-F` | Find |
| `Ctrl-L` | Lua console |
| `i` | Enter INSERT mode |
| `ESC` | Return to NORMAL mode |

### Play Mode (Headless)

```bash
aldev play song.alda              # Play file and exit
aldev play -sf gm.sf2 song.alda   # Play with built-in synth
```

## Lua Scripting (Editor)

Press `Ctrl-L` in the editor to access the Lua console:

```lua
-- Play Alda code
loki.alda.eval_sync("piano: c d e f g a b > c")

-- Async playback with callback
loki.alda.eval("piano: c d e f g", "on_done")

-- Stop playback
loki.alda.stop_all()

-- Load soundfont for built-in synth
loki.alda.load_soundfont("path/to/soundfont.sf2")
loki.alda.set_synth(true)
```

## Ableton Link

Aldev supports [Ableton Link](https://www.ableton.com/en/link/) for tempo synchronization with other musicians and applications on the same network.

### Quick Start

In the editor, use the `:link` command:

```
:link on       # Enable Link
:link off      # Disable Link
:link          # Toggle Link
```

When Link is enabled:
- Status bar shows "ALDA LINK" instead of "ALDA NORMAL"
- Playback tempo syncs with the Link session
- Other Link-enabled apps (Ableton Live, etc.) will share the same tempo

### Lua API

```lua
-- Initialize and enable Link
loki.link.init(120)           -- Initialize with 120 BPM
loki.link.enable(true)        -- Start networking

-- Tempo control
loki.link.tempo()             -- Get session tempo
loki.link.set_tempo(140)      -- Set tempo (syncs to all peers)

-- Session info
loki.link.peers()             -- Number of connected peers
loki.link.beat(4)             -- Current beat (4 beats per bar)
loki.link.phase(4)            -- Phase within bar [0, 4)

-- Transport sync (optional)
loki.link.start_stop_sync(true)
loki.link.play()              -- Start transport
loki.link.stop()              -- Stop transport
loki.link.is_playing()        -- Check transport state

-- Callbacks (called when values change)
loki.link.on_tempo("my_tempo_handler")
loki.link.on_peers("my_peers_handler")
loki.link.on_start_stop("my_transport_handler")

-- Cleanup
loki.link.cleanup()
```

## Documentation

See the `docs` folder for full technical documentation.

## Credits

- [Alda](https://alda.io) - music programming language by Dave Yarwood
- [kilo](https://github.com/antirez/kilo) by Salvatore Sanfilippo (antirez) - original editor
- [loki](https://github.com/shakfu/loki) - Lua-enhanced fork
- [link](https://github.com/Ableton/link) - Ableton Link
- [libremidi](https://github.com/celtera/libremidi) - A modern C++ MIDI 1 / MIDI 2 real-time & file I/O library. Supports Windows, macOS, Linux and WebMIDI.
- [TinySoundFont](https://github.com/schellingb/TinySoundFont) - SoundFont2 synthesizer library in a single C/C++ file
- [miniaudio](https://github.com/mackron/miniaudio) - Audio playback and capture library written in C, in a single source file.

## License

GPL-3

see [docs/licenses](docs/licenses) for dependent licenses

