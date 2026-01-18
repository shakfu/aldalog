# Bog

A C implementation of a Prolog-based live coding language for music, inspired by danja's [dogalog](https://github.com/danja/dogalog). Bog allows defining musical events using logic programming, where patterns emerge from declarative rules rather than imperative sequencing.

## Quick Start

### Starting the REPL

```bash
# Start interactive Bog REPL
psnd bog

# With virtual MIDI port (macOS)
psnd bog --virtual BogOut

# With built-in synth (TinySoundFont)
psnd bog -sf /path/to/soundfont.sf2

# List available MIDI ports
psnd bog -l
```

### Your First Beat

In the Bog REPL, define a simple kick pattern:

```prolog
bog> :def kick event(kick, 36, 0.9, T) :- every(T, 1.0).
Slot 'kick' defined (new)
```

Add a hi-hat:

```prolog
bog> :def hat event(hat, 42, 0.5, T) :- every(T, 0.25).
Slot 'hat' defined (new)
```

List your slots:

```prolog
bog> :slots
Slots:
  kick: event(kick, 36, 0.9, T) :- every(T, 1.0).
  hat: event(hat, 42, 0.5, T) :- every(T, 0.25).
```

## Core Concepts

### Event Predicate

All Bog patterns produce `event/4` facts:

```prolog
event(Voice, Pitch, Velocity, Time)
```

- **Voice**: Sound source (`kick`, `snare`, `hat`, `clap`, `noise`, `sine`, `square`, `triangle`)
- **Pitch**: MIDI note number (0-127) or ignored for drums
- **Velocity**: Intensity (0.0-1.0)
- **Time**: Beat time (bound by timing predicates)

### Timing Predicates

| Predicate | Description | Example |
|-----------|-------------|---------|
| `every(T, N)` | Fire every N beats | `every(T, 0.5)` - 8th notes |
| `beat(T, N)` | Fire on beat N of bar | `beat(T, 1)` - beat 1 |
| `euc(T, K, N, B, R)` | Euclidean rhythm | `euc(T, 5, 16, 4, 0)` - 5 hits over 16 steps |
| `phase(T, P, L, O)` | Phase pattern | `phase(T, 3, 8, 0)` - 3/8 phase |

### Euclidean Rhythms

The `euc/5` predicate generates evenly-distributed rhythms:

```prolog
euc(T, Hits, Steps, BeatsPerBar, Rotation)
```

- **Hits (K)**: Number of onsets
- **Steps (N)**: Total steps in the pattern
- **BeatsPerBar (B)**: Bar length (typically 4)
- **Rotation (R)**: Rotate pattern by R steps

Examples:

```prolog
% Classic tresillo (3 hits over 8 steps)
event(kick, 36, 0.9, T) :- euc(T, 3, 8, 4, 0).

% Son clave
event(clap, 39, 0.8, T) :- euc(T, 5, 16, 4, 0).

% Off-beat hi-hats (rotated)
event(hat, 42, 0.5, T) :- euc(T, 4, 16, 4, 1).
```

### Selection Predicates

| Predicate | Description |
|-----------|-------------|
| `choose(X, List)` | Random selection from list |
| `seq(X, List)` | Cycle through list in order |
| `shuffle(X, List)` | Random permutation |
| `wrand(X, List)` | Weighted random (pairs of `[value, weight]`) |
| `chance(P, Goal)` | Execute Goal with probability P |

Examples:

```prolog
% Random pitch from list
event(sine, Pitch, 0.7, T) :- every(T, 0.5), choose(Pitch, [60, 64, 67, 72]).

% Cycling bass line
event(sine, Pitch, 0.8, T) :- every(T, 1.0), seq(Pitch, [36, 36, 43, 41]).

% 50% chance to play
event(clap, 39, 0.6, T) :- every(T, 0.5), chance(0.5, true).
```

### Music Theory Predicates

| Predicate | Description |
|-----------|-------------|
| `scale(Pitch, Root, Mode, Degree, Octave)` | Get pitch from scale |
| `chord(Notes, Root, Type)` | Get chord notes |
| `inv(Notes, Chord, Inversion)` | Invert a chord |

```prolog
% C major scale, 3rd degree, octave 4
scale(Pitch, 60, ionian, 3, 0).  % Pitch = 64 (E)

% A minor chord
chord(Notes, 57, minor).  % Notes = [57, 60, 64]
```

## REPL Commands

### General Commands

| Command | Description |
|---------|-------------|
| `:help` (`:h`) | Show help |
| `:quit` (`:q`) | Exit REPL |
| `:stop` (`:s`) | Stop all playback |

### MIDI Commands

| Command | Description |
|---------|-------------|
| `:midi` (`:m`) | Show current MIDI port |
| `:l [N]` | List MIDI ports or set port N |
| `:panic` | All notes off |

### Bog-Specific Commands

| Command | Description |
|---------|-------------|
| `:play FILE` | Load and execute a .bog file |
| `:tempo BPM` | Set tempo (default: 120) |
| `:swing AMOUNT` | Set swing (0.0-1.0, default: 0.0) |

### Slot Commands (Named Rules)

Slots allow you to manage multiple patterns that play together.

| Command | Short | Description |
|---------|-------|-------------|
| `:def NAME RULE` | `:d` | Define or replace a named slot |
| `:undef NAME` | `:u` | Remove a named slot |
| `:slots` | `:ls` | Show all defined slots |
| `:clear` | | Remove all slots |
| `:mute NAME` | | Mute a slot (keeps rule, stops sound) |
| `:unmute NAME` | | Unmute a slot |
| `:solo NAME` | | Mute all except named slot |
| `:unsolo` | | Unmute all slots |

### Slot Workflow Example

```prolog
bog> :d kick event(kick, 36, 0.9, T) :- every(T, 1.0).
Slot 'kick' defined (new)

bog> :d snare event(snare, 38, 0.8, T) :- every(T, 2.0).
Slot 'snare' defined (new)

bog> :d hat event(hat, 42, 0.5, T) :- every(T, 0.25).
Slot 'hat' defined (new)

bog> :ls
Slots:
  kick: event(kick, 36, 0.9, T) :- every(T, 1.0).
  snare: event(snare, 38, 0.8, T) :- every(T, 2.0).
  hat: event(hat, 42, 0.5, T) :- every(T, 0.25).

bog> :mute hat
Slot 'hat' muted

bog> :solo kick
Soloing 'kick'

bog> :unsolo
All slots unmuted

bog> :u snare
Slot 'snare' removed

bog> :clear
All slots cleared
```

## MIDI Voice Mapping

Bog voices map to General MIDI:

| Voice | MIDI Note | Channel | GM Sound |
|-------|-----------|---------|----------|
| `kick` | 36 | 10 | Bass Drum 1 |
| `snare` | 38 | 10 | Acoustic Snare |
| `hat` | 42 | 10 | Closed Hi-Hat |
| `clap` | 39 | 10 | Hand Clap |
| `noise` | 46 | 10 | Open Hi-Hat |
| `sine` | pitch | 1 | (melodic) |
| `square` | pitch | 1 | (melodic) |
| `triangle` | pitch | 1 | (melodic) |

For melodic voices, the Pitch argument determines the MIDI note number.

## Editor Integration

Bog files (`.bog`) can be opened in the psnd editor:

```bash
psnd song.bog
```

### Keybindings

| Key | Mode | Action |
|-----|------|--------|
| `Ctrl-E` | Normal | Evaluate current buffer |
| `Ctrl-S` | Normal | Stop playback |
| `Ctrl-P` | Normal | Panic (all notes off) |

### Lua API

The editor exposes Bog functions via Lua:

```lua
-- Initialize Bog
loki.bog.init()

-- Evaluate code
loki.bog.eval("event(kick, 36, 0.9, T) :- every(T, 1.0).")

-- Stop playback
loki.bog.stop()

-- Check if playing
if loki.bog.is_playing() then
    print("Playing")
end

-- Set tempo
loki.bog.set_tempo(140)

-- Set swing
loki.bog.set_swing(0.3)
```

## Example Patterns

### Basic Four-on-the-Floor

```prolog
event(kick, 36, 0.9, T) :- every(T, 1.0).
event(snare, 38, 0.8, T) :- beat(T, 2), beat(T, 4).
event(hat, 42, 0.5, T) :- every(T, 0.5).
```

### Breakbeat with Euclidean Rhythms

```prolog
event(kick, 36, 0.9, T) :- euc(T, 5, 16, 4, 0).
event(snare, 38, 0.85, T) :- euc(T, 3, 8, 4, 2).
event(hat, 42, 0.4, T) :- every(T, 0.25).
```

### Melodic Sequence

```prolog
event(sine, Pitch, 0.7, T) :-
    every(T, 0.5),
    seq(Pitch, [60, 64, 67, 72, 67, 64]).
```

### Random Variations

```prolog
event(kick, 36, Vel, T) :-
    every(T, 1.0),
    choose(Vel, [0.7, 0.8, 0.9, 1.0]).

event(hat, 42, 0.5, T) :-
    every(T, 0.25),
    chance(0.7, true).
```

### Polyrhythm

```prolog
event(kick, 36, 0.9, T) :- euc(T, 3, 8, 4, 0).
event(snare, 38, 0.8, T) :- euc(T, 4, 12, 4, 0).
```

## File Format

Bog files (`.bog`) contain Prolog-like rules:

```prolog
% Drums
event(kick, 36, 0.9, T) :- every(T, 1.0).
event(snare, 38, 0.8, T) :- every(T, 2.0).

% Melody
event(sine, Pitch, 0.7, T) :-
    every(T, 0.5),
    seq(Pitch, [60, 64, 67, 72]).
```

Run a file directly:

```bash
psnd bog song.bog
```

Or load in the REPL:

```prolog
bog> :play song.bog
```

## Architecture

For implementation details, see [overview.md](overview.md).

## Attribution

Bog is inspired by and based on danja's [dogalog](https://github.com/danja/dogalog) project.
