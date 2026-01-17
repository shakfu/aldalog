# Alda

Alda is a declarative music composition language created by [Dave Yarwood](https://github.com/daveyarwood). It provides a simple, readable syntax for writing music that compiles to MIDI. psnd includes a complete Alda interpreter with non-blocking async playback.

For the official Alda documentation, see [alda.io](https://alda.io).

## Quick Start

### Starting the REPL

```bash
# Start interactive Alda REPL
psnd alda

# With built-in synth (TinySoundFont)
psnd alda -sf /path/to/soundfont.sf2

# With virtual MIDI port (macOS)
psnd alda --virtual AldaOut

# List available MIDI ports
psnd alda -l
```

### Your First Melody

Type Alda notation directly in the REPL:

```text
alda> piano: c d e f g a b > c
```

This plays a C major scale ascending from middle C to C5.

### Playing Chords

```text
alda> piano: c/e/g
```

This plays a C major chord (C, E, and G simultaneously).

## Core Concepts

### Instruments

Alda supports all 128 General MIDI instruments. Declare an instrument before writing notes:

```alda
piano: c d e f g
violin: g a b > c d
trumpet: c4 c c c | e2
```

Common instruments include:
- **Piano**: `piano`, `electric-piano`, `harpsichord`
- **Strings**: `violin`, `viola`, `cello`, `contrabass`
- **Woodwinds**: `flute`, `clarinet`, `oboe`, `bassoon`
- **Brass**: `trumpet`, `trombone`, `french-horn`, `tuba`
- **Guitar**: `acoustic-guitar`, `electric-guitar-clean`, `electric-guitar-distorted`
- **Percussion**: `midi-percussion` (channel 10)

### Notes and Rests

Notes are written as letters (a-g) with optional accidentals and durations:

| Notation | Description |
|----------|-------------|
| `c d e f g a b` | Note names |
| `c+ d+ f+ g+ a+` | Sharps |
| `d- e- a- b-` | Flats |
| `r` | Rest |

### Durations

Durations follow note names:

| Notation | Duration |
|----------|----------|
| `c1` | Whole note |
| `c2` | Half note |
| `c4` | Quarter note (default) |
| `c8` | Eighth note |
| `c16` | Sixteenth note |
| `c4.` | Dotted quarter |
| `c4..` | Double-dotted quarter |
| `c4~8` | Tied quarter + eighth |

```alda
piano: c1 d2 e4 f8 g16
piano: c4. d8 e4 f4
piano: c2~4 d2
```

### Octaves

Control the octave with explicit markers or relative shifts:

| Notation | Description |
|----------|-------------|
| `o4` | Set octave to 4 (middle C = o4 c) |
| `>` | Move up one octave |
| `<` | Move down one octave |

```alda
piano: o3 c d e f | o4 c d e f | o5 c d e f
piano: c > c > c > c < < < c
```

### Chords

Stack notes with `/` to play them simultaneously:

```alda
piano: c/e/g           # C major chord
piano: c/e-/g          # C minor chord
piano: c/e/g/b         # C major 7th
piano: c1/e/g/>c       # C major with octave doubling
```

### Attributes

Set musical attributes in parentheses:

| Attribute | Description | Example |
|-----------|-------------|---------|
| `(tempo N)` | Set tempo in BPM | `(tempo 120)` |
| `(tempo! N)` | Set tempo globally | `(tempo! 140)` |
| `(volume N)` | Set volume (0-100) | `(volume 80)` |
| `(pan N)` | Set stereo pan (0-100) | `(pan 25)` |
| `(quant N)` | Set quantization (0-100) | `(quant 90)` |
| `(key-sig "...")` | Set key signature | `(key-sig "f+ c+")` |
| `(transpose N)` | Transpose by N semitones | `(transpose 2)` |

```alda
piano:
  (tempo 140)
  (volume 80)
  c d e f g a b > c
```

### Dynamics

Use dynamic markings for expression:

| Marking | Velocity | Description |
|---------|----------|-------------|
| `(ppp)` | ~20 | Pianississimo |
| `(pp)` | ~35 | Pianissimo |
| `(p)` | ~50 | Piano |
| `(mp)` | ~65 | Mezzo-piano |
| `(mf)` | ~80 | Mezzo-forte |
| `(f)` | ~95 | Forte |
| `(ff)` | ~110 | Fortissimo |
| `(fff)` | ~125 | Fortississimo |

```alda
piano:
  (p) c d e f
  (mf) g a b > c
  (f) d e f g
```

### Bar Lines

Use `|` for visual organization (optional, doesn't affect playback):

```alda
piano:
  c4 c g g | a a g2 |
  f4 f e e | d d c2
```

### Repeats

Repeat notes or sequences:

```alda
piano: c8*4              # Repeat note 4 times
piano: [c8 d e]*4        # Repeat sequence 4 times
piano: [c d e f]*2 g2    # Repeat then continue
```

### Voices (Polyphony)

Write independent melodic lines within one instrument:

```alda
piano:
  V1: c1 d e f
  V2: e1 f g a
  V3: g1 a b > c
  V0:
  < c1/e/g
```

- `V1:`, `V2:`, `V3:` etc. declare independent voices
- Each voice has its own timing
- `V0:` merges all voices at the latest position

### Variables

Define reusable musical phrases:

```alda
motif = c8 d e f g4

piano:
  motif motif > motif < motif
```

### Markers

Jump to named positions:

```alda
piano:
  %verse
  c d e f | g a b > c |
  @verse   # Jump back to verse marker
  c d e f | g2 e2
```

### Cram Expressions

Fit notes into a specific duration:

```alda
piano:
  {c d e f g}4    # Five notes in the space of a quarter note
  {c d e}2        # Three notes in a half note
```

## Multiple Instruments

### Parts

Each instrument declaration creates a new part:

```alda
violin:
  o4 g2 a | b2 > c

cello:
  o2 g2 d | g2 e
```

### Part Groups

Group instruments to share code:

```alda
violin/viola/cello "strings":
  o3 c1/e/g
```

## REPL Commands

### General Commands

| Command | Description |
|---------|-------------|
| `:help` (`:h`) | Show help |
| `:quit` (`:q`) | Exit REPL |
| `:stop` (`:s`) | Stop playback |

### MIDI Commands

| Command | Description |
|---------|-------------|
| `:midi` (`:m`) | Show current MIDI port |
| `:l [N]` | List MIDI ports or set port N |
| `:panic` | All notes off |
| `:virtual [NAME]` | Create virtual MIDI port |

### Audio Commands

| Command | Description |
|---------|-------------|
| `:sf PATH` | Load soundfont and enable built-in synth |
| `:synth` | Switch to built-in synth |
| `:midi` | Switch to MIDI output |
| `:presets` | List soundfont presets |

### Csound Commands (if built with Csound)

| Command | Description |
|---------|-------------|
| `:cs PATH` | Load CSD file and enable Csound |
| `:csound` | Enable Csound backend |
| `:cs-disable` | Disable Csound |
| `:cs-status` | Show Csound status |

### Ableton Link Commands

| Command | Description |
|---------|-------------|
| `:link [on\|off]` | Enable/disable Ableton Link |
| `:link-tempo BPM` | Set Link tempo |
| `:link-status` | Show Link status |

### Alda-Specific Commands

| Command | Description |
|---------|-------------|
| `:sequential` | Wait for each input to complete |
| `:concurrent` | Enable polyphonic playback (default) |
| `:play FILE` | Load and play an Alda file |
| `:export FILE` | Export to MIDI file |

## Editor Integration

Alda files (`.alda`) can be opened in the psnd editor:

```bash
psnd song.alda
```

### Keybindings

| Key | Mode | Action |
|-----|------|--------|
| `Ctrl-E` | Normal | Play current part (or selection) |
| `Ctrl-P` | Normal | Play entire file |
| `Ctrl-G` | Normal | Stop playback |
| `Ctrl-S` | Normal | Save file |

### Lua API

The editor exposes Alda functions via Lua:

```lua
-- Initialize Alda
loki.alda.init()

-- Evaluate Alda code synchronously
loki.alda.eval_sync("piano: c d e f g")

-- Evaluate asynchronously with callback
loki.alda.eval("piano: c d e f g", "my_callback")

-- Stop all playback
loki.alda.stop_all()

-- Check playback state
if loki.alda.is_playing() then
    print("Playing")
end

-- Tempo control
loki.alda.set_tempo(140)
local bpm = loki.alda.get_tempo()

-- Load soundfont for built-in synth
loki.alda.load_soundfont("/path/to/gm.sf2")
loki.alda.set_synth(true)

-- Backend selection
loki.alda.set_backend("tsf")     -- TinySoundFont
loki.alda.set_backend("csound")  -- Csound (if available)
loki.alda.set_backend("midi")    -- External MIDI
```

## Example Programs

### Hello World

```alda
piano: c8 d e f g f e d c2.
```

### Twinkle Twinkle Little Star

```alda
piano:
  (tempo 100)
  o4
  c4 c g g | a a g2 |
  f4 f e e | d d c2 |
  g4 g f f | e e d2 |
  g4 g f f | e e d2 |
  c4 c g g | a a g2 |
  f4 f e e | d d c2
```

### Chord Progression (I-V-vi-IV)

```alda
piano:
  (tempo 80)
  o3
  c1/e/g |      # C major
  g/b/>d |      # G major
  <a/>c/e |     # A minor
  <f/a/>c       # F major
```

### Duet

```alda
(tempo! 100)

violin:
  o4 (mf)
  g4 a b > c | d2 e | d4 c < b a | g1

cello:
  o2 (mf)
  g2 d | g2 c | d2 g | g1
```

### Polyphonic Piano

```alda
piano:
  (tempo 80)
  o4

  V1: c1 | d | e | f
  V2: e1 | f | g | a
  V3: g1 | a | b | > c
  V0:

  < c1/e/g
```

### Dynamic Expression

```alda
piano:
  (tempo 90)
  o4
  (pp) c8 d e f
  (p) g a b > c
  (mp) d e f g
  (mf) a b > c d
  (f) e f g a
  (ff) b > c d e
  (fff) f g a b
  (ff) > c1
```

## MIDI Export

Export Alda compositions to Standard MIDI Files:

```text
alda> piano: c d e f g a b > c
alda> :export melody.mid
Exported 8 events to melody.mid
```

Or via Lua:

```lua
loki.alda.eval_sync("piano: c d e f g")
loki.midi.export("output.mid")
```

## File Format

Alda files (`.alda`) contain Alda notation:

```alda
# song.alda - A simple composition

(tempo! 120)

piano:
  o4
  (mf)
  c4 d e f | g a b > c |
  < b a g f | e d c2

violin:
  o5
  (p)
  r1 | r |
  g4 f e d | c2.
```

Run a file directly:

```bash
psnd play song.alda
```

Or load in the REPL:

```text
alda> :play song.alda
```

## More Examples

See [examples.md](examples.md) for additional Alda examples including:
- Chord progressions
- Dynamics and expression
- Multiple instruments
- Voices and polyphony
- Repeats and patterns
- Classical pieces

The `alda-language/` directory contains reference documentation from the official Alda project.

## Attribution

Alda was created by [Dave Yarwood](https://github.com/daveyarwood). See [alda.io](https://alda.io) for the official project.
