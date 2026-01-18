# Joy

Joy is a concatenative (stack-based) functional programming language for music composition. Code is written in postfix notation where operations take their arguments from a stack and push results back. Joy includes over 200 built-in primitives for stack manipulation, list processing, combinators, and MIDI music generation.

## Quick Start

### Starting the REPL

```bash
# Start interactive Joy REPL
psnd joy

# With virtual MIDI port (macOS)
psnd joy --virtual JoyOut

# With built-in synth (TinySoundFont)
psnd joy -sf /path/to/soundfont.sf2

# List available MIDI ports
psnd joy -l
```

### Your First Melody

```joy
joy> c d e f g a b c5 play
```

This plays a C major scale ascending from middle C to C5.

### Playing Chords

```joy
joy> c major chord
```

This builds a C major chord and plays it.

## Core Concepts

### Stack-Based Execution

Joy uses reverse Polish notation (postfix). Values are pushed onto a stack, and operations consume and produce stack values:

```joy
joy> 3 4 +          % Push 3, push 4, add them
7
joy> 60 80 500 midi-note   % Push pitch, velocity, duration, play note
```

### Lists and Quotations

Lists use square brackets and can contain any values:

```joy
joy> [1 2 3]        % A list of numbers
joy> [c d e]        % A list of notes
joy> [dup *]        % A quotation (deferred code)
```

Quotations are code that can be passed around and executed:

```joy
joy> 5 [2 *] i      % Push 5, push quotation, execute it -> 10
```

### Defining Words

Define new words (functions) using the `define` syntax:

```joy
joy> octave-up == [12 +] ;
joy> 60 octave-up   % -> 72
```

## Note Notation

### Note Names

| Notation | Description | Example |
|----------|-------------|---------|
| `c d e f g a b` | Note names (default octave 4) | `c` = MIDI 60 |
| `c3 d4 e5` | Notes with explicit octave | `c5` = MIDI 72 |
| `c+ d+ f+` | Sharps | `c+` = C# |
| `c- d- b-` | Flats | `b-` = Bb |
| `r` | Rest (silence) | |

### Note Values

The duration of notes is controlled by the current note value:

| Command | Note Value |
|---------|------------|
| `whole` | Whole note |
| `half` | Half note |
| `quarter` | Quarter note (default) |
| `eighth` | Eighth note |
| `sixteenth` | Sixteenth note |

```joy
joy> c d e quarter play      % Play as quarter notes
joy> [c d e] eighth play     % Play as eighth notes
```

## Music Primitives

### Playback

| Primitive | Stack Effect | Description |
|-----------|-------------|-------------|
| `play` | `[notes] ->` | Play notes sequentially |
| `chord` | `[notes] ->` | Play notes simultaneously |
| `whole` | `X ->` | Play X as whole note |
| `half` | `X ->` | Play X as half note |
| `quarter` | `X ->` | Play X as quarter note |
| `eighth` | `X ->` | Play X as eighth note |
| `sixteenth` | `X ->` | Play X as sixteenth note |

### Control

| Primitive | Stack Effect | Description |
|-----------|-------------|-------------|
| `tempo` | `N ->` | Set tempo to N BPM |
| `vol` | `N ->` | Set volume (0-100) |
| `quant` | `N ->` | Set quantization (0-100) |
| `channel` | `N ->` | Set MIDI channel (1-16) |

### Chord Construction

| Primitive | Stack Effect | Description |
|-----------|-------------|-------------|
| `major` | `root -> [notes]` | Build major triad |
| `minor` | `root -> [notes]` | Build minor triad |
| `dim` | `root -> [notes]` | Build diminished triad |
| `aug` | `root -> [notes]` | Build augmented triad |
| `dom7` | `root -> [notes]` | Build dominant 7th |
| `maj7` | `root -> [notes]` | Build major 7th |
| `min7` | `root -> [notes]` | Build minor 7th |
| `transpose` | `pitch N -> pitch'` | Transpose by N semitones |

```joy
joy> c major chord           % C major chord
joy> 60 minor chord          % C minor chord
joy> g dom7 chord            % G dominant 7th
```

### Low-Level MIDI

| Primitive | Stack Effect | Description |
|-----------|-------------|-------------|
| `midi-note` | `pitch vel dur ->` | Play note (pitch, velocity, duration ms) |
| `midi-note-on` | `pitch vel ->` | Note on |
| `midi-note-off` | `pitch ->` | Note off |
| `midi-chord` | `[pitches] vel dur ->` | Play chord |
| `midi-cc` | `cc val ->` | Send control change |
| `midi-program` | `prog ->` | Send program change |
| `midi-panic` | `->` | All notes off |
| `midi-sleep` | `ms ->` | Sleep for ms milliseconds |
| `pitch` | `"name" -> N` | Convert note name to MIDI number |

### MIDI Port Management

| Primitive | Stack Effect | Description |
|-----------|-------------|-------------|
| `midi-list` | `->` | List available MIDI ports |
| `midi-virtual` | `->` | Create virtual MIDI port |
| `midi-open` | `N ->` | Open MIDI port N |
| `midi-close` | `->` | Close MIDI port |

## Stack Primitives

### Basic Stack Operations

| Primitive | Stack Effect | Description |
|-----------|-------------|-------------|
| `dup` | `X -> X X` | Duplicate top |
| `pop` | `X ->` | Remove top |
| `swap` | `X Y -> Y X` | Swap top two |
| `over` | `X Y -> X Y X` | Copy second to top |
| `dup2` | `X Y -> X Y X Y` | Duplicate top two |
| `rollup` | `X Y Z -> Z X Y` | Rotate up |
| `rolldown` | `X Y Z -> Y Z X` | Rotate down |
| `rotate` | `X Y Z -> Z Y X` | Reverse top three |
| `pick` | `... N -> ... X` | Copy Nth item to top |
| `stack` | `... -> ... [...]` | Push stack as list |

### Arithmetic

| Primitive | Stack Effect | Description |
|-----------|-------------|-------------|
| `+` | `X Y -> X+Y` | Add |
| `-` | `X Y -> X-Y` | Subtract |
| `*` | `X Y -> X*Y` | Multiply |
| `/` | `X Y -> X/Y` | Divide |
| `%` | `X Y -> X%Y` | Modulo |
| `neg` | `X -> -X` | Negate |
| `abs` | `X -> |X|` | Absolute value |
| `max` | `X Y -> max` | Maximum |
| `min` | `X Y -> min` | Minimum |

### Comparison

| Primitive | Stack Effect | Description |
|-----------|-------------|-------------|
| `=` | `X Y -> bool` | Equal |
| `!=` | `X Y -> bool` | Not equal |
| `<` | `X Y -> bool` | Less than |
| `>` | `X Y -> bool` | Greater than |
| `<=` | `X Y -> bool` | Less or equal |
| `>=` | `X Y -> bool` | Greater or equal |

### Logic

| Primitive | Stack Effect | Description |
|-----------|-------------|-------------|
| `and` | `X Y -> bool` | Logical and |
| `or` | `X Y -> bool` | Logical or |
| `not` | `X -> bool` | Logical not |

## List Primitives

| Primitive | Stack Effect | Description |
|-----------|-------------|-------------|
| `first` | `[X ...] -> X` | First element |
| `rest` | `[X ...] -> [...]` | All but first |
| `cons` | `X [L] -> [X L...]` | Prepend element |
| `concat` | `[A] [B] -> [A B]` | Concatenate lists |
| `size` | `[L] -> N` | List length |
| `at` | `[L] N -> X` | Get element at index |
| `take` | `[L] N -> [L']` | Take first N elements |
| `drop` | `[L] N -> [L']` | Drop first N elements |
| `reverse` | `[L] -> [L']` | Reverse list |
| `null` | `X -> bool` | Test if empty |

## Combinators

Combinators are higher-order operations that apply quotations:

### Basic Combinators

| Combinator | Stack Effect | Description |
|------------|-------------|-------------|
| `i` | `[P] -> ...` | Execute quotation |
| `x` | `[P] -> [P] ...` | Execute keeping quotation |
| `dip` | `X [P] -> ... X` | Execute under top |
| `dipd` | `X Y [P] -> ... X Y` | Execute under top two |

### Conditional Combinators

| Combinator | Stack Effect | Description |
|------------|-------------|-------------|
| `ifte` | `[B] [T] [E] -> ...` | If-then-else |
| `branch` | `bool [T] [E] -> ...` | Branch on boolean |
| `cond` | `[[B1 T1] [B2 T2] ... [D]]` | Multi-way conditional |

### Iteration Combinators

| Combinator | Stack Effect | Description |
|------------|-------------|-------------|
| `times` | `N [P] -> ...` | Execute P N times |
| `map` | `[L] [P] -> [L']` | Apply P to each element |
| `fold` | `[L] I [P] -> X` | Reduce list with P |
| `filter` | `[L] [P] -> [L']` | Keep elements where P is true |
| `step` | `[L] [P] -> ...` | Execute P for each element |
| `split` | `[L] [P] -> [T] [F]` | Partition by predicate |

### Recursion Combinators

| Combinator | Stack Effect | Description |
|------------|-------------|-------------|
| `linrec` | `[P] [T] [R1] [R2]` | Linear recursion |
| `binrec` | `[P] [T] [R1] [R2]` | Binary recursion |
| `tailrec` | `[P] [T] [R1]` | Tail recursion |
| `primrec` | `X [I] [C]` | Primitive recursion |

## REPL Commands

### General Commands

| Command | Description |
|---------|-------------|
| `:help` (`:h`) | Show help |
| `:quit` (`:q`) | Exit REPL |
| `:stop` (`:s`) | Stop playback |
| `.` | Print stack |

### MIDI Commands

| Command | Description |
|---------|-------------|
| `:midi` (`:m`) | Show current MIDI port |
| `:l [N]` | List MIDI ports or set port N |
| `:panic` | All notes off |

### File Commands

| Command | Description |
|---------|-------------|
| `:play FILE` | Load and execute a .joy file |

## Ableton Link Integration

Joy supports Ableton Link for tempo synchronization:

| Primitive | Description |
|-----------|-------------|
| `link-enable` | Enable Link |
| `link-disable` | Disable Link |
| `link-tempo` | Get/set Link tempo |
| `link-beat` | Get current beat position |
| `link-phase` | Get current phase |
| `link-peers` | Get number of connected peers |
| `link-status` | Print Link status |

## Csound Integration

If built with Csound support:

| Primitive | Description |
|-----------|-------------|
| `cs-load` | Load a CSD file |
| `cs-enable` | Enable Csound backend |
| `cs-disable` | Disable Csound backend |
| `cs-play` | Play a CSD file |
| `cs-status` | Print Csound status |

## Editor Integration

Joy files (`.joy`) can be opened in the psnd editor:

```bash
psnd song.joy
```

### Keybindings

| Key | Mode | Action |
|-----|------|--------|
| `Ctrl-E` | Normal | Evaluate current buffer |
| `Ctrl-S` | Normal | Stop playback |
| `Ctrl-P` | Normal | Panic (all notes off) |

### Lua API

The editor exposes Joy functions via Lua:

```lua
-- Evaluate Joy code
loki.joy.eval("c d e play")

-- Define a new word
loki.joy.define("play-c", "60 80 500 midi-note")

-- Stop playback
loki.joy.stop()
```

## Example Programs

### Simple Melody

```joy
% Play a simple melody
c d e f g a b c5 play
```

### Chord Progression

```joy
% I-IV-V-I progression in C
c major chord
f major chord
g major chord
c major chord
```

### Using Combinators

```joy
% Play an arpeggio using map
[c e g] [80 300 midi-note] map

% Transpose a melody up an octave
[c d e f g] [12 +] map play

% Repeat a pattern 4 times
4 [c e g chord] times
```

### Defining Custom Words

```joy
% Define an arpeggio word
arp == dup first swap rest first swap rest first
       swap [80 200 midi-note] dip
       swap [80 200 midi-note] dip
       80 200 midi-note ;

% Use it
c major arp
```

### Euclidean Rhythms

```joy
% Generate a Euclidean rhythm pattern
euclidean == [
    % k n -> [pattern]
    % k hits distributed over n steps
] ;

% Play kick on euclidean pattern
[1 0 1 0 1 0 1 0] [
    [1 =] [36 100 100 midi-note 100 midi-sleep]
          [100 midi-sleep]
    ifte
] step
```

### Using SEQ for Parallel Voices

```joy
% SEQ notation plays multiple voices in parallel
SEQ{
    V1: c d e f g
    V2: [c e g] chord [d f a] chord
}
```

## File Format

Joy files (`.joy`) contain Joy code:

```joy
% melody.joy
% A simple composition

120 tempo
80 vol

melody == [c d e f g a b c5] ;
harmony == [c major chord f major chord g major chord c major chord] ;

melody play
harmony
```

Run a file directly:

```bash
psnd joy melody.joy
```

Or load in the REPL:

```joy
joy> :play melody.joy
```
