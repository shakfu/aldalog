# TR7

TR7 is a tiny R7RS-small Scheme interpreter integrated into psnd for music programming. It provides a complete Scheme environment with music-specific extensions for MIDI control and audio synthesis.

## Quick Start

### Starting the REPL

```bash
# Start interactive TR7 Scheme REPL
psnd tr7
# or
psnd scheme

# With virtual MIDI port (macOS)
psnd tr7 --virtual TR7Out

# With built-in synth (TinySoundFont)
psnd tr7 -sf /path/to/soundfont.sf2

# List available MIDI ports
psnd tr7 -l
```

### Your First Notes

```scheme
tr7> (play-note 60 80 500)     ; Middle C, velocity 80, 500ms
tr7> (play-chord '(60 64 67))  ; C major chord
tr7> (play-seq '(60 62 64 65 67 69 71 72))  ; C major scale
```

## Core Concepts

### R7RS-small Scheme

TR7 implements the R7RS-small standard, providing:

- First-class functions and closures
- Proper tail recursion
- Hygienic macros
- Lexical scoping
- Lists, vectors, strings, numbers
- Standard library procedures

### Music Extensions

TR7 extends Scheme with music primitives for:

- Playing notes and chords
- MIDI control messages
- Tempo and velocity control
- SoundFont synthesis

## Music Primitives

### Note Playback

| Procedure | Description |
|-----------|-------------|
| `(play-note pitch [vel] [dur])` | Play a single MIDI note |
| `(play-chord '(p1 p2 ...) [vel] [dur])` | Play notes simultaneously |
| `(play-seq '(p1 p2 ...) [vel] [dur])` | Play notes in sequence |
| `(note-on pitch [vel])` | Send note-on message |
| `(note-off pitch)` | Send note-off message |

Parameters:
- `pitch`: MIDI note number (0-127)
- `vel`: Velocity (0-127, default: current velocity)
- `dur`: Duration in milliseconds (default: based on tempo)

```scheme
; Play middle C for 500ms at velocity 80
(play-note 60 80 500)

; Play C major chord
(play-chord '(60 64 67) 80 1000)

; Play a melody
(play-seq '(60 62 64 65 67 69 71 72) 80 250)
```

### State Control

| Procedure | Description |
|-----------|-------------|
| `(set-tempo bpm)` | Set tempo (20-400 BPM) |
| `(set-octave n)` | Set default octave (0-9) |
| `(set-velocity v)` | Set default velocity (0-127) |
| `(set-channel ch)` | Set MIDI channel (0-15) |
| `(tempo)` | Get current tempo |
| `(octave)` | Get current octave |
| `(velocity)` | Get current velocity |
| `(channel)` | Get current channel |

```scheme
(set-tempo 140)      ; 140 BPM
(set-velocity 100)   ; Forte
(set-channel 0)      ; Channel 1
(set-octave 4)       ; Middle octave

; Check current settings
(display (tempo))    ; -> 140
```

### Note Name Conversion

| Procedure | Description |
|-----------|-------------|
| `(note "name" [octave])` | Convert note name to MIDI pitch |

Note names use standard notation:
- `c`, `d`, `e`, `f`, `g`, `a`, `b` - Note letters
- `#` or `+` - Sharp
- `b` or `-` - Flat
- Digit - Octave (e.g., `c4` = middle C)

```scheme
(note "c4")    ; -> 60 (middle C)
(note "c#4")   ; -> 61
(note "bb3")   ; -> 58
(note "g")     ; -> uses current octave

; Play using note names
(play-note (note "c4") 80 500)
(play-chord (list (note "c4") (note "e4") (note "g4")))
```

### MIDI Control

| Procedure | Description |
|-----------|-------------|
| `(midi-list)` | List available MIDI ports |
| `(midi-open port)` | Open MIDI port by index |
| `(midi-virtual name)` | Create virtual MIDI port |
| `(midi-panic)` | Send all notes off |
| `(program-change prog)` | Change instrument (0-127) |
| `(control-change cc val)` | Send CC message |

```scheme
; List available ports
(midi-list)

; Open port 0
(midi-open 0)

; Create virtual port
(midi-virtual "TR7Output")

; Change to piano
(program-change 0)

; Modulation wheel
(control-change 1 64)

; All notes off
(midi-panic)
```

### Audio Backend

| Procedure | Description |
|-----------|-------------|
| `(tsf-load path)` | Load SoundFont for built-in synth |
| `(sleep-ms ms)` | Sleep for milliseconds |

```scheme
; Load a SoundFont
(tsf-load "/path/to/soundfont.sf2")

; Add timing between operations
(play-note 60 80 200)
(sleep-ms 100)
(play-note 64 80 200)
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

### File Commands

| Command | Description |
|---------|-------------|
| `:play FILE` | Load and execute a Scheme file |
| `,load FILE` | Load and execute (Scheme syntax) |

## Scheme Basics

### Data Types

```scheme
; Numbers
42          ; integer
3.14        ; float
1/3         ; rational

; Booleans
#t          ; true
#f          ; false

; Characters
#\a         ; character 'a'
#\newline   ; newline character

; Strings
"hello"     ; string

; Symbols
'foo        ; symbol

; Lists
'(1 2 3)              ; quoted list
(list 1 2 3)          ; constructed list
(cons 1 '(2 3))       ; -> (1 2 3)

; Vectors
#(1 2 3)              ; vector
```

### Defining Variables and Functions

```scheme
; Define a variable
(define x 42)

; Define a function
(define (square n)
  (* n n))

; Lambda expressions
(define add1 (lambda (x) (+ x 1)))

; Local bindings
(let ((x 1) (y 2))
  (+ x y))
```

### Control Flow

```scheme
; Conditionals
(if (> x 0)
    "positive"
    "non-positive")

(cond
  ((< x 0) "negative")
  ((= x 0) "zero")
  (else "positive"))

; Logical operators
(and #t #f)   ; -> #f
(or #t #f)    ; -> #t
(not #t)      ; -> #f
```

### List Operations

```scheme
; Basic operations
(car '(1 2 3))        ; -> 1
(cdr '(1 2 3))        ; -> (2 3)
(cons 0 '(1 2 3))     ; -> (0 1 2 3)
(length '(1 2 3))     ; -> 3
(append '(1 2) '(3 4)) ; -> (1 2 3 4)
(reverse '(1 2 3))    ; -> (3 2 1)

; Higher-order functions
(map (lambda (x) (* x 2)) '(1 2 3))     ; -> (2 4 6)
(filter (lambda (x) (> x 2)) '(1 2 3 4)) ; -> (3 4)
(fold-right + 0 '(1 2 3 4))             ; -> 10
```

## Example Programs

### Simple Melody

```scheme
; Play a C major scale
(define (play-scale)
  (play-seq '(60 62 64 65 67 69 71 72) 80 200))

(play-scale)
```

### Chord Progression

```scheme
; Define chord types
(define (major-chord root)
  (list root (+ root 4) (+ root 7)))

(define (minor-chord root)
  (list root (+ root 3) (+ root 7)))

; I-IV-V-I progression in C
(define (play-progression)
  (play-chord (major-chord 60) 80 800)  ; C major
  (sleep-ms 200)
  (play-chord (major-chord 65) 80 800)  ; F major
  (sleep-ms 200)
  (play-chord (major-chord 67) 80 800)  ; G major
  (sleep-ms 200)
  (play-chord (major-chord 60) 80 800)) ; C major

(play-progression)
```

### Arpeggiator

```scheme
; Play notes of a chord one at a time
(define (arpeggiate chord velocity duration)
  (for-each
    (lambda (note)
      (play-note note velocity duration))
    chord))

; Arpeggio up and down
(define (arpeggio-up-down chord vel dur)
  (arpeggiate chord vel dur)
  (arpeggiate (reverse (cdr chord)) vel dur))

(arpeggio-up-down (major-chord 60) 80 150)
```

### Random Melody Generator

```scheme
(import (scheme random))

; Generate random melody
(define (random-melody scale length velocity duration)
  (define (pick-random lst)
    (list-ref lst (random (length lst))))
  (define (generate n)
    (if (= n 0)
        '()
        (cons (pick-random scale) (generate (- n 1)))))
  (play-seq (generate length) velocity duration))

; C pentatonic scale
(define c-pentatonic '(60 62 64 67 69 72 74 76 79))

(random-melody c-pentatonic 16 80 200)
```

### Euclidean Rhythm

```scheme
; Generate Euclidean rhythm pattern
(define (euclidean-rhythm hits steps)
  (define (bjorklund k n)
    (cond
      ((= k 0) (make-list n 0))
      ((= k n) (make-list n 1))
      (else
        (let loop ((pattern (append (make-list k '(1))
                                    (make-list (- n k) '(0)))))
          (let* ((last-elem (car (reverse pattern)))
                 (count-last (count (lambda (x) (equal? x last-elem)) pattern)))
            (if (<= count-last 1)
                (apply append pattern)
                (let* ((remainder (filter (lambda (x) (equal? x last-elem)) pattern))
                       (front (filter (lambda (x) (not (equal? x last-elem))) pattern))
                       (merged (map append front
                                   (append remainder
                                          (make-list (- (length front) (length remainder)) '())))))
                  (loop merged))))))))
  (bjorklund hits steps))

; Play kick on euclidean pattern
(define (play-euclidean hits steps tempo note)
  (let ((pattern (euclidean-rhythm hits steps))
        (step-dur (/ 60000 tempo 4)))  ; 16th note
    (for-each
      (lambda (hit)
        (when (= hit 1)
          (play-note note 100 (floor (* step-dur 0.8))))
        (sleep-ms (floor step-dur)))
      pattern)))

(play-euclidean 5 8 120 36)  ; Kick pattern
```

### Interactive Looper

```scheme
; Simple loop structure
(define (loop-n times proc)
  (when (> times 0)
    (proc)
    (loop-n (- times 1) proc)))

; Define a pattern
(define (my-pattern)
  (play-note 60 100 100)
  (sleep-ms 100)
  (play-note 64 80 100)
  (sleep-ms 100)
  (play-note 67 80 100)
  (sleep-ms 100)
  (play-note 72 100 200))

; Play pattern 4 times
(loop-n 4 my-pattern)
```

## Editor Integration

TR7/Scheme files (`.scm`) can be opened in the psnd editor:

```bash
psnd song.scm
```

### Keybindings

| Key | Mode | Action |
|-----|------|--------|
| `Ctrl-E` | Normal | Evaluate current buffer |
| `Ctrl-S` | Normal | Stop playback |
| `Ctrl-P` | Normal | Panic (all notes off) |

### Lua API

The editor exposes TR7 functions via Lua:

```lua
-- Initialize TR7
loki.tr7.init()

-- Evaluate Scheme code
loki.tr7.eval("(play-note 60 80 500)")

-- Stop playback
loki.tr7.stop()
```

## File Format

TR7/Scheme files (`.scm`) contain standard Scheme code with music extensions:

```scheme
; song.scm - A simple composition

; Setup
(set-tempo 120)
(set-velocity 80)

; Define helper functions
(define (major root)
  (list root (+ root 4) (+ root 7)))

; Play progression
(define (play-song)
  (play-chord (major 60) 80 800)
  (sleep-ms 100)
  (play-chord (major 65) 80 800)
  (sleep-ms 100)
  (play-chord (major 67) 80 800)
  (sleep-ms 100)
  (play-chord (major 60) 80 1200))

(play-song)
```

Run a file directly:

```bash
psnd tr7 song.scm
```

Or load in the REPL:

```scheme
tr7> ,load song.scm
; or
tr7> :play song.scm
```

## Standard Libraries

TR7 provides access to R7RS-small standard libraries:

```scheme
(import (scheme base))    ; Core procedures
(import (scheme read))    ; read procedure
(import (scheme write))   ; display, write, newline
(import (scheme file))    ; File I/O
(import (scheme load))    ; load procedure
(import (scheme eval))    ; eval procedure
```

## Attribution

TR7 is based on the [tr7](https://gitlab.com/jobol/tr7) Scheme interpreter by Jose Bollo, licensed under 0BSD.
