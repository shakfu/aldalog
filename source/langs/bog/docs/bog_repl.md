# Bog REPL Design: Rule Management

## Problem Statement

The Bog REPL currently replaces the entire program on each evaluation. This means:

```prolog
bog> event(kick, 36, 0.9, T) :- every(T, 1.0).
ok
% kick is playing

bog> event(snare, 38, 0.8, T) :- every(T, 2.0).
ok
% only snare is playing - kick was replaced
```

For live coding, users need to:
1. **Add** new patterns incrementally
2. **Remove** specific patterns without affecting others
3. **Replace** patterns (modify existing ones)
4. **Clear** everything and start fresh
5. **View** what's currently active

## Design Options

### Option 1: Named Slots

Each rule is assigned an explicit name. Rules are managed by name.

**Syntax:**
```prolog
:def kick   event(kick, 36, 0.9, T) :- every(T, 1.0).
:def snare  event(snare, 38, 0.8, T) :- every(T, 2.0).
:def hats   event(hat, 42, 0.5, T) :- every(T, 0.25).

:undef kick              % remove kick pattern
:def kick <new rule>     % redefine kick (replaces old)
:list                    % show all defined slots
:clear                   % remove everything
```

**Pros:**
- Explicit and predictable
- Easy to reason about what's active
- Names can be meaningful (kick, verse_beat, drop_bass)
- Familiar to users of other live coding environments

**Cons:**
- Extra syntax overhead (must name everything)
- Bare rules without names won't work
- Two-tier system (named vs unnamed)

**Implementation:**
- Hash table mapping name -> rule text
- On any change, concatenate all rules and re-evaluate
- Store: `{ "kick": "event(kick, 36, 0.9, T) :- every(T, 1.0).", ... }`

---

### Option 2: Voice-Based Auto-Replace

Rules are grouped by their voice (first argument of `event/4`). New rules for a voice replace old ones for that voice.

**Syntax:**
```prolog
bog> event(kick, 36, 0.9, T) :- every(T, 1.0).
ok  [kick: 1 rule]

bog> event(snare, 38, 0.8, T) :- every(T, 2.0).
ok  [kick: 1 rule, snare: 1 rule]

bog> event(kick, 36, 0.9, T) :- every(T, 0.5).
ok  [kick: 1 rule (replaced), snare: 1 rule]

:mute kick               % silence kick but keep rule
:unmute kick             % restore kick
:drop kick               % remove kick entirely
:list                    % show voices and rule counts
```

**Pros:**
- Natural mapping to musical thinking (instruments/voices)
- Less syntax overhead than named slots
- Automatic organization

**Cons:**
- What about multiple rules for same voice? (e.g., kick on 1 AND kick on 3)
- Requires parsing to extract voice name
- Non-event rules (helper predicates) don't fit this model

**Variation - Additive per voice:**
```prolog
bog> event(kick, 36, 0.9, T) :- beat(T, 1).
ok  [kick: 1 rule]

bog> event(kick, 36, 0.7, T) :- beat(T, 3).
ok  [kick: 2 rules]    % added, not replaced

:drop kick 1             % remove first kick rule
:drop kick               % remove all kick rules
```

**Implementation:**
- Parse each rule to extract voice from `event(Voice, _, _, _)`
- Hash table: voice -> list of rules
- Need parser access to inspect AST

---

### Option 3: File-Based Workflow

The REPL is secondary. Primary workflow is editing a `.bog` file and reloading.

**Syntax:**
```prolog
bog> :load mybeat.bog    % load file (replaces current)
bog> :reload             % reload last loaded file (or :r)
bog> :edit               % open file in $EDITOR, reload on save

% REPL only for one-off tests and commands
bog> :tempo 140
bog> :stop
```

**Pros:**
- Familiar workflow (edit-save-reload)
- Full editor features (undo, copy/paste, syntax highlighting)
- Easy to save/share compositions
- No new syntax needed
- Works well with version control

**Cons:**
- Context switch between editor and REPL
- Slower iteration than pure REPL
- Requires external editor

**Implementation:**
- Track last loaded file path
- `:reload` re-reads and evaluates the file
- `:edit` spawns `$EDITOR`, watches for file save, reloads

---

### Option 4: Numbered History

Rules are numbered in order of entry. Manage by number.

**Syntax:**
```prolog
bog> event(kick, 36, 0.9, T) :- every(T, 1.0).
[1] ok

bog> event(snare, 38, 0.8, T) :- every(T, 2.0).
[2] ok

bog> event(hat, 42, 0.5, T) :- every(T, 0.25).
[3] ok

:drop 1                  % remove rule 1
:drop 1-2                % remove rules 1 and 2
:keep 3                  % keep only rule 3
:list                    % show numbered rules
:clear                   % remove all
```

**Pros:**
- Simple mental model
- No naming required
- Easy to implement

**Cons:**
- Numbers change when rules are removed (or do they?)
- Hard to remember what number 7 was
- Less semantic than names

**Implementation:**
- Array/vector of rules
- On drop, either renumber or leave gaps
- Concatenate active rules for evaluation

---

### Option 5: Pattern Groups (TidalCycles-style)

Predefined pattern slots (p1-p9 or similar). Each slot holds one pattern.

**Syntax:**
```prolog
:p1 event(kick, 36, 0.9, T) :- every(T, 1.0).
:p2 event(snare, 38, 0.8, T) :- every(T, 2.0).
:p3 event(hat, 42, 0.5, T) :- every(T, 0.25).

:p1 <new rule>           % replace p1
:hush                    % silence all (but keep rules)
:hush 1                  % silence only p1
:solo 1                  % only p1 audible
:unsolo                  % restore all
:clear 1                 % clear p1 slot
:clear                   % clear all slots
```

**Pros:**
- Very fast to type (:p1, :p2)
- Familiar to TidalCycles/SuperCollider users
- Fixed number of slots = predictable
- Solo/mute workflow built-in

**Cons:**
- Limited slots (though 9-16 is usually enough)
- Less semantic than names
- Different syntax from normal Prolog rules

**Implementation:**
- Fixed array of N slots (e.g., 9)
- Each slot: { rule_text, muted, active }
- Concatenate non-muted active slots for evaluation

---

### Option 6: Hybrid - Named with Defaults

Combine named slots with a default behavior for unnamed rules.

**Syntax:**
```prolog
% Unnamed rules go to "_" (default/scratch slot)
bog> event(kick, 36, 0.9, T) :- every(T, 1.0).
ok  [_: 1 rule]

% Named rules are persistent
:def beat1 event(snare, 38, 0.8, T) :- every(T, 2.0).
ok  [_: 1 rule, beat1: 1 rule]

% Another unnamed rule replaces the scratch slot
bog> event(hat, 42, 0.5, T) :- every(T, 0.25).
ok  [_: 1 rule (replaced), beat1: 1 rule]

:keep                    % promote scratch to auto-named slot
ok  [_1: 1 rule, beat1: 1 rule]

:undef beat1             % remove named slot
:clear                   % clear everything including scratch
```

**Pros:**
- Quick experimentation (just type rules)
- Promote good patterns to named slots
- Best of both worlds

**Cons:**
- More complex mental model
- Scratch replacement might be surprising

---

## Recommendation

For a Prolog-based live coding language, I recommend **Option 1 (Named Slots)** with some ergonomic additions:

```prolog
% Core commands
:def NAME RULE           % define/replace named rule
:undef NAME              % remove named rule
:list                    % show all rules
:clear                   % remove all rules

% Shortcuts
:d NAME RULE             % short for :def
:u NAME                  % short for :undef
:l                       % short for :list

% Muting (optional, for performance)
:mute NAME               % silence but keep
:unmute NAME             % restore
:solo NAME               % only this one
:unsolo                  % restore all

% File integration
:save FILE               % save current rules to file
:load FILE               % load file (replaces all)
:reload                  % reload last file
```

**Why Named Slots:**
1. Explicit is better than implicit for live performance
2. Names carry meaning ("verse_kick" vs "drop_kick")
3. Simple implementation (hash table + concatenation)
4. No parsing/AST inspection needed
5. Works for any rule, not just `event/4`

**Example Session:**
```prolog
bog> :def kick event(kick, 36, 0.9, T) :- every(T, 1.0).
ok

bog> :def snare event(snare, 38, 0.8, T) :- every(T, 2.0).
ok

bog> :list
  kick:  event(kick, 36, 0.9, T) :- every(T, 1.0).
  snare: event(snare, 38, 0.8, T) :- every(T, 2.0).

bog> :def kick event(kick, 36, 0.9, T) :- every(T, 0.5).
ok  [replaced]

bog> :mute snare
ok  [snare muted]

bog> :undef kick
ok

bog> :list
  snare: event(snare, 38, 0.8, T) :- every(T, 2.0).  [muted]
```

---

## Implementation Notes

### Data Structure

```c
typedef struct {
    char *name;
    char *rule_text;
    bool muted;
} BogReplSlot;

typedef struct {
    BogReplSlot *slots;
    size_t count;
    size_t capacity;
} BogReplSlots;
```

### Evaluation

On any change:
1. Collect all non-muted slot rules
2. Concatenate with newlines
3. Pass to `bog_live_evaluator_evaluate()`

```c
static char* bog_repl_build_program(BogReplSlots *slots) {
    size_t total_len = 0;
    for (size_t i = 0; i < slots->count; i++) {
        if (!slots->slots[i].muted) {
            total_len += strlen(slots->slots[i].rule_text) + 1;
        }
    }
    char *program = malloc(total_len + 1);
    // ... concatenate ...
    return program;
}
```

### File Persistence

`:save FILE` writes:
```prolog
% Bog REPL saved state
:def kick event(kick, 36, 0.9, T) :- every(T, 1.0).
:def snare event(snare, 38, 0.8, T) :- every(T, 2.0).
```

`:load FILE` reads and processes each `:def` line.

---

## Open Questions

1. **Multi-line rules?** Some rules span multiple lines. How to handle in REPL?
   - Option A: Backslash continuation
   - Option B: Detect incomplete rule (missing `.`) and prompt for more
   - Option C: Only single-line rules in REPL, use files for complex rules

2. **Helper predicates?** Rules like `helper(X) :- X > 0.` aren't events.
   - Option A: Just use `:def helper helper(X) :- X > 0.`
   - Option B: Special `:defhelper` command
   - Option C: Detect non-event rules automatically

3. **Multiple clauses per name?** Can one slot hold multiple clauses?
   - Option A: No, one clause per name (simple)
   - Option B: Yes, append with `:def+ name clause`
   - Option C: Yes, `:def` replaces all, `:add` appends

4. **Timing of transitions?** Should rule changes be quantized?
   - Current: Changes take effect on next scheduler tick
   - Alternative: Quantize to next bar/beat boundary
