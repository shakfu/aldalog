#!/usr/bin/env python3
"""
Generate a minimal music programming language for psnd using PackCC (PEG parser).

This script creates a language with:
- A PEG grammar file (.peg) for the music DSL
- CMake integration that builds packcc and generates the parser automatically
- Runtime that uses the shared backend for realtime MIDI
- REPL and editor integration
- Tests

The generated language supports:
- note C4 [duration] [velocity]   # Play a note (pitch names or MIDI numbers)
- chord C4 E4 G4 [dur:ms] [vel:v] # Play a chord
- rest <duration>                 # Rest for duration ms
- tempo <bpm>                     # Set tempo
- # comments                      # Line comments

Unlike new_lang.py (hand-written recursive descent parser), this script generates
a PackCC PEG grammar that is compiled to C. This demonstrates an alternative
approach using parser generators.

Workflow:
1. Run this script to generate the language skeleton
2. Edit the .peg grammar file to experiment with syntax
3. Run 'make' - CMake automatically rebuilds packcc and regenerates the parser
4. Test your changes

The packcc tool is bundled in thirdparty/packcc-2.2.0 and built automatically.
"""

import argparse
import re
import sys
from pathlib import Path
from typing import List


def get_project_root() -> Path:
    """Find the project root (directory containing CMakeLists.txt)."""
    script_dir = Path(__file__).resolve().parent
    root = script_dir.parent
    if not (root / "CMakeLists.txt").exists():
        root = root.parent
    if not (root / "CMakeLists.txt").exists():
        raise RuntimeError("Could not find project root (no CMakeLists.txt found)")
    return root


def to_upper(name: str) -> str:
    return name.upper()


def to_title(name: str) -> str:
    return name.title()


def to_lower(name: str) -> str:
    return name.lower()


# =============================================================================
# PEG Grammar Template
# =============================================================================

PEG_GRAMMAR_TEMPLATE = '''/*
 * {Title} Language Grammar (PackCC PEG)
 *
 * A minimal music DSL with note/chord/rest/tempo commands.
 * Compile with: packcc {name}_grammar.peg
 * Generates: {name}_grammar.h, {name}_grammar.c
 */

%prefix "{name}_peg"

%value "struct {Title}AstNode*"

%auxil "struct {Title}ParseContext*"

%header {{
#ifndef {NAME}_GRAMMAR_H_INCLUDED
#define {NAME}_GRAMMAR_H_INCLUDED

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* AST Node Types */
typedef enum {{
    {NAME}_AST_NOTE,
    {NAME}_AST_CHORD,
    {NAME}_AST_REST,
    {NAME}_AST_TEMPO,
    {NAME}_AST_PROGRAM,    /* List of statements */
    {NAME}_AST_ERROR
}} {Title}AstNodeType;

/* AST Node */
typedef struct {Title}AstNode {{
    {Title}AstNodeType type;
    union {{
        struct {{
            int pitch;
            int duration_ms;
            int velocity;
        }} note;
        struct {{
            int pitches[16];
            int pitch_count;
            int duration_ms;
            int velocity;
        }} chord;
        struct {{
            int duration_ms;
        }} rest;
        struct {{
            int bpm;
        }} tempo;
        struct {{
            struct {Title}AstNode** statements;
            int count;
            int capacity;
        }} program;
        struct {{
            char message[128];
        }} error;
    }} data;
}} {Title}AstNode;

/* Parse context passed as auxil */
typedef struct {Title}ParseContext {{
    const char* input;
    int pos;
    char error_msg[256];
    int has_error;
}} {Title}ParseContext;

/* AST construction helpers */
static inline {Title}AstNode* {name}_ast_new({Title}AstNodeType type) {{
    {Title}AstNode* n = ({Title}AstNode*)calloc(1, sizeof({Title}AstNode));
    if (n) n->type = type;
    return n;
}}

static inline void {name}_ast_free({Title}AstNode* node) {{
    if (!node) return;
    if (node->type == {NAME}_AST_PROGRAM) {{
        for (int i = 0; i < node->data.program.count; i++) {{
            {name}_ast_free(node->data.program.statements[i]);
        }}
        free(node->data.program.statements);
    }}
    free(node);
}}

static inline void {name}_program_add({Title}AstNode* prog, {Title}AstNode* stmt) {{
    if (!prog || !stmt || prog->type != {NAME}_AST_PROGRAM) return;
    if (prog->data.program.count >= prog->data.program.capacity) {{
        int newcap = prog->data.program.capacity ? prog->data.program.capacity * 2 : 16;
        {Title}AstNode** new_stmts = ({Title}AstNode**)realloc(
            prog->data.program.statements,
            newcap * sizeof({Title}AstNode*)
        );
        if (!new_stmts) return;
        prog->data.program.statements = new_stmts;
        prog->data.program.capacity = newcap;
    }}
    prog->data.program.statements[prog->data.program.count++] = stmt;
}}

/* Note name to MIDI pitch conversion */
static inline int {name}_note_to_midi(const char* name, int len) {{
    if (len < 1) return -1;

    /* Base pitch: C=0, D=2, E=4, F=5, G=7, A=9, B=11 */
    int base;
    switch (toupper((unsigned char)name[0])) {{
        case 'C': base = 0; break;
        case 'D': base = 2; break;
        case 'E': base = 4; break;
        case 'F': base = 5; break;
        case 'G': base = 7; break;
        case 'A': base = 9; break;
        case 'B': base = 11; break;
        default: return -1;
    }}

    int i = 1;
    /* Accidentals */
    while (i < len && (name[i] == '#' || name[i] == 'b')) {{
        if (name[i] == '#') base++;
        else if (name[i] == 'b') base--;
        i++;
    }}

    /* Octave (default 4) */
    int octave = 4;
    if (i < len && isdigit((unsigned char)name[i])) {{
        octave = name[i] - '0';
        i++;
    }}

    return 12 * (octave + 1) + base;
}}

#endif /* {NAME}_GRAMMAR_H_INCLUDED */
}}

%source {{
/* Default values */
#define {NAME}_DEFAULT_DURATION 250
#define {NAME}_DEFAULT_VELOCITY 80
#define {NAME}_DEFAULT_TEMPO    120
}}

/* Entry point: a program is a list of statements */
program <- sp stmts:statement_list sp eof {{
    $$ = stmts;
}}

statement_list <- s:statement sp more:statement_list {{
    if (more) {{
        /* Prepend s to more's list */
        {Title}AstNode** old = more->data.program.statements;
        int oldcount = more->data.program.count;
        more->data.program.statements = ({Title}AstNode**)malloc((oldcount + 1) * sizeof({Title}AstNode*));
        more->data.program.statements[0] = s;
        for (int i = 0; i < oldcount; i++) {{
            more->data.program.statements[i + 1] = old[i];
        }}
        more->data.program.count = oldcount + 1;
        more->data.program.capacity = oldcount + 1;
        free(old);
        $$ = more;
    }} else {{
        {Title}AstNode* prog = {name}_ast_new({NAME}_AST_PROGRAM);
        {name}_program_add(prog, s);
        $$ = prog;
    }}
}} / {{
    $$ = {name}_ast_new({NAME}_AST_PROGRAM);
}}

statement <- note_stmt / chord_stmt / rest_stmt / tempo_stmt / comment / eol {{
    $$ = NULL;
}}

/* note C4 [duration] [velocity] OR note 60 [duration] [velocity] */
note_stmt <- 'note' sp+ pitch:pitch_spec dur:opt_int vel:opt_int sp* eol {{
    {Title}AstNode* n = {name}_ast_new({NAME}_AST_NOTE);
    n->data.note.pitch = pitch ? (int)(intptr_t)pitch : 60;
    n->data.note.duration_ms = dur ? (int)(intptr_t)dur : {NAME}_DEFAULT_DURATION;
    n->data.note.velocity = vel ? (int)(intptr_t)vel : {NAME}_DEFAULT_VELOCITY;
    $$ = n;
}}

/* chord C4 E4 G4 [dur:ms] [vel:v] */
chord_stmt <- 'chord' sp+ pitches:pitch_list opts:chord_opts sp* eol {{
    {Title}AstNode* n = {name}_ast_new({NAME}_AST_CHORD);
    /* pitches is encoded as: pitch_count in high bits, pitches in array */
    /* We'll decode from the pitch_list helper */
    int* pdata = (int*)pitches;
    if (pdata) {{
        n->data.chord.pitch_count = pdata[0];
        for (int i = 0; i < pdata[0] && i < 16; i++) {{
            n->data.chord.pitches[i] = pdata[i + 1];
        }}
        free(pdata);
    }}
    int* odata = (int*)opts;
    if (odata) {{
        n->data.chord.duration_ms = odata[0];
        n->data.chord.velocity = odata[1];
        free(odata);
    }} else {{
        n->data.chord.duration_ms = {NAME}_DEFAULT_DURATION;
        n->data.chord.velocity = {NAME}_DEFAULT_VELOCITY;
    }}
    $$ = n;
}}

/* rest <duration> */
rest_stmt <- 'rest' sp+ dur:integer sp* eol {{
    {Title}AstNode* n = {name}_ast_new({NAME}_AST_REST);
    n->data.rest.duration_ms = (int)(intptr_t)dur;
    $$ = n;
}}

/* tempo <bpm> */
tempo_stmt <- 'tempo' sp+ bpm:integer sp* eol {{
    {Title}AstNode* n = {name}_ast_new({NAME}_AST_TEMPO);
    n->data.tempo.bpm = (int)(intptr_t)bpm;
    $$ = n;
}}

/* Pitch: note name (C4, C#4, Db3) or MIDI number (60) */
pitch_spec <- <[A-Ga-g] [#b]* [0-9]?> {{
    int pitch = {name}_note_to_midi($1, $1e - $1s);
    $$ = (struct {Title}AstNode*)(intptr_t)pitch;
}} / n:integer {{
    $$ = n;
}}

/* Pitch list for chords */
pitch_list <- p:pitch_spec sp+ rest:pitch_list {{
    int* rdata = (int*)rest;
    int rcount = rdata ? rdata[0] : 0;
    int* result = (int*)malloc((rcount + 2) * sizeof(int));
    result[0] = rcount + 1;
    result[1] = (int)(intptr_t)p;
    for (int i = 0; i < rcount; i++) {{
        result[i + 2] = rdata[i + 1];
    }}
    if (rdata) free(rdata);
    $$ = (struct {Title}AstNode*)result;
}} / p:pitch_spec {{
    int* result = (int*)malloc(2 * sizeof(int));
    result[0] = 1;
    result[1] = (int)(intptr_t)p;
    $$ = (struct {Title}AstNode*)result;
}}

/* Chord options: dur:ms vel:v */
chord_opts <- sp+ 'dur:' d:integer rest:chord_opts {{
    int* rdata = (int*)rest;
    int* result = (int*)malloc(2 * sizeof(int));
    result[0] = (int)(intptr_t)d;
    result[1] = rdata ? rdata[1] : {NAME}_DEFAULT_VELOCITY;
    if (rdata) free(rdata);
    $$ = (struct {Title}AstNode*)result;
}} / sp+ 'vel:' v:integer rest:chord_opts {{
    int* rdata = (int*)rest;
    int* result = (int*)malloc(2 * sizeof(int));
    result[0] = rdata ? rdata[0] : {NAME}_DEFAULT_DURATION;
    result[1] = (int)(intptr_t)v;
    if (rdata) free(rdata);
    $$ = (struct {Title}AstNode*)result;
}} / {{
    $$ = NULL;
}}

/* Optional integer */
opt_int <- sp+ n:integer {{
    $$ = n;
}} / {{
    $$ = NULL;
}}

/* Integer literal */
integer <- <'-'? [0-9]+> {{
    $$ = (struct {Title}AstNode*)(intptr_t)atoi($1);
}}

/* Comment */
comment <- '#' [^\\n\\r]* eol {{
    $$ = NULL;
}}

/* Whitespace (not including newlines) */
sp <- [ \\t]*

/* Required whitespace */
sp+ <- [ \\t]+

/* End of line or input */
eol <- '\\r\\n' / '\\r' / '\\n' / !.
eof <- !.

%%
'''

# =============================================================================
# Runtime Implementation
# =============================================================================

RUNTIME_H_TEMPLATE = '''#ifndef {NAME}_RUNTIME_H
#define {NAME}_RUNTIME_H

#include "{name}_grammar.h"

/* Forward declaration */
typedef struct SharedContext SharedContext;

/**
 * {Title} runtime context.
 */
typedef struct {{
    SharedContext *shared;
    int tempo_bpm;
    int default_velocity;
    int default_duration_ms;
    char last_error[256];
}} {Title}Runtime;

/**
 * Initialize the runtime with a shared context.
 */
void {name}_runtime_init({Title}Runtime *rt, SharedContext *shared);

/**
 * Execute a parsed AST node.
 */
int {name}_runtime_exec({Title}Runtime *rt, const {Title}AstNode *node);

/**
 * Parse and evaluate a string of {name} code.
 */
int {name}_runtime_eval({Title}Runtime *rt, const char *code);

/**
 * Stop all playing notes (MIDI panic).
 */
void {name}_runtime_stop({Title}Runtime *rt);

/**
 * Get last error message, or NULL if none.
 */
const char *{name}_runtime_get_error({Title}Runtime *rt);

#endif /* {NAME}_RUNTIME_H */
'''

RUNTIME_C_TEMPLATE = '''/**
 * @file {name}_runtime.c
 * @brief {Title} language runtime - executes parsed AST from PackCC parser.
 */

#include "{name}_runtime.h"
#include "{name}_grammar.h"
#include "shared/context.h"
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#define SLEEP_MS(ms) Sleep(ms)
#else
#include <unistd.h>
#define SLEEP_MS(ms) usleep((ms) * 1000)
#endif

void {name}_runtime_init({Title}Runtime *rt, SharedContext *shared) {{
    memset(rt, 0, sizeof(*rt));
    rt->shared = shared;
    rt->tempo_bpm = 120;
    rt->default_velocity = 80;
    rt->default_duration_ms = 250;
}}

static void set_error({Title}Runtime *rt, const char *msg) {{
    if (msg) {{
        strncpy(rt->last_error, msg, sizeof(rt->last_error) - 1);
        rt->last_error[sizeof(rt->last_error) - 1] = '\\0';
    }} else {{
        rt->last_error[0] = '\\0';
    }}
}}

int {name}_runtime_exec({Title}Runtime *rt, const {Title}AstNode *node) {{
    if (!rt || !node) return -1;

    switch (node->type) {{
    case {NAME}_AST_NOTE: {{
        int pitch = node->data.note.pitch;
        int velocity = node->data.note.velocity;
        int duration = node->data.note.duration_ms;
        int channel = rt->shared ? rt->shared->default_channel : 0;

        if (rt->shared) {{
            shared_send_note_on(rt->shared, channel, pitch, velocity);
            SLEEP_MS(duration);
            shared_send_note_off(rt->shared, channel, pitch);
        }}
        break;
    }}

    case {NAME}_AST_CHORD: {{
        int velocity = node->data.chord.velocity;
        int duration = node->data.chord.duration_ms;
        int channel = rt->shared ? rt->shared->default_channel : 0;

        if (rt->shared) {{
            /* Note on for all pitches */
            for (int i = 0; i < node->data.chord.pitch_count; i++) {{
                shared_send_note_on(rt->shared, channel, node->data.chord.pitches[i], velocity);
            }}

            SLEEP_MS(duration);

            /* Note off for all pitches */
            for (int i = 0; i < node->data.chord.pitch_count; i++) {{
                shared_send_note_off(rt->shared, channel, node->data.chord.pitches[i]);
            }}
        }}
        break;
    }}

    case {NAME}_AST_REST: {{
        SLEEP_MS(node->data.rest.duration_ms);
        break;
    }}

    case {NAME}_AST_TEMPO: {{
        rt->tempo_bpm = node->data.tempo.bpm;
        if (rt->shared) {{
            rt->shared->tempo = node->data.tempo.bpm;
        }}
        break;
    }}

    case {NAME}_AST_PROGRAM: {{
        for (int i = 0; i < node->data.program.count; i++) {{
            {Title}AstNode *stmt = node->data.program.statements[i];
            if (stmt) {{
                int result = {name}_runtime_exec(rt, stmt);
                if (result != 0) return result;
            }}
        }}
        break;
    }}

    case {NAME}_AST_ERROR: {{
        set_error(rt, node->data.error.message);
        return -1;
    }}
    }}

    set_error(rt, NULL);
    return 0;
}}

int {name}_runtime_eval({Title}Runtime *rt, const char *code) {{
    if (!rt || !code) return -1;

    /* Create parse context */
    {Title}ParseContext pctx = {{0}};
    pctx.input = code;
    pctx.pos = 0;
    pctx.has_error = 0;

    /* Create parser context */
    {name}_peg_context_t *ctx = {name}_peg_create(&pctx);
    if (!ctx) {{
        set_error(rt, "Failed to create parser context");
        return -1;
    }}

    /* Parse the input */
    {Title}AstNode *ast = NULL;
    int result = {name}_peg_parse(ctx, &ast);

    if (result != 0 || !ast) {{
        if (pctx.has_error) {{
            set_error(rt, pctx.error_msg);
        }} else {{
            set_error(rt, "Parse error");
        }}
        {name}_peg_destroy(ctx);
        return -1;
    }}

    /* Execute the AST */
    int exec_result = {name}_runtime_exec(rt, ast);

    /* Cleanup */
    {name}_ast_free(ast);
    {name}_peg_destroy(ctx);

    return exec_result;
}}

void {name}_runtime_stop({Title}Runtime *rt) {{
    if (rt && rt->shared) {{
        shared_send_panic(rt->shared);
    }}
}}

const char *{name}_runtime_get_error({Title}Runtime *rt) {{
    if (!rt) return NULL;
    return rt->last_error[0] ? rt->last_error : NULL;
}}
'''

# =============================================================================
# Register/REPL/Dispatch Templates (same as new_lang.py but referencing PEG)
# =============================================================================

REGISTER_H_TEMPLATE = '''#ifndef {NAME}_REGISTER_H
#define {NAME}_REGISTER_H

/**
 * Initialize {Title} language registration with the language bridge.
 * Called from loki_lang_init() when LANG_{NAME} is defined.
 *
 * This language uses a PackCC-generated PEG parser.
 */
void {name}_loki_lang_init(void);

#endif /* {NAME}_REGISTER_H */
'''

REGISTER_C_TEMPLATE = '''/**
 * @file register.c
 * @brief {Title} language integration with Loki editor.
 *
 * Uses a PackCC-generated PEG parser for parsing.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "register.h"
#include "psnd.h"
#include "loki/internal.h"
#include "loki/lang_bridge.h"
#include "loki/lua.h"
#include "lauxlib.h"

#include "shared/context.h"
#include "shared/midi/midi.h"
#include "{name}_runtime.h"

/* ============================================================================
 * Per-Context State
 * ============================================================================ */

struct Loki{Title}State {{
    int initialized;
    SharedContext *shared;
    {Title}Runtime runtime;
    char last_error[256];
}};

static struct Loki{Title}State *get_state(editor_ctx_t *ctx) {{
    return ctx ? ctx->model.{name}_state : NULL;
}}

static void set_error(struct Loki{Title}State *state, const char *msg) {{
    if (!state) return;
    if (msg) {{
        strncpy(state->last_error, msg, sizeof(state->last_error) - 1);
        state->last_error[sizeof(state->last_error) - 1] = '\\0';
    }} else {{
        state->last_error[0] = '\\0';
    }}
}}

/* ============================================================================
 * LokiLangOps Implementation
 * ============================================================================ */

static int {name}_lang_init(editor_ctx_t *ctx) {{
    if (!ctx) return -1;

    if (ctx->model.{name}_state && ctx->model.{name}_state->initialized) {{
        return 0;
    }}

    struct Loki{Title}State *state = ctx->model.{name}_state;
    if (!state) {{
        state = calloc(1, sizeof(struct Loki{Title}State));
        if (!state) return -1;
        ctx->model.{name}_state = state;
    }}

    state->shared = malloc(sizeof(SharedContext));
    if (!state->shared || shared_context_init(state->shared) != 0) {{
        set_error(state, "Failed to initialize shared context");
        free(state);
        ctx->model.{name}_state = NULL;
        return -1;
    }}

    shared_midi_open_virtual(state->shared, PSND_MIDI_PORT_NAME "-{name}");
    {name}_runtime_init(&state->runtime, state->shared);

    state->initialized = 1;
    set_error(state, NULL);
    return 0;
}}

static void {name}_lang_cleanup(editor_ctx_t *ctx) {{
    if (!ctx) return;

    struct Loki{Title}State *state = get_state(ctx);
    if (!state) return;

    if (state->shared) {{
        shared_send_panic(state->shared);
        shared_context_cleanup(state->shared);
        free(state->shared);
    }}

    free(state);
    ctx->model.{name}_state = NULL;
}}

static int {name}_lang_is_initialized(editor_ctx_t *ctx) {{
    struct Loki{Title}State *state = get_state(ctx);
    return state ? state->initialized : 0;
}}

static int {name}_lang_eval(editor_ctx_t *ctx, const char *code) {{
    struct Loki{Title}State *state = get_state(ctx);
    if (!state || !state->initialized) {{
        if (state) set_error(state, "Not initialized");
        return -1;
    }}

    int result = {name}_runtime_eval(&state->runtime, code);
    if (result != 0) {{
        const char *err = {name}_runtime_get_error(&state->runtime);
        set_error(state, err ? err : "Evaluation failed");
        return -1;
    }}

    set_error(state, NULL);
    return 0;
}}

static void {name}_lang_stop(editor_ctx_t *ctx) {{
    struct Loki{Title}State *state = get_state(ctx);
    if (!state || !state->initialized) return;
    {name}_runtime_stop(&state->runtime);
}}

static const char *{name}_lang_get_error(editor_ctx_t *ctx) {{
    struct Loki{Title}State *state = get_state(ctx);
    if (!state) return NULL;
    return state->last_error[0] ? state->last_error : NULL;
}}

static int {name}_lang_is_playing(editor_ctx_t *ctx) {{
    (void)ctx;
    return 0;  /* Synchronous execution */
}}

/* ============================================================================
 * Lua API
 * ============================================================================ */

static int lua_{name}_init(lua_State *L) {{
    editor_ctx_t *ctx = loki_lua_get_editor_context(L);
    int result = {name}_lang_init(ctx);
    lua_pushboolean(L, result == 0);
    return 1;
}}

static int lua_{name}_eval(lua_State *L) {{
    editor_ctx_t *ctx = loki_lua_get_editor_context(L);
    const char *code = luaL_checkstring(L, 1);

    int result = {name}_lang_eval(ctx, code);
    if (result != 0) {{
        lua_pushnil(L);
        lua_pushstring(L, {name}_lang_get_error(ctx));
        return 2;
    }}

    lua_pushboolean(L, 1);
    return 1;
}}

static int lua_{name}_stop(lua_State *L) {{
    editor_ctx_t *ctx = loki_lua_get_editor_context(L);
    {name}_lang_stop(ctx);
    return 0;
}}

static void {name}_register_lua_api(lua_State *L) {{
    lua_getglobal(L, "loki");
    if (!lua_istable(L, -1)) {{
        lua_pop(L, 1);
        return;
    }}

    lua_newtable(L);

    lua_pushcfunction(L, lua_{name}_init);
    lua_setfield(L, -2, "init");

    lua_pushcfunction(L, lua_{name}_eval);
    lua_setfield(L, -2, "eval");

    lua_pushcfunction(L, lua_{name}_stop);
    lua_setfield(L, -2, "stop");

    lua_setfield(L, -2, "{name}");
    lua_pop(L, 1);
}}

/* ============================================================================
 * Language Registration
 * ============================================================================ */

static const LokiLangOps {name}_lang_ops = {{
    .name = "{name}",
    .extensions = {{{extensions_c}}},

    .init = {name}_lang_init,
    .cleanup = {name}_lang_cleanup,
    .is_initialized = {name}_lang_is_initialized,
    .check_callbacks = NULL,

    .eval = {name}_lang_eval,
    .stop = {name}_lang_stop,
    .is_playing = {name}_lang_is_playing,

    .has_events = NULL,
    .populate_shared_buffer = NULL,

    .get_error = {name}_lang_get_error,
    .configure_backend = NULL,
    .register_lua_api = {name}_register_lua_api,
}};

void {name}_loki_lang_init(void) {{
    loki_lang_register(&{name}_lang_ops);
}}
'''

REPL_H_TEMPLATE = '''#ifndef {NAME}_REPL_H
#define {NAME}_REPL_H

int {name}_repl_main(int argc, char **argv);

#endif /* {NAME}_REPL_H */
'''

REPL_C_TEMPLATE = '''/**
 * @file repl.c
 * @brief {Title} language REPL with PackCC-generated PEG parser.
 */

#include "repl.h"
#include "psnd.h"
#include "loki/core.h"
#include "loki/internal.h"
#include "loki/repl_launcher.h"
#include "shared/repl_commands.h"
#include "shared/context.h"
#include "{name}_runtime.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* ============================================================================
 * REPL State
 * ============================================================================ */

static SharedContext *g_shared_ctx = NULL;
static {Title}Runtime g_runtime;

/* ============================================================================
 * Usage and Help
 * ============================================================================ */

static void print_usage(const char *prog) {{
    printf("Usage: %s [options] [file.{ext}]\\n", prog);
    printf("\\n{Title} music language interpreter (PEG parser).\\n\\n");
    printf("Options:\\n");
    printf("  -h, --help        Show this help message\\n");
    printf("  -l, --list        List available MIDI ports\\n");
    printf("  -p, --port N      Use MIDI port N\\n");
    printf("  --virtual NAME    Create virtual MIDI port\\n");
    printf("  -sf PATH          Use built-in synth with soundfont\\n");
}}

static void print_help(void) {{
    shared_print_command_help();

    printf("{Title} Syntax (PEG-based):\\n");
    printf("  note C4 [dur] [vel]      Play a note (e.g., C4, C#4, Db3, or 60)\\n");
    printf("  chord C4 E4 G4 ...       Play a chord\\n");
    printf("    [dur:ms] [vel:v]       Optional duration/velocity\\n");
    printf("  rest <duration>          Rest for duration ms\\n");
    printf("  tempo <bpm>              Set tempo\\n");
    printf("  # comment                Line comment\\n");
    printf("\\n");
}}

/* ============================================================================
 * Command Processing
 * ============================================================================ */

static void {name}_stop_playback(void) {{
    {name}_runtime_stop(&g_runtime);
}}

static int process_command(const char *input) {{
    int result = shared_process_command(g_shared_ctx, input, {name}_stop_playback);
    if (result == REPL_CMD_QUIT) return 1;
    if (result == REPL_CMD_HANDLED) return 0;

    const char *cmd = input;
    if (cmd[0] == ':') cmd++;

    if (strcmp(cmd, "help") == 0 || strcmp(cmd, "h") == 0) {{
        print_help();
        return 0;
    }}

    return 2;  /* Interpret as code */
}}

/* ============================================================================
 * Code Evaluation
 * ============================================================================ */

static int evaluate_code(const char *code) {{
    int result = {name}_runtime_eval(&g_runtime, code);
    if (result != 0) {{
        const char *err = {name}_runtime_get_error(&g_runtime);
        if (err) {{
            fprintf(stderr, "Error: %s\\n", err);
        }}
        return -1;
    }}
    return 0;
}}

static int evaluate_file(const char *path) {{
    FILE *f = fopen(path, "r");
    if (!f) {{
        fprintf(stderr, "Error: Cannot open file: %s\\n", path);
        return -1;
    }}

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *code = malloc(size + 1);
    if (!code) {{
        fclose(f);
        fprintf(stderr, "Error: Out of memory\\n");
        return -1;
    }}

    size_t read_size = fread(code, 1, size, f);
    code[read_size] = '\\0';
    fclose(f);

    int result = evaluate_code(code);
    free(code);
    return result;
}}

/* ============================================================================
 * REPL Loop
 * ============================================================================ */

static void repl_loop(editor_ctx_t *syntax_ctx) {{
    ReplLineEditor ed;
    char *input;

    if (!isatty(STDIN_FILENO)) {{
        char line[4096];
        while (fgets(line, sizeof(line), stdin)) {{
            size_t len = strlen(line);
            while (len > 0 && (line[len-1] == '\\n' || line[len-1] == '\\r'))
                line[--len] = '\\0';
            if (len == 0) continue;

            int result = process_command(line);
            if (result == 1) break;
            if (result == 0) continue;

            evaluate_code(line);
        }}
        return;
    }}

    repl_editor_init(&ed);

    printf("{Title} REPL (PEG parser). Type :help for commands, :quit to exit.\\n");

    while ((input = repl_editor_readline(&ed, "{name}> ", syntax_ctx)) != NULL) {{
        if (input[0] == '\\0') {{
            free(input);
            continue;
        }}

        repl_editor_add_history(&ed, input);

        int result = process_command(input);
        if (result == 1) {{
            free(input);
            break;
        }}
        if (result == 0) {{
            free(input);
            continue;
        }}

        evaluate_code(input);

        free(input);
    }}

    repl_editor_cleanup(&ed);
}}

/* ============================================================================
 * Main Entry Point
 * ============================================================================ */

int {name}_repl_main(int argc, char **argv) {{
    const char *input_file = NULL;
    const char *soundfont = NULL;
    const char *virtual_name = NULL;
    int port = -1;

    for (int i = 1; i < argc; i++) {{
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {{
            print_usage(argv[0]);
            return 0;
        }}
        if (strcmp(argv[i], "-l") == 0 || strcmp(argv[i], "--list") == 0) {{
            shared_midi_list_ports();
            return 0;
        }}
        if ((strcmp(argv[i], "-p") == 0 || strcmp(argv[i], "--port") == 0) && i+1 < argc) {{
            port = atoi(argv[++i]);
            continue;
        }}
        if (strcmp(argv[i], "--virtual") == 0 && i+1 < argc) {{
            virtual_name = argv[++i];
            continue;
        }}
        if ((strcmp(argv[i], "-sf") == 0 || strcmp(argv[i], "--soundfont") == 0) && i+1 < argc) {{
            soundfont = argv[++i];
            continue;
        }}
        if (argv[i][0] != '-') {{
            input_file = argv[i];
        }}
    }}

    g_shared_ctx = malloc(sizeof(SharedContext));
    if (!g_shared_ctx || shared_context_init(g_shared_ctx) != 0) {{
        fprintf(stderr, "Failed to initialize shared context\\n");
        return 1;
    }}

    if (virtual_name) {{
        shared_midi_open_virtual(g_shared_ctx, virtual_name);
    }} else if (port >= 0) {{
        shared_midi_open_port(g_shared_ctx, port);
    }}

    if (soundfont) {{
        shared_audio_tsf_load(g_shared_ctx, soundfont);
    }}

    {name}_runtime_init(&g_runtime, g_shared_ctx);

    if (input_file) {{
        evaluate_file(input_file);
    }}

    if (!input_file || isatty(STDIN_FILENO)) {{
        editor_ctx_t syntax_ctx = {{0}};
        repl_loop(&syntax_ctx);
    }}

    {name}_runtime_stop(&g_runtime);
    shared_context_cleanup(g_shared_ctx);
    free(g_shared_ctx);

    return 0;
}}
'''

DISPATCH_C_TEMPLATE = '''/**
 * @file dispatch.c
 * @brief CLI dispatch for {Title} language.
 */

#include "repl.h"

int {name}_dispatch(int argc, char **argv) {{
    return {name}_repl_main(argc, argv);
}}
'''

# =============================================================================
# CMake Templates
# =============================================================================

CMAKE_LIBRARY_TEMPLATE = '''# {Title} language library (PEG-based parser)
include_guard(GLOBAL)

# ==============================================================================
# Build packcc tool from thirdparty (if not already available)
# ==============================================================================
if(NOT TARGET packcc_tool)
    set(PACKCC_SOURCE_DIR "${{PSND_ROOT_DIR}}/thirdparty/packcc-2.2.0")
    set(PACKCC_BINARY "${{CMAKE_BINARY_DIR}}/tools/packcc")

    add_custom_command(
        OUTPUT "${{PACKCC_BINARY}}"
        COMMAND ${{CMAKE_COMMAND}} -E make_directory "${{CMAKE_BINARY_DIR}}/tools"
        COMMAND ${{CMAKE_C_COMPILER}} -O2 -o "${{PACKCC_BINARY}}"
                "${{PACKCC_SOURCE_DIR}}/src/packcc.c"
        DEPENDS "${{PACKCC_SOURCE_DIR}}/src/packcc.c"
        COMMENT "Building packcc parser generator"
        VERBATIM
    )

    add_custom_target(packcc_tool DEPENDS "${{PACKCC_BINARY}}")
endif()

# ==============================================================================
# Generate parser from PEG grammar
# ==============================================================================
set({NAME}_PEG_FILE "${{PSND_ROOT_DIR}}/src/lang/{name}/impl/{name}_grammar.peg")
set({NAME}_GENERATED_C "${{CMAKE_BINARY_DIR}}/generated/{name}/{name}_grammar.c")
set({NAME}_GENERATED_H "${{CMAKE_BINARY_DIR}}/generated/{name}/{name}_grammar.h")

add_custom_command(
    OUTPUT "${{{NAME}_GENERATED_C}}" "${{{NAME}_GENERATED_H}}"
    COMMAND ${{CMAKE_COMMAND}} -E make_directory "${{CMAKE_BINARY_DIR}}/generated/{name}"
    COMMAND "${{CMAKE_BINARY_DIR}}/tools/packcc" -o "${{CMAKE_BINARY_DIR}}/generated/{name}/{name}_grammar" "${{{NAME}_PEG_FILE}}"
    DEPENDS "${{{NAME}_PEG_FILE}}" packcc_tool
    COMMENT "Generating {name} parser from PEG grammar"
    VERBATIM
)

add_custom_target({name}_parser_gen DEPENDS "${{{NAME}_GENERATED_C}}" "${{{NAME}_GENERATED_H}}")

# ==============================================================================
# Build the language library
# ==============================================================================
set({NAME}_SOURCES
    "${{{NAME}_GENERATED_C}}"
    "${{PSND_ROOT_DIR}}/src/lang/{name}/impl/{name}_runtime.c"
)

add_library({name} STATIC ${{{NAME}_SOURCES}})
add_dependencies({name} {name}_parser_gen)

target_include_directories({name}
    PUBLIC
        ${{PSND_ROOT_DIR}}/include
        ${{PSND_ROOT_DIR}}/src/lang/{name}/impl
        ${{CMAKE_BINARY_DIR}}/generated/{name}
    PRIVATE
        ${{PSND_ROOT_DIR}}/src
)

target_link_libraries({name} PRIVATE shared)
'''

# =============================================================================
# Test Templates
# =============================================================================

TEST_CMAKE_TEMPLATE = '''# {Title} language tests (PEG parser)

add_executable(test_{name}_parser test_parser.c)
add_dependencies(test_{name}_parser {name}_parser_gen)
target_link_libraries(test_{name}_parser PRIVATE {name} test_framework)
target_include_directories(test_{name}_parser PRIVATE
    ${{PSND_ROOT_DIR}}/src/lang/{name}/impl
    ${{CMAKE_BINARY_DIR}}/generated/{name}
    ${{PSND_ROOT_DIR}}/tests
)
add_test(NAME {name}_parser COMMAND test_{name}_parser)
set_tests_properties({name}_parser PROPERTIES LABELS "unit")
'''

TEST_PARSER_TEMPLATE = '''#include "test_framework.h"
#include "{name}_grammar.h"
#include "{name}_runtime.h"

/* Helper to parse a line and check result */
static {Title}AstNode* parse_line(const char* input) {{
    {Title}ParseContext pctx = {{0}};
    pctx.input = input;

    {name}_peg_context_t *ctx = {name}_peg_create(&pctx);
    if (!ctx) return NULL;

    {Title}AstNode *ast = NULL;
    int result = {name}_peg_parse(ctx, &ast);
    {name}_peg_destroy(ctx);

    if (result != 0) {{
        if (ast) {name}_ast_free(ast);
        return NULL;
    }}

    return ast;
}}

TEST(test_parse_note_midi_number) {{
    {Title}AstNode* ast = parse_line("note 60\\n");
    ASSERT_NOT_NULL(ast);
    ASSERT_EQ(ast->type, {NAME}_AST_PROGRAM);
    ASSERT_EQ(ast->data.program.count, 1);

    {Title}AstNode* note = ast->data.program.statements[0];
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->type, {NAME}_AST_NOTE);
    ASSERT_EQ(note->data.note.pitch, 60);

    {name}_ast_free(ast);
}}

TEST(test_parse_note_name) {{
    {Title}AstNode* ast = parse_line("note C4\\n");
    ASSERT_NOT_NULL(ast);
    ASSERT_EQ(ast->data.program.count, 1);

    {Title}AstNode* note = ast->data.program.statements[0];
    ASSERT_EQ(note->type, {NAME}_AST_NOTE);
    ASSERT_EQ(note->data.note.pitch, 60);  /* C4 = 60 */

    {name}_ast_free(ast);
}}

TEST(test_parse_note_sharp) {{
    {Title}AstNode* ast = parse_line("note C#4\\n");
    ASSERT_NOT_NULL(ast);

    {Title}AstNode* note = ast->data.program.statements[0];
    ASSERT_EQ(note->data.note.pitch, 61);  /* C#4 = 61 */

    {name}_ast_free(ast);
}}

TEST(test_parse_note_flat) {{
    {Title}AstNode* ast = parse_line("note Db4\\n");
    ASSERT_NOT_NULL(ast);

    {Title}AstNode* note = ast->data.program.statements[0];
    ASSERT_EQ(note->data.note.pitch, 61);  /* Db4 = 61 */

    {name}_ast_free(ast);
}}

TEST(test_parse_note_with_duration) {{
    {Title}AstNode* ast = parse_line("note C4 500\\n");
    ASSERT_NOT_NULL(ast);

    {Title}AstNode* note = ast->data.program.statements[0];
    ASSERT_EQ(note->data.note.pitch, 60);
    ASSERT_EQ(note->data.note.duration_ms, 500);

    {name}_ast_free(ast);
}}

TEST(test_parse_note_with_velocity) {{
    {Title}AstNode* ast = parse_line("note C4 250 100\\n");
    ASSERT_NOT_NULL(ast);

    {Title}AstNode* note = ast->data.program.statements[0];
    ASSERT_EQ(note->data.note.pitch, 60);
    ASSERT_EQ(note->data.note.duration_ms, 250);
    ASSERT_EQ(note->data.note.velocity, 100);

    {name}_ast_free(ast);
}}

TEST(test_parse_chord) {{
    {Title}AstNode* ast = parse_line("chord C4 E4 G4\\n");
    ASSERT_NOT_NULL(ast);

    {Title}AstNode* chord = ast->data.program.statements[0];
    ASSERT_EQ(chord->type, {NAME}_AST_CHORD);
    ASSERT_EQ(chord->data.chord.pitch_count, 3);
    ASSERT_EQ(chord->data.chord.pitches[0], 60);  /* C4 */
    ASSERT_EQ(chord->data.chord.pitches[1], 64);  /* E4 */
    ASSERT_EQ(chord->data.chord.pitches[2], 67);  /* G4 */

    {name}_ast_free(ast);
}}

TEST(test_parse_chord_with_opts) {{
    {Title}AstNode* ast = parse_line("chord C4 E4 G4 dur:1000 vel:90\\n");
    ASSERT_NOT_NULL(ast);

    {Title}AstNode* chord = ast->data.program.statements[0];
    ASSERT_EQ(chord->data.chord.pitch_count, 3);
    ASSERT_EQ(chord->data.chord.duration_ms, 1000);
    ASSERT_EQ(chord->data.chord.velocity, 90);

    {name}_ast_free(ast);
}}

TEST(test_parse_rest) {{
    {Title}AstNode* ast = parse_line("rest 500\\n");
    ASSERT_NOT_NULL(ast);

    {Title}AstNode* rest = ast->data.program.statements[0];
    ASSERT_EQ(rest->type, {NAME}_AST_REST);
    ASSERT_EQ(rest->data.rest.duration_ms, 500);

    {name}_ast_free(ast);
}}

TEST(test_parse_tempo) {{
    {Title}AstNode* ast = parse_line("tempo 140\\n");
    ASSERT_NOT_NULL(ast);

    {Title}AstNode* tempo = ast->data.program.statements[0];
    ASSERT_EQ(tempo->type, {NAME}_AST_TEMPO);
    ASSERT_EQ(tempo->data.tempo.bpm, 140);

    {name}_ast_free(ast);
}}

TEST(test_parse_comment) {{
    {Title}AstNode* ast = parse_line("# this is a comment\\n");
    ASSERT_NOT_NULL(ast);
    /* Comments produce empty program or NULL statement */
    {name}_ast_free(ast);
}}

TEST(test_parse_multiple_statements) {{
    {Title}AstNode* ast = parse_line("note C4\\nnote E4\\nnote G4\\n");
    ASSERT_NOT_NULL(ast);
    ASSERT_EQ(ast->type, {NAME}_AST_PROGRAM);
    ASSERT_EQ(ast->data.program.count, 3);

    {name}_ast_free(ast);
}}

BEGIN_TEST_SUITE("{Title} PEG Parser Tests")
    RUN_TEST(test_parse_note_midi_number);
    RUN_TEST(test_parse_note_name);
    RUN_TEST(test_parse_note_sharp);
    RUN_TEST(test_parse_note_flat);
    RUN_TEST(test_parse_note_with_duration);
    RUN_TEST(test_parse_note_with_velocity);
    RUN_TEST(test_parse_chord);
    RUN_TEST(test_parse_chord_with_opts);
    RUN_TEST(test_parse_rest);
    RUN_TEST(test_parse_tempo);
    RUN_TEST(test_parse_comment);
    RUN_TEST(test_parse_multiple_statements);
END_TEST_SUITE()
'''

# =============================================================================
# Documentation Templates
# =============================================================================

DOC_README_TEMPLATE = '''# {Title} Language (PEG Parser)

{Title} is a minimal music programming language for psnd, using a PackCC-generated
PEG parser. It supports note names (C4, C#4, Db3) as well as MIDI numbers.

## Quick Start

```bash
# Build (CMake automatically builds packcc and generates parser)
make clean && make test

# Start the REPL
psnd {name}

# Or with a virtual MIDI port
psnd {name} --virtual {Title}Out

# Play a file
psnd {name} song.{ext}

# With a soundfont
psnd {name} -sf /path/to/soundfont.sf2 song.{ext}
```

## Syntax

### Note

Play a single MIDI note:

```
note <pitch> [duration_ms] [velocity]
```

- `pitch`: Note name (C4, C#4, Db3) or MIDI number (60)
- `duration_ms`: Duration in milliseconds (default: 250)
- `velocity`: MIDI velocity (default: 80)

Examples:
```{name}
note C4                # Middle C, 250ms, velocity 80
note C#4 500           # C#4, 500ms
note 60 250 100        # MIDI 60, 250ms, velocity 100
note Db3 300 90        # D-flat octave 3
```

### Chord

Play multiple notes simultaneously:

```
chord <pitch1> <pitch2> ... [dur:<ms>] [vel:<velocity>]
```

Examples:
```{name}
chord C4 E4 G4                  # C major chord
chord C4 E4 G4 dur:1000         # C major, 1 second
chord C4 Eb4 G4 dur:500 vel:90  # C minor, 500ms, velocity 90
```

### Rest

Pause for a duration:

```
rest <duration_ms>
```

### Tempo

Set the tempo:

```
tempo <bpm>
```

### Comments

Lines starting with `#` are comments:

```{name}
# This is a comment
note C4  # Inline comments work too
```

## Note Names

| Note   | MIDI | Note   | MIDI |
|--------|------|--------|------|
| C4     | 60   | C#4/Db4| 61   |
| D4     | 62   | D#4/Eb4| 63   |
| E4     | 64   | F4     | 65   |
| F#4/Gb4| 66   | G4     | 67   |
| G#4/Ab4| 68   | A4     | 69   |
| A#4/Bb4| 70   | B4     | 71   |

## Example Song

```{name}
# Simple melody
tempo 120

note C4 300
note E4 300
note G4 300
note C5 600

rest 200

chord C4 E4 G4 dur:800

rest 100

note C5 300
note G4 300
note E4 300
note C4 600
```

## Modifying the Grammar

The PEG grammar is in `src/lang/{name}/impl/{name}_grammar.peg`. To iterate:

```bash
# Edit the grammar
vim src/lang/{name}/impl/{name}_grammar.peg

# Rebuild - parser regenerates automatically
make
```

CMake detects changes to the `.peg` file and regenerates the parser using the
bundled packcc from `thirdparty/packcc-2.2.0`.

## Architecture

- **{name}_grammar.peg** - PEG grammar (source of truth)
- **build/generated/{name}/{name}_grammar.h/c** - Generated parser (build artifact)
- **{name}_runtime.h/c** - AST interpreter using shared backend
- **register.c** - Loki editor integration
- **repl.c** - REPL implementation
'''

SYNTAX_LUA_TEMPLATE = '''-- {Title} language syntax highlighting
loki.register_language({{
    name = "{Title}",
    extensions = {{{extensions_lua}}},
    keywords = {{"note", "chord", "rest", "tempo"}},
    types = {{"dur", "vel"}},
    line_comment = "#",
    block_comment_start = nil,
    block_comment_end = nil,
    string_delimiters = {{'"', "'"}},
}})
'''

# =============================================================================
# File Generation
# =============================================================================

def create_file(path: Path, content: str, dry_run: bool = False) -> None:
    """Create a file with the given content."""
    if dry_run:
        print(f"  [dry-run] Would create: {path}")
        return

    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)
    print(f"  Created: {path}")


def update_file(path: Path, old: str, new: str, dry_run: bool = False) -> bool:
    """Update a file by replacing old with new."""
    if not path.exists():
        print(f"  Warning: {path} does not exist, skipping update")
        return False

    content = path.read_text()
    if old not in content:
        return False

    if dry_run:
        print(f"  [dry-run] Would update: {path}")
        return True

    new_content = content.replace(old, new)
    path.write_text(new_content)
    print(f"  Updated: {path}")
    return True


# =============================================================================
# Main Generation Logic
# =============================================================================

def generate_language(
    name: str,
    extensions: List[str],
    root: Path,
    dry_run: bool = False
) -> None:
    """Generate all files for a new PEG-based language."""

    name_lower = to_lower(name)
    name_upper = to_upper(name)
    name_title = to_title(name)

    ext_c = ", ".join(f'".{e.lstrip(".")}"' for e in extensions) + ", NULL"
    ext_lua = ", ".join(f'".{e.lstrip(".")}"' for e in extensions)
    primary_ext = extensions[0].lstrip(".")

    subs = {
        "name": name_lower,
        "Name": name_lower,
        "NAME": name_upper,
        "Title": name_title,
        "extensions_c": ext_c,
        "extensions_lua": ext_lua,
        "ext": primary_ext,
    }

    print(f"\nGenerating PEG-based language: {name_title}")
    print(f"  Extensions: {extensions}")
    print()

    # === Create new files ===
    print("Creating new files:")

    lang_dir = root / "src" / "lang" / name_lower
    impl_dir = lang_dir / "impl"

    # PEG grammar
    peg_file = impl_dir / f"{name_lower}_grammar.peg"
    create_file(peg_file, PEG_GRAMMAR_TEMPLATE.format(**subs), dry_run)

    # Runtime
    create_file(
        impl_dir / f"{name_lower}_runtime.h",
        RUNTIME_H_TEMPLATE.format(**subs),
        dry_run
    )
    create_file(
        impl_dir / f"{name_lower}_runtime.c",
        RUNTIME_C_TEMPLATE.format(**subs),
        dry_run
    )

    # Integration files
    create_file(
        lang_dir / "register.h",
        REGISTER_H_TEMPLATE.format(**subs),
        dry_run
    )
    create_file(
        lang_dir / "register.c",
        REGISTER_C_TEMPLATE.format(**subs),
        dry_run
    )
    create_file(
        lang_dir / "repl.h",
        REPL_H_TEMPLATE.format(**subs),
        dry_run
    )
    create_file(
        lang_dir / "repl.c",
        REPL_C_TEMPLATE.format(**subs),
        dry_run
    )
    create_file(
        lang_dir / "dispatch.c",
        DISPATCH_C_TEMPLATE.format(**subs),
        dry_run
    )

    # CMake
    create_file(
        root / "scripts" / "cmake" / f"psnd_{name_lower}_library.cmake",
        CMAKE_LIBRARY_TEMPLATE.format(**subs),
        dry_run
    )

    # Tests
    test_dir = root / "tests" / name_lower
    create_file(
        test_dir / "CMakeLists.txt",
        TEST_CMAKE_TEMPLATE.format(**subs),
        dry_run
    )
    create_file(
        test_dir / "test_parser.c",
        TEST_PARSER_TEMPLATE.format(**subs),
        dry_run
    )

    # Documentation
    create_file(
        root / "docs" / name_lower / "README.md",
        DOC_README_TEMPLATE.format(**subs),
        dry_run
    )

    # Syntax highlighting
    create_file(
        root / ".psnd" / "languages" / f"{name_lower}.lua",
        SYNTAX_LUA_TEMPLATE.format(**subs),
        dry_run
    )

    # === Update existing files ===
    print("\nUpdating existing files:")

    # 1. Update src/lang_config.h
    lang_config = root / "src" / "lang_config.h"
    if lang_config.exists():
        content = lang_config.read_text()

        if f"LANG_{name_upper}" in content:
            print(f"  Skipping {lang_config} (already contains LANG_{name_upper})")
        else:
            new_content = content

            helper_marker = "#define IF_LANG_BOG(x)\n#endif"
            helper_addition = f'''

#ifdef LANG_{name_upper}
#define IF_LANG_{name_upper}(x) x
#else
#define IF_LANG_{name_upper}(x)
#endif'''
            new_content = new_content.replace(helper_marker, helper_marker + helper_addition)

            fwd_marker = "IF_LANG_BOG(struct LokiBogState;)"
            fwd_addition = f"\nIF_LANG_{name_upper}(struct Loki{name_title}State;)"
            new_content = new_content.replace(fwd_marker, fwd_marker + fwd_addition)

            state_marker = "IF_LANG_BOG(struct LokiBogState *bog_state;)"
            state_addition = f" \\\n    IF_LANG_{name_upper}(struct Loki{name_title}State *{name_lower}_state;)"
            new_content = new_content.replace(state_marker, state_marker + state_addition)

            init_decl_marker = "IF_LANG_BOG(void bog_loki_lang_init(void);)"
            init_decl_addition = f"\nIF_LANG_{name_upper}(void {name_lower}_loki_lang_init(void);)"
            new_content = new_content.replace(init_decl_marker, init_decl_marker + init_decl_addition)

            init_call_marker = "IF_LANG_BOG(bog_loki_lang_init();)"
            init_call_addition = f" \\\n    IF_LANG_{name_upper}({name_lower}_loki_lang_init();)"
            new_content = new_content.replace(init_call_marker, init_call_marker + init_call_addition)

            if dry_run:
                print(f"  [dry-run] Would update: {lang_config}")
            else:
                lang_config.write_text(new_content)
                print(f"  Updated: {lang_config}")

    # 2. Update src/lang_dispatch.c
    lang_dispatch = root / "src" / "lang_dispatch.c"
    if lang_dispatch.exists():
        content = lang_dispatch.read_text()
        if f'"{name_lower}"' not in content:
            include_marker = "#ifdef LANG_BOG\n#include"
            include_addition = f'''#ifdef LANG_{name_upper}
#include "lang/{name_lower}/repl.h"
#endif

'''
            new_content = content.replace(include_marker, include_addition + include_marker)

            dispatch_marker = "    return -1;  /* Unknown language */"
            dispatch_addition = f'''#ifdef LANG_{name_upper}
    if (strcmp(lang, "{name_lower}") == 0) {{
        return {name_lower}_repl_main(argc, argv);
    }}
#endif

    '''
            new_content = new_content.replace(dispatch_marker, dispatch_addition + dispatch_marker)

            if dry_run:
                print(f"  [dry-run] Would update: {lang_dispatch}")
            else:
                lang_dispatch.write_text(new_content)
                print(f"  Updated: {lang_dispatch}")
        else:
            print(f"  Skipping {lang_dispatch} (already contains {name_lower})")

    # 3. Update CMakeLists.txt
    cmakelists = root / "CMakeLists.txt"
    if cmakelists.exists():
        content = cmakelists.read_text()
        if f"LANG_{name_upper}" not in content:
            option_marker = 'option(LANG_BOG "Include the Bog language" ON)'
            option_addition = f'\noption(LANG_{name_upper} "Include the {name_title} language (PEG)" ON)'
            new_content = content.replace(option_marker, option_marker + option_addition)

            include_marker = "if(LANG_BOG)\n    include(psnd_bog_library)\nendif()"
            include_addition = f'''

if(LANG_{name_upper})
    include(psnd_{name_lower}_library)
endif()'''
            new_content = new_content.replace(include_marker, include_marker + include_addition)

            if dry_run:
                print(f"  [dry-run] Would update: {cmakelists}")
            else:
                cmakelists.write_text(new_content)
                print(f"  Updated: {cmakelists}")
        else:
            print(f"  Skipping {cmakelists} (already contains LANG_{name_upper})")

    # 4. Update psnd_loki_library.cmake
    loki_cmake = root / "scripts" / "cmake" / "psnd_loki_library.cmake"
    if loki_cmake.exists():
        content = loki_cmake.read_text()
        if f"LANG_{name_upper}" not in content:
            src_marker = "if(LANG_BOG)\n    list(APPEND LOKI_LANG_SOURCES ${PSND_ROOT_DIR}/src/lang/bog/register.c)\nendif()"
            src_addition = f'''

if(LANG_{name_upper})
    list(APPEND LOKI_LANG_SOURCES ${{PSND_ROOT_DIR}}/src/lang/{name_lower}/register.c)
endif()'''
            new_content = content.replace(src_marker, src_marker + src_addition)

            link_marker = "if(LANG_BOG)\n    list(APPEND LOKI_PUBLIC_LIBS bog)\n    target_compile_definitions(libloki PUBLIC LANG_BOG=1)\nendif()"
            link_addition = f'''

if(LANG_{name_upper})
    list(APPEND LOKI_PUBLIC_LIBS {name_lower})
    target_compile_definitions(libloki PUBLIC LANG_{name_upper}=1)
endif()'''
            new_content = new_content.replace(link_marker, link_marker + link_addition)

            if dry_run:
                print(f"  [dry-run] Would update: {loki_cmake}")
            else:
                loki_cmake.write_text(new_content)
                print(f"  Updated: {loki_cmake}")
        else:
            print(f"  Skipping {loki_cmake} (already contains LANG_{name_upper})")

    # 5. Update psnd_psnd_binary.cmake
    psnd_cmake = root / "scripts" / "cmake" / "psnd_psnd_binary.cmake"
    if psnd_cmake.exists():
        content = psnd_cmake.read_text()
        if f"LANG_{name_upper}" not in content:
            marker = "if(LANG_BOG)\n    list(APPEND PSND_LANG_SOURCES\n        ${PSND_ROOT_DIR}/src/lang/bog/repl.c\n        ${PSND_ROOT_DIR}/src/lang/bog/dispatch.c\n    )\nendif()"
            addition = f'''

if(LANG_{name_upper})
    list(APPEND PSND_LANG_SOURCES
        ${{PSND_ROOT_DIR}}/src/lang/{name_lower}/repl.c
        ${{PSND_ROOT_DIR}}/src/lang/{name_lower}/dispatch.c
    )
endif()'''
            new_content = content.replace(marker, marker + addition)

            if dry_run:
                print(f"  [dry-run] Would update: {psnd_cmake}")
            else:
                psnd_cmake.write_text(new_content)
                print(f"  Updated: {psnd_cmake}")
        else:
            print(f"  Skipping {psnd_cmake} (already contains LANG_{name_upper})")

    # 6. Update psnd_tests.cmake
    tests_cmake = root / "scripts" / "cmake" / "psnd_tests.cmake"
    if tests_cmake.exists():
        content = tests_cmake.read_text()
        if f"tests/{name_lower}" not in content:
            marker = "if(LANG_BOG)\n    add_subdirectory(${PSND_ROOT_DIR}/tests/bog ${CMAKE_BINARY_DIR}/tests/bog)\nendif()"
            addition = f'''

if(LANG_{name_upper})
    add_subdirectory(${{PSND_ROOT_DIR}}/tests/{name_lower} ${{CMAKE_BINARY_DIR}}/tests/{name_lower})
endif()'''
            new_content = content.replace(marker, marker + addition)

            if dry_run:
                print(f"  [dry-run] Would update: {tests_cmake}")
            else:
                tests_cmake.write_text(new_content)
                print(f"  Updated: {tests_cmake}")
        else:
            print(f"  Skipping {tests_cmake} (already contains tests/{name_lower})")

    print("\nDone!")
    if not dry_run:
        print(f"\nYour PEG-based language is ready! Next steps:")
        print(f"  1. Build and test: make clean && make test")
        print(f"     (CMake will build packcc and generate the parser automatically)")
        print(f"  2. Start the REPL: ./build/psnd {name_lower}")
        print(f"  3. Try: note C4        # Play middle C")
        print(f"  4. Try: note C#4 500   # C# for 500ms")
        print(f"  5. Try: chord C4 E4 G4 # C major chord")
        print(f"")
        print(f"To modify the grammar:")
        print(f"  1. Edit: src/lang/{name_lower}/impl/{name_lower}_grammar.peg")
        print(f"  2. Run: make")
        print(f"     (Parser regenerates automatically when .peg file changes)")


def main():
    parser = argparse.ArgumentParser(
        description="Generate a PEG-based music language for psnd using PackCC",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
This script generates a language with a PackCC PEG parser, which offers:
- Declarative grammar specification (.peg file)
- Automatic parser generation (CMake builds packcc from thirdparty/)
- Support for note names (C4, C#4, Db3) and MIDI numbers
- AST-based interpretation

Examples:
  %(prog)s foo                    # Create 'foo' with extension .foo
  %(prog)s bar -e .bar .br        # Create 'bar' with extensions .bar and .br
  %(prog)s baz --dry-run          # Preview what would be created

Workflow:
  1. %(prog)s mymusic             # Generate language skeleton
  2. make clean && make test      # Build (auto-generates parser)
  3. ./build/psnd mymusic         # Start the REPL
  4. Edit src/lang/mymusic/impl/mymusic_grammar.peg
  5. make                         # Parser regenerates automatically
"""
    )

    parser.add_argument(
        "name",
        help="Language name (lowercase, e.g., 'foo')"
    )

    parser.add_argument(
        "-e", "--extensions",
        nargs="+",
        help="File extensions (default: .<name>)"
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be created without making changes"
    )

    args = parser.parse_args()

    # Validate name
    name = args.name.lower()
    if not re.match(r'^[a-z][a-z0-9]*$', name):
        print("Error: Language name must be lowercase alphanumeric starting with a letter", file=sys.stderr)
        sys.exit(1)

    extensions = args.extensions if args.extensions else [f".{name}"]

    try:
        root = get_project_root()
    except RuntimeError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

    generate_language(name, extensions, root, args.dry_run)


if __name__ == "__main__":
    main()
