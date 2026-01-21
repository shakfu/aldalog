/**
 * @file repl.c
 * @brief MHS (Micro Haskell MIDI) REPL and play mode entry points.
 *
 * Provides mhs_repl_main() and mhs_play_main() for psnd CLI dispatch.
 * These wrap the MicroHs main() with appropriate arguments for MIDI support.
 *
 * psnd embeds MHS libraries using VFS (Virtual File System). The VFS
 * intercepts file operations and serves embedded content from memory,
 * making psnd mhs fully self-contained with ~2s startup time.
 *
 * For compilation to executable (-o without .c extension), files are
 * extracted to a temp directory since cc needs real filesystem access.
 *
 * CLI flags for psnd integration:
 *   --virtual NAME   Create virtual MIDI port with given name
 *   -sf PATH         Load soundfont and use built-in synth
 *   -p N             Open MIDI port by index
 *   -l, --list       List available MIDI ports
 *   -v, --verbose    Verbose output
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _WIN32
#include <unistd.h>
#endif

#include "vfs.h"
#include "midi_ffi.h"
#include "include/psnd.h"

/* SharedContext integration for TSF/Csound/Link support */
#ifdef PSND_SHARED_CONTEXT
#include "shared/context.h"
#include "shared/audio/tsf_backend.h"

/* Global SharedContext for REPL mode */
static struct SharedContext *g_mhs_repl_shared = NULL;

/* External setter from midi_ffi.c */
extern void midi_ffi_set_shared(struct SharedContext *ctx);
#endif

/* Forward declaration of MicroHs main */
extern int mhs_main(int argc, char **argv);

/* ============================================================================
 * CLI Argument Parsing
 * ============================================================================ */

typedef struct {
    const char *virtual_name;
    const char *soundfont_path;
    int port_index;
    int list_ports;
    int verbose;
    int show_help;
    /* Remaining args for MHS */
    int mhs_argc;
    char **mhs_argv;
} MhsReplArgs;

/**
 * @brief Parse CLI arguments, separating psnd flags from MHS flags.
 */
static void parse_mhs_args(MhsReplArgs *args, int argc, char **argv) {
    memset(args, 0, sizeof(*args));
    args->port_index = -1;

    /* Allocate space for MHS args (worst case: all args go to MHS) */
    args->mhs_argv = malloc((argc + 1) * sizeof(char *));
    args->mhs_argc = 0;

    for (int i = 0; i < argc; i++) {
        /* psnd-specific flags */
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            args->show_help = 1;
        } else if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0) {
            args->verbose = 1;
        } else if (strcmp(argv[i], "-l") == 0 || strcmp(argv[i], "--list") == 0) {
            args->list_ports = 1;
        } else if (strcmp(argv[i], "--virtual") == 0 && i + 1 < argc) {
            args->virtual_name = argv[++i];
        } else if ((strcmp(argv[i], "-p") == 0 || strcmp(argv[i], "--port") == 0) &&
                   i + 1 < argc) {
            args->port_index = atoi(argv[++i]);
        } else if ((strcmp(argv[i], "-sf") == 0 || strcmp(argv[i], "--soundfont") == 0) &&
                   i + 1 < argc) {
            args->soundfont_path = argv[++i];
        } else {
            /* Pass through to MHS */
            args->mhs_argv[args->mhs_argc++] = argv[i];
        }
    }
    args->mhs_argv[args->mhs_argc] = NULL;
}

static void free_mhs_args(MhsReplArgs *args) {
    if (args->mhs_argv) {
        free(args->mhs_argv);
        args->mhs_argv = NULL;
    }
}

/* ============================================================================
 * Help and Usage
 * ============================================================================ */

/**
 * @brief Print usage information for psnd mhs.
 */
static void print_mhs_usage(void) {
    printf("psnd mhs - Micro Haskell with MIDI support\n\n");
    printf("Usage:\n");
    printf("  psnd mhs                     Start interactive REPL\n");
    printf("  psnd mhs -r <file.hs>        Run a Haskell file\n");
    printf("  psnd mhs -o<prog> <file.hs>  Compile to executable\n");
    printf("  psnd mhs -o<file.c> <file.hs> Output C code only\n");
    printf("  psnd mhs [options]           Pass options to MicroHs\n\n");
    printf("MIDI options:\n");
    printf("  --virtual NAME    Create virtual MIDI port with given name\n");
    printf("  -p N, --port N    Open MIDI port by index\n");
    printf("  -sf PATH          Load soundfont and use built-in synth\n");
    printf("  -l, --list        List available MIDI ports\n");
    printf("  -v, --verbose     Verbose output\n");
    printf("  -h, --help        Show this help\n\n");
    printf("Available MIDI modules: Midi, Music, MusicPerform, MidiPerform, Async\n\n");
    printf("Examples:\n");
    printf("  psnd mhs                     Start REPL with default virtual port\n");
    printf("  psnd mhs --virtual MyPort    Start REPL with named virtual port\n");
    printf("  psnd mhs -sf gm.sf2          Start REPL with built-in synth\n");
    printf("  psnd mhs -p 0                Start REPL using MIDI port 0\n");
    printf("  psnd mhs -r MyFile.hs        Run a Haskell file\n");
    printf("  psnd mhs -oMyProg MyFile.hs  Compile to executable\n\n");
    printf("MicroHs options: -q (quiet), -C (cache), -i<path> (include)\n");
}

/**
 * @brief List available MIDI ports.
 */
static void list_midi_ports(void) {
    if (mhs_midi_init() != 0) {
        fprintf(stderr, "Error: Failed to initialize MIDI\n");
        return;
    }

    int count = midi_list_ports();
    printf("MIDI outputs:\n");
    if (count == 0) {
        printf("  (none)\n");
    } else {
        for (int i = 0; i < count; i++) {
            printf("  %d: %s\n", i, midi_port_name(i));
        }
    }

    mhs_midi_cleanup();
}

/* ============================================================================
 * MIDI/Audio Setup
 * ============================================================================ */

#ifdef PSND_SHARED_CONTEXT
/**
 * @brief Initialize SharedContext and MIDI for REPL mode.
 */
static int setup_midi_for_repl(MhsReplArgs *args) {
    /* Allocate SharedContext */
    g_mhs_repl_shared = (struct SharedContext *)calloc(1, sizeof(struct SharedContext));
    if (!g_mhs_repl_shared) {
        fprintf(stderr, "Error: Failed to allocate shared context\n");
        return -1;
    }

    if (shared_context_init(g_mhs_repl_shared) != 0) {
        fprintf(stderr, "Error: Failed to initialize shared context\n");
        free(g_mhs_repl_shared);
        g_mhs_repl_shared = NULL;
        return -1;
    }

    /* Route midi_ffi through SharedContext for TSF/Csound/Link support */
    midi_ffi_set_shared(g_mhs_repl_shared);

    /* Initialize MIDI subsystem */
    if (mhs_midi_init() != 0) {
        fprintf(stderr, "Error: Failed to initialize MIDI\n");
        shared_context_cleanup(g_mhs_repl_shared);
        free(g_mhs_repl_shared);
        g_mhs_repl_shared = NULL;
        return -1;
    }

    /* Setup audio/MIDI output based on args */
    if (args->soundfont_path) {
        /* Use built-in synth */
        if (shared_audio_tsf_load(args->soundfont_path) != 0) {
            fprintf(stderr, "Error: Failed to load soundfont: %s\n", args->soundfont_path);
            mhs_midi_cleanup();
            shared_context_cleanup(g_mhs_repl_shared);
            free(g_mhs_repl_shared);
            g_mhs_repl_shared = NULL;
            return -1;
        }
        if (shared_audio_tsf_enable(g_mhs_repl_shared) != 0) {
            fprintf(stderr, "Error: Failed to enable built-in synth\n");
            mhs_midi_cleanup();
            shared_context_cleanup(g_mhs_repl_shared);
            free(g_mhs_repl_shared);
            g_mhs_repl_shared = NULL;
            return -1;
        }
        if (args->verbose) {
            printf("Using built-in synth: %s\n", args->soundfont_path);
        }
    } else {
        /* Setup MIDI output */
        int midi_opened = 0;

        if (args->virtual_name) {
            if (midi_open_virtual(args->virtual_name) == 0) {
                midi_opened = 1;
                if (args->verbose) {
                    printf("Created virtual MIDI output: %s\n", args->virtual_name);
                }
            }
        } else if (args->port_index >= 0) {
            if (midi_open(args->port_index) == 0) {
                midi_opened = 1;
                if (args->verbose) {
                    printf("Opened MIDI port %d\n", args->port_index);
                }
            }
        } else {
            /* Default: try to open virtual port with psnd name */
            if (midi_open_virtual(PSND_MIDI_PORT_NAME) == 0) {
                midi_opened = 1;
                if (args->verbose) {
                    printf("Created virtual MIDI output: " PSND_MIDI_PORT_NAME "\n");
                }
            }
        }

        if (!midi_opened) {
            fprintf(stderr, "Warning: No MIDI output available\n");
            fprintf(stderr, "Hint: Use -sf <soundfont.sf2> for built-in synth\n");
        }
    }

    return 0;
}

/**
 * @brief Cleanup SharedContext and MIDI for REPL mode.
 */
static void cleanup_midi_for_repl(void) {
    /* Send panic to stop all notes */
    midi_panic();

    /* Clear SharedContext routing */
    midi_ffi_set_shared(NULL);

    /* Cleanup MIDI */
    mhs_midi_cleanup();

    /* Free SharedContext */
    if (g_mhs_repl_shared) {
        shared_context_cleanup(g_mhs_repl_shared);
        free(g_mhs_repl_shared);
        g_mhs_repl_shared = NULL;
    }
}
#else
/* Stub implementations when SharedContext is not available */
static int setup_midi_for_repl(MhsReplArgs *args) {
    if (mhs_midi_init() != 0) {
        fprintf(stderr, "Error: Failed to initialize MIDI\n");
        return -1;
    }

    int midi_opened = 0;
    if (args->virtual_name) {
        midi_opened = (midi_open_virtual(args->virtual_name) == 0);
    } else if (args->port_index >= 0) {
        midi_opened = (midi_open(args->port_index) == 0);
    } else {
        midi_opened = (midi_open_virtual(PSND_MIDI_PORT_NAME) == 0);
    }

    if (!midi_opened && args->verbose) {
        fprintf(stderr, "Warning: No MIDI output available\n");
    }

    return 0;
}

static void cleanup_midi_for_repl(void) {
    midi_panic();
    mhs_midi_cleanup();
}
#endif

/* Cross-platform setenv */
static int set_env(const char *name, const char *value) {
#ifdef _WIN32
    return _putenv_s(name, value);
#else
    return setenv(name, value, 1);
#endif
}

#ifndef MHS_NO_COMPILATION
/**
 * @brief Check if we need to extract files for compilation.
 *
 * When compiling to an executable (not .c output), cc needs real files.
 *
 * @return 1 if extraction needed (executable output), 0 otherwise
 */
static int needs_extraction(int argc, char **argv) {
    for (int i = 0; i < argc; i++) {
        /* Check for -oFILE or -o FILE */
        if (strncmp(argv[i], "-o", 2) == 0) {
            const char *output = NULL;
            if (argv[i][2] != '\0') {
                /* -oFILE form */
                output = argv[i] + 2;
            } else if (i + 1 < argc) {
                /* -o FILE form */
                output = argv[i + 1];
            }
            if (output) {
                size_t len = strlen(output);
                /* Check if output ends with .c */
                if (len >= 2 && strcmp(output + len - 2, ".c") == 0) {
                    return 0;  /* C output, VFS works fine */
                }
                return 1;  /* Executable output, needs extraction */
            }
        }
    }
    return 0;  /* No -o flag, VFS works fine */
}
#endif /* MHS_NO_COMPILATION */

/* ============================================================================
 * Build MHS argv with VFS paths
 * ============================================================================ */

static char **build_mhs_argv(MhsReplArgs *args, int *out_argc, char *path_buf1,
                             char *path_buf2, size_t buf_size) {
    /* Calculate total args needed */
#ifdef MHS_USE_PKG
    int extra_args = 4;  /* -C, -a<path>, -pbase, -pmusic */
#else
    int extra_args = 3;  /* -C, -i<path>, -i<path>/lib */
#endif

    int new_argc = args->mhs_argc + extra_args;
    char **new_argv = malloc((new_argc + 1) * sizeof(char *));
    if (!new_argv) return NULL;

    int j = 0;
    new_argv[j++] = "mhs";
    new_argv[j++] = "-C";  /* Enable caching */

#ifdef MHS_USE_PKG
    snprintf(path_buf1, buf_size, "-a%s", VFS_VIRTUAL_ROOT);
    new_argv[j++] = path_buf1;
    new_argv[j++] = "-pbase";
    new_argv[j++] = "-pmusic";
#else
    snprintf(path_buf1, buf_size, "-i%s", VFS_VIRTUAL_ROOT);
    snprintf(path_buf2, buf_size, "-i%s/lib", VFS_VIRTUAL_ROOT);
    new_argv[j++] = path_buf1;
    new_argv[j++] = path_buf2;
#endif

    /* Copy user's MHS arguments (skip argv[0] which is already "mhs") */
    for (int i = 1; i < args->mhs_argc; i++) {
        new_argv[j++] = args->mhs_argv[i];
    }
    new_argv[j] = NULL;

    *out_argc = j;
    return new_argv;
}

/* ============================================================================
 * MHS REPL Main Entry Point
 * ============================================================================ */

/**
 * @brief MHS REPL entry point.
 *
 * Called when user runs: psnd mhs
 * Starts an interactive MicroHs REPL with MIDI library support.
 * Uses embedded VFS for fast startup (~2s vs ~17s from source).
 *
 * For compilation to executable (when MHS_ENABLE_COMPILATION=ON),
 * extracts embedded files to temp directory.
 */
int mhs_repl_main(int argc, char **argv) {
    /* Parse arguments */
    MhsReplArgs args;
    parse_mhs_args(&args, argc, argv);

    /* Handle --help */
    if (args.show_help) {
        print_mhs_usage();
        free_mhs_args(&args);
        return 0;
    }

    /* Handle --list */
    if (args.list_ports) {
        list_midi_ports();
        free_mhs_args(&args);
        return 0;
    }

    /* Initialize VFS with embedded libraries */
    if (vfs_init() != 0) {
        fprintf(stderr, "Error: Failed to initialize MHS Virtual File System\n");
        free_mhs_args(&args);
        return 1;
    }

    /* Setup MIDI/audio for REPL */
    if (setup_midi_for_repl(&args) != 0) {
        free_mhs_args(&args);
        return 1;
    }

#ifdef MHS_NO_COMPILATION
    /*
     * Compilation disabled - simple VFS-only path
     * No extraction needed, smaller binary without libremidi
     */

    /* Check if user is trying to compile to executable */
    for (int i = 0; i < args.mhs_argc; i++) {
        if (strncmp(args.mhs_argv[i], "-o", 2) == 0) {
            const char *output = NULL;
            if (args.mhs_argv[i][2] != '\0') {
                output = args.mhs_argv[i] + 2;
            } else if (i + 1 < args.mhs_argc) {
                output = args.mhs_argv[i + 1];
            }
            if (output) {
                size_t len = strlen(output);
                /* Check if output is NOT .c (i.e., trying to compile to executable) */
                if (len < 2 || strcmp(output + len - 2, ".c") != 0) {
                    fprintf(stderr, "Error: Compilation to executable is disabled in this build.\n");
                    fprintf(stderr, "This psnd was built with MHS_ENABLE_COMPILATION=OFF.\n");
                    fprintf(stderr, "\n");
                    fprintf(stderr, "Available options:\n");
                    fprintf(stderr, "  psnd mhs -o%s.c %s   Output C code only\n",
                            output, args.mhs_argc > i + 2 ? args.mhs_argv[i + 2] : "file.hs");
                    fprintf(stderr, "  psnd mhs -r file.hs       Run without compiling\n");
                    fprintf(stderr, "\n");
                    fprintf(stderr, "To enable compilation, rebuild psnd with:\n");
                    fprintf(stderr, "  cmake -DMHS_ENABLE_COMPILATION=ON ..\n");
                    cleanup_midi_for_repl();
                    free_mhs_args(&args);
                    return 1;
                }
            }
        }
    }

    set_env("MHSDIR", VFS_VIRTUAL_ROOT);

    char path_buf1[512], path_buf2[512];
    int new_argc;
    char **new_argv = build_mhs_argv(&args, &new_argc, path_buf1, path_buf2, sizeof(path_buf1));
    if (!new_argv) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        cleanup_midi_for_repl();
        free_mhs_args(&args);
        return 1;
    }

    int result = mhs_main(new_argc, new_argv);

    free(new_argv);
    cleanup_midi_for_repl();
    free_mhs_args(&args);
    return result;

#else /* MHS_NO_COMPILATION not defined - full compilation support */

    char *temp_dir = NULL;
    int linking_midi = 0;

    /* Check if we're compiling to an executable (cc needs real files) */
    if (needs_extraction(args.mhs_argc, args.mhs_argv)) {
        temp_dir = vfs_extract_to_temp();
        if (!temp_dir) {
            fprintf(stderr, "Error: Failed to extract embedded files for compilation\n");
            cleanup_midi_for_repl();
            free_mhs_args(&args);
            return 1;
        }
        /* Set MHSDIR to temp directory for cc to find runtime files */
        set_env("MHSDIR", temp_dir);
        linking_midi = 1;
    } else {
        /* Use VFS - set MHSDIR to virtual root */
        set_env("MHSDIR", VFS_VIRTUAL_ROOT);
    }

    /* Build argv for MHS */
    /* When linking: add -optl flags for MIDI libraries and frameworks */
#ifdef __APPLE__
    /* macOS: 3 libraries + 3 frameworks + C++ runtime = 7 -optl pairs = 14 args */
    #define LINK_EXTRA_ARGS 14
#else
    /* Linux: --no-as-needed + 3 libraries + ALSA + C++ runtime + math = 7 -optl pairs = 14 args */
    #define LINK_EXTRA_ARGS 14
#endif

#ifdef MHS_USE_PKG
    /* Package mode: mhs -C -a<path> -pbase -pmusic */
    int extra_args = 4;  /* -C, -a<path>, -pbase, -pmusic */
#else
    /* Source mode: mhs -C -i<path> -i<path>/lib */
    int extra_args = 3;  /* -C, -i<path>, -i<path>/lib */
#endif
    if (linking_midi) {
        extra_args += LINK_EXTRA_ARGS;
    }

    int new_argc = args.mhs_argc + extra_args;
    char **new_argv = malloc((new_argc + 1) * sizeof(char *));

    /* Buffers for path arguments */
    char path_arg1[512];
    char path_arg2[512];
    char lib_libremidi[512];
    char lib_midi_ffi[512];
    char lib_music_theory[512];

    if (!new_argv) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        if (temp_dir) vfs_cleanup_temp(temp_dir);
        cleanup_midi_for_repl();
        free_mhs_args(&args);
        return 1;
    }

    int j = 0;
    new_argv[j++] = "mhs";
    new_argv[j++] = "-C";  /* Enable caching */

#ifdef MHS_USE_PKG
    /* Package mode: set archive path */
    if (temp_dir) {
        snprintf(path_arg1, sizeof(path_arg1), "-a%s", temp_dir);
    } else {
        snprintf(path_arg1, sizeof(path_arg1), "-a%s", VFS_VIRTUAL_ROOT);
    }
    new_argv[j++] = path_arg1;
    new_argv[j++] = "-pbase";   /* Preload base package */
    new_argv[j++] = "-pmusic";  /* Preload music package */
#else
    /* Source mode: set include paths */
    if (temp_dir) {
        snprintf(path_arg1, sizeof(path_arg1), "-i%s", temp_dir);
        snprintf(path_arg2, sizeof(path_arg2), "-i%s/lib", temp_dir);
    } else {
        snprintf(path_arg1, sizeof(path_arg1), "-i%s", VFS_VIRTUAL_ROOT);
        snprintf(path_arg2, sizeof(path_arg2), "-i%s/lib", VFS_VIRTUAL_ROOT);
    }
    new_argv[j++] = path_arg1;
    new_argv[j++] = path_arg2;
#endif

    /* Add linker flags for MIDI libraries if compiling to executable */
    if (linking_midi) {
        /* Build library paths from temp directory */
        snprintf(lib_midi_ffi, sizeof(lib_midi_ffi), "%s/lib/libmidi_ffi.a", temp_dir);
        snprintf(lib_music_theory, sizeof(lib_music_theory), "%s/lib/libmusic_theory.a", temp_dir);
        snprintf(lib_libremidi, sizeof(lib_libremidi), "%s/lib/liblibremidi.a", temp_dir);

        /* Add -optl flags for each library */
        new_argv[j++] = "-optl";
        new_argv[j++] = lib_midi_ffi;
        new_argv[j++] = "-optl";
        new_argv[j++] = lib_music_theory;
        new_argv[j++] = "-optl";
        new_argv[j++] = lib_libremidi;

#ifdef __APPLE__
        /* macOS frameworks */
        new_argv[j++] = "-optl";
        new_argv[j++] = "-framework";
        new_argv[j++] = "-optl";
        new_argv[j++] = "CoreMIDI";
        new_argv[j++] = "-optl";
        new_argv[j++] = "-framework";
        new_argv[j++] = "-optl";
        new_argv[j++] = "CoreFoundation";
        new_argv[j++] = "-optl";
        new_argv[j++] = "-framework";
        new_argv[j++] = "-optl";
        new_argv[j++] = "CoreAudio";
        /* C++ standard library (libremidi is C++) */
        new_argv[j++] = "-optl";
        new_argv[j++] = "-lc++";
#else
        /* Linux: Force linker to include libraries */
        new_argv[j++] = "-optl";
        new_argv[j++] = "-Wl,--no-as-needed";
        /* Linux: ALSA */
        new_argv[j++] = "-optl";
        new_argv[j++] = "-lasound";
        /* C++ standard library */
        new_argv[j++] = "-optl";
        new_argv[j++] = "-lstdc++";
        /* Math library */
        new_argv[j++] = "-optl";
        new_argv[j++] = "-lm";
#endif
    }

    /* Copy user's MHS arguments (skip argv[0]) */
    for (int i = 1; i < args.mhs_argc; i++) {
        new_argv[j++] = args.mhs_argv[i];
    }
    new_argv[j] = NULL;

    /* Update argc to match */
    new_argc = j;

    /* Run MHS */
    int result = mhs_main(new_argc, new_argv);

    free(new_argv);

    /* Clean up temp directory if we extracted */
    if (temp_dir) {
        vfs_cleanup_temp(temp_dir);
    }

    cleanup_midi_for_repl();
    free_mhs_args(&args);

    return result;
#endif /* MHS_NO_COMPILATION */
}

/* ============================================================================
 * MHS Play Main Entry Point
 * ============================================================================ */

/**
 * @brief MHS play entry point.
 *
 * Called when user runs: psnd play file.hs
 * Runs the specified Haskell file.
 * Uses embedded VFS for fast startup.
 */
int mhs_play_main(int argc, char **argv) {
    if (argc < 1) {
        fprintf(stderr, "Usage: psnd play [-v] [-sf soundfont.sf2] <file.hs>\n");
        return 1;
    }

    /* Parse arguments for play mode */
    MhsReplArgs args;
    memset(&args, 0, sizeof(args));
    args.port_index = -1;

    const char *input_file = NULL;

    for (int i = 0; i < argc; i++) {
        if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0) {
            args.verbose = 1;
        } else if ((strcmp(argv[i], "-sf") == 0 || strcmp(argv[i], "--soundfont") == 0) &&
                   i + 1 < argc) {
            args.soundfont_path = argv[++i];
        } else if (strcmp(argv[i], "--virtual") == 0 && i + 1 < argc) {
            args.virtual_name = argv[++i];
        } else if (argv[i][0] != '-' && input_file == NULL) {
            input_file = argv[i];
        }
    }

    if (!input_file) {
        fprintf(stderr, "Usage: psnd play [-v] [-sf soundfont.sf2] <file.hs>\n");
        return 1;
    }

    /* Initialize VFS */
    if (vfs_init() != 0) {
        fprintf(stderr, "Error: Failed to initialize MHS Virtual File System\n");
        return 1;
    }

    /* Setup MIDI/audio */
    if (setup_midi_for_repl(&args) != 0) {
        return 1;
    }

    /* Set MHSDIR to VFS virtual root */
    set_env("MHSDIR", VFS_VIRTUAL_ROOT);

    /* Build argv for MHS run */
#ifdef MHS_USE_PKG
    /* Package mode: mhs -C -a<path> -pbase -pmusic -r <file> */
    int extra_args = 5;  /* -C, -a<path>, -pbase, -pmusic, -r */
#else
    /* Source mode: mhs -C -i<path> -i<path>/lib -r <file> */
    int extra_args = 4;  /* -C, -i<path>, -i<path>/lib, -r */
#endif
    int new_argc = 1 + extra_args + 1;  /* mhs + extra + file */
    char **new_argv = malloc((new_argc + 1) * sizeof(char *));
    char path_arg1[512];
    char path_arg2[512];

    if (!new_argv) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        cleanup_midi_for_repl();
        return 1;
    }

    int j = 0;
    new_argv[j++] = "mhs";
    new_argv[j++] = "-C";               /* Enable caching */

#ifdef MHS_USE_PKG
    snprintf(path_arg1, sizeof(path_arg1), "-a%s", VFS_VIRTUAL_ROOT);
    new_argv[j++] = path_arg1;
    new_argv[j++] = "-pbase";           /* Preload base package */
    new_argv[j++] = "-pmusic";          /* Preload music package */
#else
    snprintf(path_arg1, sizeof(path_arg1), "-i%s", VFS_VIRTUAL_ROOT);
    snprintf(path_arg2, sizeof(path_arg2), "-i%s/lib", VFS_VIRTUAL_ROOT);
    new_argv[j++] = path_arg1;
    new_argv[j++] = path_arg2;
#endif
    new_argv[j++] = "-r";               /* Run mode */
    new_argv[j++] = (char *)input_file;
    new_argv[j] = NULL;

    if (args.verbose) {
        printf("Running: %s\n", input_file);
    }

    /* Run MHS */
    int result = mhs_main(j, new_argv);

    free(new_argv);
    cleanup_midi_for_repl();

    return result;
}
