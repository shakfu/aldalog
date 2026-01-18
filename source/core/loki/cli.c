/* cli.c - Command-line argument parsing implementation
 *
 * Extracts CLI parsing from loki_editor_main() for reuse by different hosts.
 */

#include "cli.h"
#include "psnd.h"
#include <stdio.h>
#include <string.h>

void editor_cli_print_version(void) {
    printf(PSND_NAME " %s\n", PSND_VERSION);
}

void editor_cli_print_usage(void) {
    printf("Usage: " PSND_NAME " [options] <filename>\n");
    printf("\nOptions:\n");
    printf("  -h, --help          Show this help message\n");
    printf("  -v, --version       Show version information\n");
    printf("  -sf PATH            Use built-in synth with soundfont (.sf2)\n");
    printf("  -cs PATH            Use Csound synthesis with .csd file\n");
    printf("  --line-numbers      Show line numbers\n");
    printf("  --word-wrap         Enable word wrap\n");
    printf("\nInteractive mode (default):\n");
    printf("  " PSND_NAME " <file.alda>           Open file in editor\n");
    printf("  " PSND_NAME " -sf gm.sf2 song.alda  Open with TinySoundFont synth\n");
    printf("  " PSND_NAME " -cs inst.csd song.alda Open with Csound synthesis\n");
    printf("\nKeybindings:\n");
    printf("  Ctrl-E    Play current part or selection\n");
    printf("  Ctrl-P    Play entire file\n");
    printf("  Ctrl-G    Stop playback\n");
    printf("  Ctrl-S    Save file\n");
    printf("  Ctrl-Q    Quit\n");
    printf("  Ctrl-F    Find\n");
    printf("  Ctrl-L    Lua console\n");
}

int editor_cli_parse(int argc, char **argv, EditorCliArgs *args) {
    if (!args) return -1;

    /* Initialize to defaults */
    memset(args, 0, sizeof(EditorCliArgs));

    for (int i = 1; i < argc; i++) {
        const char *arg = argv[i];

        /* Help flags */
        if (strcmp(arg, "--help") == 0 || strcmp(arg, "-h") == 0) {
            args->show_help = 1;
            return 0;
        }

        /* Version flags */
        if (strcmp(arg, "--version") == 0 || strcmp(arg, "-v") == 0) {
            args->show_version = 1;
            return 0;
        }

        /* Soundfont option */
        if (strcmp(arg, "-sf") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: -sf requires a path argument\n");
                return -1;
            }
            args->soundfont_path = argv[++i];
            continue;
        }

        /* Csound option */
        if (strcmp(arg, "-cs") == 0) {
            if (i + 1 >= argc) {
                fprintf(stderr, "Error: -cs requires a path argument\n");
                return -1;
            }
            args->csound_path = argv[++i];
            continue;
        }

        /* Line numbers option */
        if (strcmp(arg, "--line-numbers") == 0) {
            args->line_numbers = 1;
            continue;
        }

        /* Word wrap option */
        if (strcmp(arg, "--word-wrap") == 0) {
            args->word_wrap = 1;
            continue;
        }

        /* Unknown option */
        if (arg[0] == '-') {
            fprintf(stderr, "Error: Unknown option: %s\n", arg);
            return -1;
        }

        /* Non-option argument is the filename */
        if (args->filename == NULL) {
            args->filename = arg;
        } else {
            fprintf(stderr, "Error: Too many arguments\n");
            return -1;
        }
    }

    return 0;
}
