/**
 * @file joy_midi_backend.c
 * @brief MIDI backend wrapper for Joy language.
 *
 * Provides the MIDI interface that Joy's primitives expect.
 * Uses libremidi directly (same library as psnd's MIDI backend).
 */

#include "joy_midi_backend.h"
#include <libremidi/libremidi-c.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

/* Cross-platform sleep */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define usleep(us) Sleep((us) / 1000)
#else
#include <unistd.h>
#endif

/* ============================================================================
 * Module State
 * ============================================================================ */

#define JOY_MIDI_MAX_PORTS 64

static libremidi_midi_observer_handle* g_observer = NULL;
static libremidi_midi_out_handle* g_midi_out = NULL;
static libremidi_midi_out_port* g_out_ports[JOY_MIDI_MAX_PORTS];
static int g_out_port_count = 0;
static int g_current_channel = 1;
static int g_initialized = 0;

/* ============================================================================
 * Internal Helpers
 * ============================================================================ */

static void on_output_port_found(void* user_ctx, const libremidi_midi_out_port* port) {
    (void)user_ctx;
    if (g_out_port_count >= JOY_MIDI_MAX_PORTS) return;
    libremidi_midi_out_port_clone(port, &g_out_ports[g_out_port_count]);
    g_out_port_count++;
}

static int ensure_observer(void) {
    if (g_observer != NULL) return 0;

    int ret = 0;

    /* Create observer configuration */
    libremidi_observer_configuration observer_conf;
    ret = libremidi_midi_observer_configuration_init(&observer_conf);
    if (ret != 0) {
        fprintf(stderr, "Joy MIDI: Failed to init observer config: %d\n", ret);
        return -1;
    }

    observer_conf.track_hardware = true;
    observer_conf.track_virtual = true;
    observer_conf.track_any = true;

    /* Create API configuration */
    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        fprintf(stderr, "Joy MIDI: Failed to init API config: %d\n", ret);
        return -1;
    }

    api_conf.configuration_type = Observer;
    api_conf.api = UNSPECIFIED;

    /* Create observer */
    ret = libremidi_midi_observer_new(&observer_conf, &api_conf, &g_observer);
    if (ret != 0) {
        fprintf(stderr, "Joy MIDI: Failed to create observer: %d\n", ret);
        return -1;
    }

    return 0;
}

static void enumerate_ports(void) {
    if (ensure_observer() != 0) return;

    /* Free existing ports */
    for (int i = 0; i < g_out_port_count; i++) {
        if (g_out_ports[i]) {
            libremidi_midi_out_port_free(g_out_ports[i]);
            g_out_ports[i] = NULL;
        }
    }
    g_out_port_count = 0;

    /* Enumerate */
    libremidi_midi_observer_enumerate_output_ports(g_observer, NULL, on_output_port_found);
}

/* ============================================================================
 * Public API - Initialization
 * ============================================================================ */

int joy_midi_init(void) {
    if (g_initialized) return 0;

    g_current_channel = 1;
    g_initialized = 1;

    return 0;
}

void joy_midi_cleanup(void) {
    if (!g_initialized) return;

    /* Close output */
    if (g_midi_out != NULL) {
        joy_midi_panic();
        libremidi_midi_out_free(g_midi_out);
        g_midi_out = NULL;
    }

    /* Free ports */
    for (int i = 0; i < g_out_port_count; i++) {
        if (g_out_ports[i]) {
            libremidi_midi_out_port_free(g_out_ports[i]);
            g_out_ports[i] = NULL;
        }
    }
    g_out_port_count = 0;

    /* Free observer */
    if (g_observer != NULL) {
        libremidi_midi_observer_free(g_observer);
        g_observer = NULL;
    }

    g_initialized = 0;
}

/* ============================================================================
 * Public API - Port Management
 * ============================================================================ */

void joy_midi_list_ports(void) {
    enumerate_ports();

    printf("MIDI outputs:\n");
    if (g_out_port_count == 0) {
        printf("  (none - use midi-virtual to create a virtual port)\n");
    } else {
        for (int i = 0; i < g_out_port_count; i++) {
            const char* name = NULL;
            size_t len = 0;
            if (libremidi_midi_out_port_name(g_out_ports[i], &name, &len) == 0) {
                printf("  %d: %s\n", i, name);
            }
        }
    }
}

int joy_midi_open_port(int port_idx) {
    enumerate_ports();

    if (port_idx < 0 || port_idx >= g_out_port_count) {
        fprintf(stderr, "Joy MIDI: Invalid port index: %d (have %d ports)\n",
                port_idx, g_out_port_count);
        return -1;
    }

    /* Close existing output */
    if (g_midi_out != NULL) {
        libremidi_midi_out_free(g_midi_out);
        g_midi_out = NULL;
    }

    int ret = 0;

    /* Create MIDI configuration */
    libremidi_midi_configuration midi_conf;
    ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        fprintf(stderr, "Joy MIDI: Failed to init MIDI config\n");
        return -1;
    }

    midi_conf.version = MIDI1;
    midi_conf.out_port = g_out_ports[port_idx];

    /* Create API configuration */
    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        fprintf(stderr, "Joy MIDI: Failed to init API config\n");
        return -1;
    }

    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;

    /* Open output */
    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &g_midi_out);
    if (ret != 0) {
        fprintf(stderr, "Joy MIDI: Failed to open output: %d\n", ret);
        return -1;
    }

    const char* name = NULL;
    size_t len = 0;
    libremidi_midi_out_port_name(g_out_ports[port_idx], &name, &len);
    printf("Joy MIDI: Opened port %d: %s\n", port_idx, name);

    return 0;
}

int joy_midi_open_virtual(const char* name) {
    /* Close existing output */
    if (g_midi_out != NULL) {
        libremidi_midi_out_free(g_midi_out);
        g_midi_out = NULL;
    }

    int ret = 0;

    /* Create MIDI configuration for virtual port */
    libremidi_midi_configuration midi_conf;
    ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        fprintf(stderr, "Joy MIDI: Failed to init MIDI config\n");
        return -1;
    }

    midi_conf.version = MIDI1;
    midi_conf.virtual_port = true;
    midi_conf.port_name = name ? name : "JoyMIDI";

    /* Create API configuration */
    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        fprintf(stderr, "Joy MIDI: Failed to init API config\n");
        return -1;
    }

    api_conf.configuration_type = Output;
    api_conf.api = UNSPECIFIED;

    /* Open virtual output */
    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &g_midi_out);
    if (ret != 0) {
        fprintf(stderr, "Joy MIDI: Failed to create virtual port: %d\n", ret);
        return -1;
    }

    printf("Joy MIDI: Created virtual port '%s'\n", midi_conf.port_name);
    return 0;
}

void joy_midi_close(void) {
    if (g_midi_out != NULL) {
        joy_midi_panic();
        libremidi_midi_out_free(g_midi_out);
        g_midi_out = NULL;
        printf("Joy MIDI: Port closed\n");
    }
}

int joy_midi_is_open(void) {
    return g_midi_out != NULL;
}

/* ============================================================================
 * Public API - Channel Management
 * ============================================================================ */

void joy_midi_set_channel(int channel) {
    if (channel < 1) channel = 1;
    if (channel > 16) channel = 16;
    g_current_channel = channel;
}

int joy_midi_get_channel(void) {
    return g_current_channel;
}

/* ============================================================================
 * Public API - MIDI Messages
 * ============================================================================ */

void joy_midi_note_on(int pitch, int velocity) {
    joy_midi_note_on_ch(g_current_channel, pitch, velocity);
}

void joy_midi_note_off(int pitch) {
    joy_midi_note_off_ch(g_current_channel, pitch);
}

void joy_midi_note_on_ch(int channel, int pitch, int velocity) {
    if (!g_midi_out) return;

    unsigned char msg[3];
    msg[0] = 0x90 | ((channel - 1) & 0x0F);
    msg[1] = pitch & 0x7F;
    msg[2] = velocity & 0x7F;
    libremidi_midi_out_send_message(g_midi_out, msg, 3);
}

void joy_midi_note_off_ch(int channel, int pitch) {
    if (!g_midi_out) return;

    unsigned char msg[3];
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[1] = pitch & 0x7F;
    msg[2] = 0;
    libremidi_midi_out_send_message(g_midi_out, msg, 3);
}

void joy_midi_program(int channel, int program) {
    if (!g_midi_out) return;

    unsigned char msg[2];
    msg[0] = 0xC0 | ((channel - 1) & 0x0F);
    msg[1] = program & 0x7F;
    libremidi_midi_out_send_message(g_midi_out, msg, 2);
}

void joy_midi_cc(int channel, int cc, int value) {
    if (!g_midi_out) return;

    unsigned char msg[3];
    msg[0] = 0xB0 | ((channel - 1) & 0x0F);
    msg[1] = cc & 0x7F;
    msg[2] = value & 0x7F;
    libremidi_midi_out_send_message(g_midi_out, msg, 3);
}

void joy_midi_panic(void) {
    if (!g_midi_out) return;

    /* Send all notes off on all channels */
    for (int ch = 1; ch <= 16; ch++) {
        joy_midi_cc(ch, 123, 0);  /* All Notes Off */
        joy_midi_cc(ch, 120, 0);  /* All Sound Off */
    }
}

void joy_midi_sleep_ms(int ms) {
    if (ms > 0) {
        usleep(ms * 1000);
    }
}
