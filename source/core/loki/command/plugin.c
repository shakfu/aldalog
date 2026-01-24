/* plugin.c - Plugin preset commands
 *
 * :plugin              Show plugin info
 * :plugin presets      List all presets (opens scratch buffer)
 * :plugin preset N     Select preset by index
 * :plugin preset NAME  Select preset by name (partial match)
 */

#include "command_impl.h"
#include "shared/audio/minihost_backend.h"

#ifdef BUILD_MINIHOST_BACKEND

/* Forward declarations for buffer operations */
void editor_insert_row(editor_ctx_t *ctx, int at, char *s, size_t len);
void editor_del_row(editor_ctx_t *ctx, int at);
int buffer_create(const char *filename);
int buffer_switch(int buffer_id);
editor_ctx_t *buffer_get_current(void);
int buffer_get_current_id(void);

/* Helper: find preset by name (case-insensitive partial match) */
static int find_preset_by_name(int slot, const char *name) {
    int num_presets = shared_minihost_get_num_presets(slot);
    char preset_name[256];

    /* First pass: exact match (case-insensitive) */
    for (int i = 0; i < num_presets; i++) {
        if (shared_minihost_get_preset_name(slot, i, preset_name, sizeof(preset_name)) == 0) {
            if (strcasecmp(preset_name, name) == 0) {
                return i;
            }
        }
    }

    /* Second pass: partial match (case-insensitive) */
    for (int i = 0; i < num_presets; i++) {
        if (shared_minihost_get_preset_name(slot, i, preset_name, sizeof(preset_name)) == 0) {
            if (strcasestr(preset_name, name) != NULL) {
                return i;
            }
        }
    }

    return -1;
}

/* :plugin - Main plugin command */
int cmd_plugin(editor_ctx_t *ctx, const char *args) {
    if (!shared_minihost_is_available()) {
        editor_set_status_msg(ctx, "Plugin support not compiled in");
        return 0;
    }

    if (!shared_minihost_has_plugin(0)) {
        editor_set_status_msg(ctx, "No plugin loaded (use -pg option)");
        return 0;
    }

    /* No args: show plugin info */
    if (!args || !args[0]) {
        const char *name = shared_minihost_get_plugin_name(0);
        int num_presets = shared_minihost_get_num_presets(0);
        int current = shared_minihost_get_preset(0);
        char preset_name[256] = "";

        if (current >= 0) {
            shared_minihost_get_preset_name(0, current, preset_name, sizeof(preset_name));
        }

        editor_set_status_msg(ctx, "Plugin: %s | Preset %d/%d: %s",
            name ? name : "Unknown",
            current + 1, num_presets,
            preset_name[0] ? preset_name : "(none)");
        return 1;
    }

    /* :plugin presets - list presets in scratch buffer */
    if (strcmp(args, "presets") == 0 || strcmp(args, "list") == 0) {
        int num_presets = shared_minihost_get_num_presets(0);
        int current = shared_minihost_get_preset(0);
        const char *plugin_name = shared_minihost_get_plugin_name(0);

        if (num_presets == 0) {
            editor_set_status_msg(ctx, "No presets available");
            return 0;
        }

        /* Create a new scratch buffer for presets */
        int buf_id = buffer_create(NULL);
        if (buf_id < 0) {
            editor_set_status_msg(ctx, "Failed to create preset buffer");
            return 0;
        }

        if (buffer_switch(buf_id) != 0) {
            editor_set_status_msg(ctx, "Failed to switch to preset buffer");
            return 0;
        }

        /* Verify buffer switch worked */
        int current_id = buffer_get_current_id();
        if (current_id != buf_id) {
            editor_set_status_msg(ctx, "Buffer switch failed: expected %d, got %d", buf_id, current_id);
            return 0;
        }

        editor_ctx_t *preset_ctx = buffer_get_current();
        if (!preset_ctx) {
            editor_set_status_msg(ctx, "Failed to switch to preset buffer");
            return 0;
        }

        /* Clear initial empty row and add content */
        if (preset_ctx->model.numrows > 0) {
            editor_del_row(preset_ctx, 0);
        }

        /* Header */
        char header[256];
        snprintf(header, sizeof(header), "Plugin: %s", plugin_name ? plugin_name : "Unknown");
        editor_insert_row(preset_ctx, preset_ctx->model.numrows, header, strlen(header));

        snprintf(header, sizeof(header), "Presets: %d (current: %d)", num_presets, current + 1);
        editor_insert_row(preset_ctx, preset_ctx->model.numrows, header, strlen(header));

        editor_insert_row(preset_ctx, preset_ctx->model.numrows, "", 0);
        editor_insert_row(preset_ctx, preset_ctx->model.numrows, "Use :plugin preset <n> to select", 32);
        editor_insert_row(preset_ctx, preset_ctx->model.numrows, "", 0);

        /* List all presets */
        for (int i = 0; i < num_presets; i++) {
            char name[256];
            char line[320];
            if (shared_minihost_get_preset_name(0, i, name, sizeof(name)) == 0) {
                snprintf(line, sizeof(line), "%s%4d: %s",
                    (i == current) ? "* " : "  ",
                    i + 1, name);
            } else {
                snprintf(line, sizeof(line), "  %4d: (unnamed)", i + 1);
            }
            editor_insert_row(preset_ctx, preset_ctx->model.numrows, line, strlen(line));
        }

        /* Mark as not dirty since it's a scratch buffer */
        preset_ctx->model.dirty = 0;

        /* Position cursor at current preset */
        preset_ctx->view.cy = 5 + current;  /* Header lines + current preset index */
        if (preset_ctx->view.cy >= preset_ctx->model.numrows) {
            preset_ctx->view.cy = preset_ctx->model.numrows - 1;
        }

        /* Ensure new buffer is in NORMAL mode (command_mode_exit will set old ctx) */
        preset_ctx->view.mode = MODE_NORMAL;

        editor_set_status_msg(preset_ctx, "Presets for %s (:q to close)", plugin_name ? plugin_name : "plugin");
        return 1;
    }

    /* :plugin preset N or :plugin preset NAME */
    if (strncmp(args, "preset ", 7) == 0 || strncmp(args, "preset\t", 7) == 0) {
        const char *arg = args + 7;
        while (*arg == ' ' || *arg == '\t') arg++;

        if (!*arg) {
            editor_set_status_msg(ctx, "Usage: :plugin preset <number|name>");
            return 0;
        }

        int index = -1;

        /* Try parsing as number first */
        char *endptr;
        long num = strtol(arg, &endptr, 10);
        if (*endptr == '\0' && num > 0) {
            /* It's a number - convert from 1-based to 0-based */
            index = (int)num - 1;
        } else {
            /* Try matching by name */
            index = find_preset_by_name(0, arg);
        }

        if (index < 0 || index >= shared_minihost_get_num_presets(0)) {
            editor_set_status_msg(ctx, "Preset not found: %s", arg);
            return 0;
        }

        if (shared_minihost_set_preset(0, index) == 0) {
            char name[256];
            shared_minihost_get_preset_name(0, index, name, sizeof(name));
            editor_set_status_msg(ctx, "Preset %d: %s", index + 1, name);
            return 1;
        } else {
            editor_set_status_msg(ctx, "Failed to set preset");
            return 0;
        }
    }

    /* Unknown subcommand */
    editor_set_status_msg(ctx, "Usage: :plugin [presets|preset <n>]");
    return 0;
}

#else /* BUILD_MINIHOST_BACKEND not defined */

int cmd_plugin(editor_ctx_t *ctx, const char *args) {
    (void)args;
    editor_set_status_msg(ctx, "Plugin support not compiled in");
    return 0;
}

#endif /* BUILD_MINIHOST_BACKEND */
