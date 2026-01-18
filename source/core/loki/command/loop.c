/* loop.c - Live loop commands (:loop, :unloop)
 *
 * Commands for starting and stopping live loops that re-evaluate
 * buffer content on beat boundaries synced to Ableton Link.
 */

#include "command_impl.h"
#include "../live_loop.h"
#include "loki/link.h"
#include <stdlib.h>

/* :loop <beats> - Start live loop every N beats */
int cmd_loop(editor_ctx_t *ctx, const char *args) {
    if (!args || !*args) {
        /* No argument - show current loop status */
        if (live_loop_is_active(ctx)) {
            double interval = live_loop_get_interval(ctx);
            editor_set_status_msg(ctx, "Loop active: every %.1f beats", interval);
        } else {
            editor_set_status_msg(ctx, "Usage: :loop <beats> (e.g., :loop 4)");
        }
        return 1;
    }

    /* Check if Link is enabled */
    if (!loki_link_is_enabled(ctx)) {
        editor_set_status_msg(ctx, "Link must be enabled first (:link on)");
        return 0;
    }

    double beats = atof(args);
    if (beats <= 0) {
        editor_set_status_msg(ctx, "Beats must be positive (e.g., :loop 4)");
        return 0;
    }

    if (live_loop_start(ctx, beats) == 0) {
        editor_set_status_msg(ctx, "Loop started: every %.1f beats", beats);
        return 1;
    } else {
        editor_set_status_msg(ctx, "Failed to start loop (max loops reached?)");
        return 0;
    }
}

/* :unloop - Stop live loop for current buffer */
int cmd_unloop(editor_ctx_t *ctx, const char *args) {
    (void)args;

    if (!live_loop_is_active(ctx)) {
        editor_set_status_msg(ctx, "No active loop");
        return 1;
    }

    live_loop_stop(ctx);
    editor_set_status_msg(ctx, "Loop stopped");
    return 1;
}
