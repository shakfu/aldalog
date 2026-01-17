/* goto.c - Navigation commands (:goto, :<number>)
 *
 * Commands for cursor movement and navigation.
 */

#include "command_impl.h"

/* :goto, :<number> - Go to line number */
int cmd_goto(editor_ctx_t *ctx, const char *args) {
    if (!args || !args[0]) {
        editor_set_status_msg(ctx, "Usage: :<line> or :goto <line>");
        return 0;
    }

    /* Parse line number */
    int line = atoi(args);
    if (line < 1) {
        editor_set_status_msg(ctx, "Invalid line number: %s", args);
        return 0;
    }

    /* Clamp to valid range (1-indexed for user, 0-indexed internally) */
    if (line > ctx->model.numrows) {
        line = ctx->model.numrows;
    }

    /* Move cursor to the line (convert to 0-indexed) */
    ctx->view.cy = line - 1;
    ctx->view.cx = 0;

    /* Adjust scroll to show the target line */
    if (ctx->view.cy < ctx->view.rowoff) {
        ctx->view.rowoff = ctx->view.cy;
    } else if (ctx->view.cy >= ctx->view.rowoff + ctx->view.screenrows - 2) {
        ctx->view.rowoff = ctx->view.cy - ctx->view.screenrows / 2;
        if (ctx->view.rowoff < 0) ctx->view.rowoff = 0;
    }

    editor_set_status_msg(ctx, "Line %d", line);
    return 1;
}
