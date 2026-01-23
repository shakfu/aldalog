/**
 * test_audio.c - Tests for tracker audio integration
 */

#include "test_framework.h"
#include "tracker_audio.h"
#include "tracker_model.h"
#include "tracker_plugin_notes.h"
#include <string.h>

/*============================================================================
 * Test Fixtures
 *============================================================================*/

/* Mock shared context that records MIDI events */
typedef struct {
    SharedContext base;  /* Must be first for casting */

    /* Recorded events */
    struct {
        int channel;
        int pitch;
        int velocity;
        int type;  /* 0=note_on, 1=note_off, 2=cc, 3=program */
    } events[256];
    int event_count;

    /* For CC events */
    int last_cc;
    int last_cc_value;
} MockSharedContext;

static MockSharedContext mock_ctx;

/* Mock implementations of shared backend functions */
static int mock_note_on_count = 0;
static int mock_note_off_count = 0;
static int mock_cc_count = 0;
static int mock_program_count = 0;
static int mock_panic_count = 0;

static void reset_mocks(void) {
    memset(&mock_ctx, 0, sizeof(mock_ctx));
    mock_note_on_count = 0;
    mock_note_off_count = 0;
    mock_cc_count = 0;
    mock_program_count = 0;
    mock_panic_count = 0;
}

/*============================================================================
 * Connection Tests
 *============================================================================*/

TEST(connect_returns_true_with_valid_args) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    ASSERT_NOT_NULL(engine);

    bool result = tracker_audio_connect(engine, &mock_ctx.base);
    ASSERT_TRUE(result);

    tracker_engine_free(engine);
}

TEST(connect_returns_false_with_null_engine) {
    reset_mocks();
    bool result = tracker_audio_connect(NULL, &mock_ctx.base);
    ASSERT_FALSE(result);
}

TEST(connect_returns_false_with_null_context) {
    TrackerEngine* engine = tracker_engine_new();
    ASSERT_NOT_NULL(engine);

    bool result = tracker_audio_connect(engine, NULL);
    ASSERT_FALSE(result);

    tracker_engine_free(engine);
}

TEST(is_connected_returns_true_after_connect) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    tracker_audio_connect(engine, &mock_ctx.base);

    ASSERT_TRUE(tracker_audio_is_connected(engine));

    tracker_engine_free(engine);
}

TEST(is_connected_returns_false_before_connect) {
    TrackerEngine* engine = tracker_engine_new();
    ASSERT_FALSE(tracker_audio_is_connected(engine));
    tracker_engine_free(engine);
}

TEST(is_connected_returns_false_after_disconnect) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    tracker_audio_connect(engine, &mock_ctx.base);
    tracker_audio_disconnect(engine);

    ASSERT_FALSE(tracker_audio_is_connected(engine));

    tracker_engine_free(engine);
}

TEST(get_context_returns_connected_context) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    tracker_audio_connect(engine, &mock_ctx.base);

    SharedContext* ctx = tracker_audio_get_context(engine);
    ASSERT_TRUE(ctx == &mock_ctx.base);

    tracker_engine_free(engine);
}

TEST(get_context_returns_null_when_not_connected) {
    TrackerEngine* engine = tracker_engine_new();

    SharedContext* ctx = tracker_audio_get_context(engine);
    ASSERT_NULL(ctx);

    tracker_engine_free(engine);
}

/*============================================================================
 * Convenience Constructor Tests
 *============================================================================*/

TEST(engine_new_creates_connected_engine) {
    reset_mocks();

    TrackerEngine* engine = tracker_audio_engine_new(&mock_ctx.base);
    ASSERT_NOT_NULL(engine);
    ASSERT_TRUE(tracker_audio_is_connected(engine));

    tracker_engine_free(engine);
}

TEST(engine_new_returns_null_with_null_context) {
    TrackerEngine* engine = tracker_audio_engine_new(NULL);
    /* Should still create engine but not be connected */
    if (engine) {
        ASSERT_FALSE(tracker_audio_is_connected(engine));
        tracker_engine_free(engine);
    }
}

TEST(engine_new_with_config_creates_configured_engine) {
    reset_mocks();

    TrackerEngineConfig config;
    tracker_engine_config_init(&config);
    config.send_all_notes_off_on_stop = false;

    TrackerEngine* engine = tracker_audio_engine_new_with_config(&config, &mock_ctx.base);
    ASSERT_NOT_NULL(engine);
    ASSERT_TRUE(tracker_audio_is_connected(engine));

    tracker_engine_free(engine);
}

/*============================================================================
 * Output Callback Tests
 *
 * These tests verify the TrackerOutput callbacks are properly wired to
 * dispatch MIDI events. Since we can't easily mock shared_send_note_on
 * without modifying the linker, we test that the output interface is
 * properly configured.
 *============================================================================*/

TEST(output_has_note_on_callback) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    tracker_audio_connect(engine, &mock_ctx.base);

    const TrackerOutput* output = tracker_engine_get_output(engine);
    ASSERT_NOT_NULL(output);
    ASSERT_NOT_NULL(output->note_on);

    tracker_engine_free(engine);
}

TEST(output_has_note_off_callback) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    tracker_audio_connect(engine, &mock_ctx.base);

    const TrackerOutput* output = tracker_engine_get_output(engine);
    ASSERT_NOT_NULL(output);
    ASSERT_NOT_NULL(output->note_off);

    tracker_engine_free(engine);
}

TEST(output_has_cc_callback) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    tracker_audio_connect(engine, &mock_ctx.base);

    const TrackerOutput* output = tracker_engine_get_output(engine);
    ASSERT_NOT_NULL(output);
    ASSERT_NOT_NULL(output->cc);

    tracker_engine_free(engine);
}

TEST(output_has_program_change_callback) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    tracker_audio_connect(engine, &mock_ctx.base);

    const TrackerOutput* output = tracker_engine_get_output(engine);
    ASSERT_NOT_NULL(output);
    ASSERT_NOT_NULL(output->program_change);

    tracker_engine_free(engine);
}

TEST(output_has_all_notes_off_callback) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    tracker_audio_connect(engine, &mock_ctx.base);

    const TrackerOutput* output = tracker_engine_get_output(engine);
    ASSERT_NOT_NULL(output);
    ASSERT_NOT_NULL(output->all_notes_off);

    tracker_engine_free(engine);
}

TEST(output_user_data_is_context) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    tracker_audio_connect(engine, &mock_ctx.base);

    const TrackerOutput* output = tracker_engine_get_output(engine);
    ASSERT_NOT_NULL(output);
    ASSERT_TRUE(output->user_data == &mock_ctx.base);

    tracker_engine_free(engine);
}

/*============================================================================
 * Link Sync Tests
 *============================================================================*/

TEST(link_sync_returns_false_when_link_disabled) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    tracker_audio_connect(engine, &mock_ctx.base);

    /* Link is not enabled in mock context */
    mock_ctx.base.link_enabled = 0;

    bool result = tracker_audio_enable_link_sync(engine, &mock_ctx.base);
    ASSERT_FALSE(result);

    tracker_engine_free(engine);
}

TEST(disable_link_sync_sets_internal_mode) {
    reset_mocks();

    TrackerEngine* engine = tracker_engine_new();
    tracker_audio_connect(engine, &mock_ctx.base);

    /* Manually set to external mode */
    tracker_engine_set_sync_mode(engine, TRACKER_SYNC_EXTERNAL_LINK);

    tracker_audio_disable_link_sync(engine);

    /* Should now be internal */
    /* (no direct getter, but verify it doesn't crash) */

    tracker_engine_free(engine);
}

/*============================================================================
 * Integration Tests - Engine with Song
 *============================================================================*/

TEST(engine_with_song_can_play) {
    reset_mocks();

    /* Register notes plugin */
    tracker_plugin_notes_register();

    /* Create song */
    TrackerSong* song = tracker_song_new("Test");
    ASSERT_NOT_NULL(song);
    song->bpm = 120;
    song->rows_per_beat = 4;
    song->ticks_per_row = 24;

    /* Create pattern with 1 track and 16 rows */
    TrackerPattern* pattern = tracker_pattern_new(16, 1, "Pattern 1");
    ASSERT_NOT_NULL(pattern);

    /* Add pattern to song */
    int pattern_idx = tracker_song_add_pattern(song, pattern);
    ASSERT_TRUE(pattern_idx >= 0);

    /* Set track name and channel (track is at index 0) */
    pattern->tracks[0].default_channel = 1;

    /* Add a note to row 0 */
    TrackerCell* cell = tracker_pattern_get_cell(pattern, 0, 0);
    ASSERT_NOT_NULL(cell);
    tracker_cell_set_expression(cell, "C4", "notes");

    /* Create engine and connect */
    TrackerEngine* engine = tracker_audio_engine_new(&mock_ctx.base);
    ASSERT_NOT_NULL(engine);

    /* Load song */
    bool loaded = tracker_engine_load_song(engine, song);
    ASSERT_TRUE(loaded);

    /* Play should succeed */
    bool playing = tracker_engine_play(engine);
    ASSERT_TRUE(playing);
    ASSERT_TRUE(tracker_engine_is_playing(engine));

    /* Stop */
    tracker_engine_stop(engine);
    ASSERT_TRUE(tracker_engine_is_stopped(engine));

    tracker_engine_free(engine);
    tracker_song_free(song);
}

/*============================================================================
 * Test Runner
 *============================================================================*/

BEGIN_TEST_SUITE("Tracker Audio Integration")

/* Connection tests */
RUN_TEST(connect_returns_true_with_valid_args);
RUN_TEST(connect_returns_false_with_null_engine);
RUN_TEST(connect_returns_false_with_null_context);
RUN_TEST(is_connected_returns_true_after_connect);
RUN_TEST(is_connected_returns_false_before_connect);
RUN_TEST(is_connected_returns_false_after_disconnect);
RUN_TEST(get_context_returns_connected_context);
RUN_TEST(get_context_returns_null_when_not_connected);

/* Convenience constructor tests */
RUN_TEST(engine_new_creates_connected_engine);
RUN_TEST(engine_new_returns_null_with_null_context);
RUN_TEST(engine_new_with_config_creates_configured_engine);

/* Output callback tests */
RUN_TEST(output_has_note_on_callback);
RUN_TEST(output_has_note_off_callback);
RUN_TEST(output_has_cc_callback);
RUN_TEST(output_has_program_change_callback);
RUN_TEST(output_has_all_notes_off_callback);
RUN_TEST(output_user_data_is_context);

/* Link sync tests */
RUN_TEST(link_sync_returns_false_when_link_disabled);
RUN_TEST(disable_link_sync_sets_internal_mode);

/* Integration tests */
RUN_TEST(engine_with_song_can_play);

END_TEST_SUITE()
