/**
 * @file test_tsf_backend.c
 * @brief Tests for TinySoundFont audio backend.
 *
 * Tests verify:
 * - Backend initialization and cleanup
 * - State queries before/after initialization
 * - Soundfont loading (requires test soundfont)
 * - Enable/disable ref counting
 * - MIDI message sending (note on/off, program, CC)
 * - All notes off
 *
 * Note: Full audio output testing requires manual verification.
 * These tests verify the API behaves correctly without crashing.
 */

#include "test_framework.h"
#include "audio/audio.h"

/* ============================================================================
 * Initialization Tests
 * ============================================================================ */

TEST(tsf_init_cleanup) {
    /* Init should succeed */
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Double init should also succeed (idempotent) */
    result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Cleanup should work */
    shared_tsf_cleanup();

    /* Double cleanup should not crash */
    shared_tsf_cleanup();
}

TEST(tsf_state_before_init) {
    /* Ensure clean state */
    shared_tsf_cleanup();

    /* Should report no soundfont */
    ASSERT_FALSE(shared_tsf_has_soundfont());

    /* Should report not enabled */
    ASSERT_FALSE(shared_tsf_is_enabled());

    /* Preset count should be 0 */
    ASSERT_EQ(shared_tsf_get_preset_count(), 0);

    /* Preset name should be NULL */
    ASSERT_NULL(shared_tsf_get_preset_name(0));
}

TEST(tsf_state_after_init_no_soundfont) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Should report no soundfont */
    ASSERT_FALSE(shared_tsf_has_soundfont());

    /* Should report not enabled */
    ASSERT_FALSE(shared_tsf_is_enabled());

    /* Preset count should be 0 */
    ASSERT_EQ(shared_tsf_get_preset_count(), 0);

    shared_tsf_cleanup();
}

/* ============================================================================
 * Soundfont Loading Tests
 * ============================================================================ */

TEST(tsf_load_null_path) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Loading NULL path should fail */
    result = shared_tsf_load_soundfont(NULL);
    ASSERT_EQ(result, -1);

    shared_tsf_cleanup();
}

TEST(tsf_load_nonexistent_file) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Loading nonexistent file should fail */
    result = shared_tsf_load_soundfont("/nonexistent/path/to/soundfont.sf2");
    ASSERT_EQ(result, -1);

    /* Should still report no soundfont */
    ASSERT_FALSE(shared_tsf_has_soundfont());

    shared_tsf_cleanup();
}

TEST(tsf_load_without_init) {
    /* Ensure clean state */
    shared_tsf_cleanup();

    /* Loading without init should fail */
    int result = shared_tsf_load_soundfont("/some/path.sf2");
    ASSERT_EQ(result, -1);
}

/* ============================================================================
 * Enable/Disable Tests
 * ============================================================================ */

TEST(tsf_enable_without_soundfont) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Enable without soundfont should fail */
    result = shared_tsf_enable();
    ASSERT_EQ(result, -1);

    /* Should still be disabled */
    ASSERT_FALSE(shared_tsf_is_enabled());

    shared_tsf_cleanup();
}

TEST(tsf_disable_without_enable) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Disable without enable should not crash */
    shared_tsf_disable();

    /* Should still be disabled */
    ASSERT_FALSE(shared_tsf_is_enabled());

    shared_tsf_cleanup();
}

TEST(tsf_disable_without_init) {
    /* Ensure clean state */
    shared_tsf_cleanup();

    /* Disable without init should not crash */
    shared_tsf_disable();
}

/* ============================================================================
 * MIDI Message Tests (without soundfont - should not crash)
 * ============================================================================ */

TEST(tsf_note_on_without_soundfont) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Note on without soundfont should not crash */
    shared_tsf_send_note_on(1, 60, 100);

    shared_tsf_cleanup();
}

TEST(tsf_note_off_without_soundfont) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Note off without soundfont should not crash */
    shared_tsf_send_note_off(1, 60);

    shared_tsf_cleanup();
}

TEST(tsf_program_change_without_soundfont) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Program change without soundfont should not crash */
    shared_tsf_send_program(1, 0);

    shared_tsf_cleanup();
}

TEST(tsf_cc_without_soundfont) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* CC without soundfont should not crash */
    shared_tsf_send_cc(1, 7, 100);  /* Volume */
    shared_tsf_send_cc(1, 10, 64);  /* Pan */

    shared_tsf_cleanup();
}

TEST(tsf_all_notes_off_without_soundfont) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* All notes off without soundfont should not crash */
    shared_tsf_all_notes_off();

    shared_tsf_cleanup();
}

/* ============================================================================
 * MIDI Message Tests (without init - should not crash)
 * ============================================================================ */

TEST(tsf_messages_without_init) {
    /* Ensure clean state */
    shared_tsf_cleanup();

    /* All these should not crash even without init */
    shared_tsf_send_note_on(1, 60, 100);
    shared_tsf_send_note_off(1, 60);
    shared_tsf_send_program(1, 0);
    shared_tsf_send_cc(1, 7, 100);
    shared_tsf_all_notes_off();
}

/* ============================================================================
 * Channel/Pitch/Velocity Boundary Tests
 * ============================================================================ */

TEST(tsf_channel_boundaries) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Test channel boundaries (1-16 are valid MIDI channels) */
    shared_tsf_send_note_on(1, 60, 100);   /* Min channel */
    shared_tsf_send_note_on(16, 60, 100);  /* Max channel */
    shared_tsf_send_note_on(0, 60, 100);   /* Below min - should handle gracefully */
    shared_tsf_send_note_on(17, 60, 100);  /* Above max - should handle gracefully */

    shared_tsf_cleanup();
}

TEST(tsf_pitch_boundaries) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Test pitch boundaries (0-127 are valid MIDI notes) */
    shared_tsf_send_note_on(1, 0, 100);    /* Min pitch */
    shared_tsf_send_note_on(1, 127, 100);  /* Max pitch */
    shared_tsf_send_note_on(1, -1, 100);   /* Below min - should handle gracefully */
    shared_tsf_send_note_on(1, 128, 100);  /* Above max - should handle gracefully */

    shared_tsf_cleanup();
}

TEST(tsf_velocity_boundaries) {
    int result = shared_tsf_init();
    ASSERT_EQ(result, 0);

    /* Test velocity boundaries (0-127 are valid) */
    shared_tsf_send_note_on(1, 60, 0);     /* Min velocity (note off) */
    shared_tsf_send_note_on(1, 60, 127);   /* Max velocity */
    shared_tsf_send_note_on(1, 60, -1);    /* Below min - should handle gracefully */
    shared_tsf_send_note_on(1, 60, 128);   /* Above max - should handle gracefully */

    shared_tsf_cleanup();
}

/* ============================================================================
 * Test Runner
 * ============================================================================ */

BEGIN_TEST_SUITE("TSF Backend Tests")

    /* Initialization */
    RUN_TEST(tsf_init_cleanup);
    RUN_TEST(tsf_state_before_init);
    RUN_TEST(tsf_state_after_init_no_soundfont);

    /* Soundfont loading */
    RUN_TEST(tsf_load_null_path);
    RUN_TEST(tsf_load_nonexistent_file);
    RUN_TEST(tsf_load_without_init);

    /* Enable/disable */
    RUN_TEST(tsf_enable_without_soundfont);
    RUN_TEST(tsf_disable_without_enable);
    RUN_TEST(tsf_disable_without_init);

    /* MIDI messages without soundfont */
    RUN_TEST(tsf_note_on_without_soundfont);
    RUN_TEST(tsf_note_off_without_soundfont);
    RUN_TEST(tsf_program_change_without_soundfont);
    RUN_TEST(tsf_cc_without_soundfont);
    RUN_TEST(tsf_all_notes_off_without_soundfont);

    /* MIDI messages without init */
    RUN_TEST(tsf_messages_without_init);

    /* Boundary tests */
    RUN_TEST(tsf_channel_boundaries);
    RUN_TEST(tsf_pitch_boundaries);
    RUN_TEST(tsf_velocity_boundaries);

END_TEST_SUITE()
