/**
 * @file test_minihost_backend.c
 * @brief Tests for minihost (VST3/AU plugin) audio backend.
 *
 * Tests verify:
 * - Backend initialization and cleanup
 * - Availability reporting based on build configuration
 * - State queries before/after initialization
 * - Plugin loading (requires test plugin, may be skipped)
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
 * Availability Tests
 * ============================================================================ */

TEST(minihost_availability) {
#ifdef BUILD_MINIHOST_BACKEND
    /* Should report available when compiled in */
    ASSERT_TRUE(shared_minihost_is_available());
#else
    /* Should report not available when not compiled in */
    ASSERT_FALSE(shared_minihost_is_available());
#endif
}

/* ============================================================================
 * Initialization Tests
 * ============================================================================ */

TEST(minihost_init_cleanup) {
#ifdef BUILD_MINIHOST_BACKEND
    /* Init should succeed */
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Double init should also succeed (idempotent) */
    result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Cleanup should work */
    shared_minihost_cleanup();

    /* Double cleanup should not crash */
    shared_minihost_cleanup();
#else
    /* Init should fail when not compiled in */
    int result = shared_minihost_init();
    ASSERT_EQ(result, -1);
#endif
}

TEST(minihost_state_before_init) {
    /* Ensure clean state */
    shared_minihost_cleanup();

    /* Should report no plugin */
    ASSERT_FALSE(shared_minihost_has_plugin(0));

    /* Should report not enabled */
    ASSERT_FALSE(shared_minihost_is_enabled());

    /* Plugin name should be NULL */
    ASSERT_NULL(shared_minihost_get_plugin_name(0));
}

TEST(minihost_state_after_init_no_plugin) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Should report no plugin */
    ASSERT_FALSE(shared_minihost_has_plugin(0));

    /* Should report not enabled */
    ASSERT_FALSE(shared_minihost_is_enabled());

    /* Plugin name should be NULL */
    ASSERT_NULL(shared_minihost_get_plugin_name(0));

    shared_minihost_cleanup();
#endif
}

/* ============================================================================
 * Plugin Loading Tests
 * ============================================================================ */

TEST(minihost_load_null_path) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Loading NULL path should fail */
    result = shared_minihost_load(0, NULL);
    ASSERT_EQ(result, -1);

    shared_minihost_cleanup();
#endif
}

TEST(minihost_load_nonexistent_file) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Loading nonexistent file should fail */
    result = shared_minihost_load(0, "/nonexistent/path/to/plugin.vst3");
    ASSERT_EQ(result, -1);

    /* Should still report no plugin */
    ASSERT_FALSE(shared_minihost_has_plugin(0));

    shared_minihost_cleanup();
#endif
}

TEST(minihost_load_without_init) {
    /* Ensure clean state */
    shared_minihost_cleanup();

    /* Loading without init should fail */
    int result = shared_minihost_load(0, "/some/path.vst3");
    ASSERT_EQ(result, -1);
}

TEST(minihost_invalid_slot) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Invalid slot numbers should fail */
    result = shared_minihost_load(-1, "/some/path.vst3");
    ASSERT_EQ(result, -1);

    result = shared_minihost_load(MINIHOST_MAX_PLUGINS, "/some/path.vst3");
    ASSERT_EQ(result, -1);

    /* has_plugin with invalid slot should return false */
    ASSERT_FALSE(shared_minihost_has_plugin(-1));
    ASSERT_FALSE(shared_minihost_has_plugin(MINIHOST_MAX_PLUGINS));

    /* unload with invalid slot should not crash */
    shared_minihost_unload(-1);
    shared_minihost_unload(MINIHOST_MAX_PLUGINS);

    shared_minihost_cleanup();
#endif
}

/* ============================================================================
 * Enable/Disable Tests
 * ============================================================================ */

TEST(minihost_enable_without_plugin) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Enable without plugin should succeed (backend starts, just no sound) */
    result = shared_minihost_enable();
    ASSERT_EQ(result, 0);

    /* Should be enabled */
    ASSERT_TRUE(shared_minihost_is_enabled());

    /* Disable */
    shared_minihost_disable();
    ASSERT_FALSE(shared_minihost_is_enabled());

    shared_minihost_cleanup();
#endif
}

TEST(minihost_disable_without_enable) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Disable without enable should not crash */
    shared_minihost_disable();

    /* Should still be disabled */
    ASSERT_FALSE(shared_minihost_is_enabled());

    shared_minihost_cleanup();
#endif
}

TEST(minihost_disable_without_init) {
    /* Ensure clean state */
    shared_minihost_cleanup();

    /* Disable without init should not crash */
    shared_minihost_disable();
}

TEST(minihost_refcount) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Enable multiple times */
    shared_minihost_enable();
    ASSERT_TRUE(shared_minihost_is_enabled());

    shared_minihost_enable();
    ASSERT_TRUE(shared_minihost_is_enabled());

    /* First disable should keep it enabled (ref_count > 0) */
    shared_minihost_disable();
    ASSERT_TRUE(shared_minihost_is_enabled());

    /* Second disable should actually disable */
    shared_minihost_disable();
    ASSERT_FALSE(shared_minihost_is_enabled());

    shared_minihost_cleanup();
#endif
}

/* ============================================================================
 * MIDI Message Tests (without plugin - should not crash)
 * ============================================================================ */

TEST(minihost_note_on_without_plugin) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Note on without plugin should not crash */
    shared_minihost_send_note_on(1, 60, 100);

    shared_minihost_cleanup();
#endif
}

TEST(minihost_note_off_without_plugin) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Note off without plugin should not crash */
    shared_minihost_send_note_off(1, 60);

    shared_minihost_cleanup();
#endif
}

TEST(minihost_program_change_without_plugin) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Program change without plugin should not crash */
    shared_minihost_send_program(1, 0);

    shared_minihost_cleanup();
#endif
}

TEST(minihost_cc_without_plugin) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* CC without plugin should not crash */
    shared_minihost_send_cc(1, 7, 100);  /* Volume */
    shared_minihost_send_cc(1, 10, 64);  /* Pan */

    shared_minihost_cleanup();
#endif
}

TEST(minihost_pitch_bend_without_plugin) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Pitch bend without plugin should not crash */
    shared_minihost_send_pitch_bend(1, 0);     /* Center */
    shared_minihost_send_pitch_bend(1, 8191);  /* Max up */
    shared_minihost_send_pitch_bend(1, -8192); /* Max down */

    shared_minihost_cleanup();
#endif
}

TEST(minihost_all_notes_off_without_plugin) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* All notes off without plugin should not crash */
    shared_minihost_all_notes_off();

    shared_minihost_cleanup();
#endif
}

/* ============================================================================
 * MIDI Message Tests (without init - should not crash)
 * ============================================================================ */

TEST(minihost_messages_without_init) {
    /* Ensure clean state */
    shared_minihost_cleanup();

    /* All these should not crash even without init */
    shared_minihost_send_note_on(1, 60, 100);
    shared_minihost_send_note_off(1, 60);
    shared_minihost_send_program(1, 0);
    shared_minihost_send_cc(1, 7, 100);
    shared_minihost_send_pitch_bend(1, 0);
    shared_minihost_all_notes_off();
}

/* ============================================================================
 * Channel/Pitch/Velocity Boundary Tests
 * ============================================================================ */

TEST(minihost_channel_boundaries) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Test channel boundaries (1-16 are valid MIDI channels) */
    shared_minihost_send_note_on(1, 60, 100);   /* Min channel */
    shared_minihost_send_note_on(16, 60, 100);  /* Max channel */
    shared_minihost_send_note_on(0, 60, 100);   /* Below min - should handle gracefully */
    shared_minihost_send_note_on(17, 60, 100);  /* Above max - should handle gracefully */

    shared_minihost_cleanup();
#endif
}

TEST(minihost_pitch_boundaries) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Test pitch boundaries (0-127 are valid MIDI notes) */
    shared_minihost_send_note_on(1, 0, 100);    /* Min pitch */
    shared_minihost_send_note_on(1, 127, 100);  /* Max pitch */
    shared_minihost_send_note_on(1, -1, 100);   /* Below min - should handle gracefully */
    shared_minihost_send_note_on(1, 128, 100);  /* Above max - should handle gracefully */

    shared_minihost_cleanup();
#endif
}

TEST(minihost_velocity_boundaries) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Test velocity boundaries (0-127 are valid) */
    shared_minihost_send_note_on(1, 60, 0);     /* Min velocity (note off) */
    shared_minihost_send_note_on(1, 60, 127);   /* Max velocity */
    shared_minihost_send_note_on(1, 60, -1);    /* Below min - should handle gracefully */
    shared_minihost_send_note_on(1, 60, 128);   /* Above max - should handle gracefully */

    shared_minihost_cleanup();
#endif
}

/* ============================================================================
 * Parameter Tests
 * ============================================================================ */

TEST(minihost_params_without_plugin) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* Parameter operations without plugin should fail gracefully */
    ASSERT_EQ(shared_minihost_get_num_params(0), 0);
    ASSERT_EQ(shared_minihost_get_param(0, 0), 0.0f);
    ASSERT_EQ(shared_minihost_set_param(0, 0, 0.5f), -1);

    char buf[64];
    ASSERT_EQ(shared_minihost_get_param_name(0, 0, buf, sizeof(buf)), -1);

    shared_minihost_cleanup();
#endif
}

/* ============================================================================
 * State Persistence Tests
 * ============================================================================ */

TEST(minihost_state_without_plugin) {
#ifdef BUILD_MINIHOST_BACKEND
    int result = shared_minihost_init();
    ASSERT_EQ(result, 0);

    /* State operations without plugin should fail */
    ASSERT_EQ(shared_minihost_save_state(0, "/tmp/test.state"), -1);
    ASSERT_EQ(shared_minihost_load_state(0, "/tmp/test.state"), -1);

    shared_minihost_cleanup();
#endif
}

/* ============================================================================
 * Test Runner
 * ============================================================================ */

BEGIN_TEST_SUITE("Minihost Backend Tests")

    /* Availability */
    RUN_TEST(minihost_availability);

    /* Initialization */
    RUN_TEST(minihost_init_cleanup);
    RUN_TEST(minihost_state_before_init);
    RUN_TEST(minihost_state_after_init_no_plugin);

    /* Plugin loading */
    RUN_TEST(minihost_load_null_path);
    RUN_TEST(minihost_load_nonexistent_file);
    RUN_TEST(minihost_load_without_init);
    RUN_TEST(minihost_invalid_slot);

    /* Enable/disable */
    RUN_TEST(minihost_enable_without_plugin);
    RUN_TEST(minihost_disable_without_enable);
    RUN_TEST(minihost_disable_without_init);
    RUN_TEST(minihost_refcount);

    /* MIDI messages without plugin */
    RUN_TEST(minihost_note_on_without_plugin);
    RUN_TEST(minihost_note_off_without_plugin);
    RUN_TEST(minihost_program_change_without_plugin);
    RUN_TEST(minihost_cc_without_plugin);
    RUN_TEST(minihost_pitch_bend_without_plugin);
    RUN_TEST(minihost_all_notes_off_without_plugin);

    /* MIDI messages without init */
    RUN_TEST(minihost_messages_without_init);

    /* Boundary tests */
    RUN_TEST(minihost_channel_boundaries);
    RUN_TEST(minihost_pitch_boundaries);
    RUN_TEST(minihost_velocity_boundaries);

    /* Parameter tests */
    RUN_TEST(minihost_params_without_plugin);

    /* State persistence tests */
    RUN_TEST(minihost_state_without_plugin);

END_TEST_SUITE()
