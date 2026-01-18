/* test_serialize.c - Unit tests for EditorModel serialization
 *
 * Tests serialize/deserialize, save/load snapshot, and edge cases.
 */

#include "test_framework.h"
#include "loki/core.h"
#include "loki/internal.h"
#include "loki/serialize.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

/* Helper: Create a test model with sample data */
static void setup_test_model(EditorModel *model) {
    memset(model, 0, sizeof(EditorModel));

    /* Set filename */
    model->filename = strdup("test_file.txt");
    model->dirty = 1;

    /* Create 3 rows */
    model->numrows = 3;
    model->row = calloc(3, sizeof(t_erow));

    /* Row 0: "Hello, World!" */
    model->row[0].idx = 0;
    model->row[0].size = 13;
    model->row[0].chars = strdup("Hello, World!");

    /* Row 1: "" (empty) */
    model->row[1].idx = 1;
    model->row[1].size = 0;
    model->row[1].chars = strdup("");

    /* Row 2: "Line 3" */
    model->row[2].idx = 2;
    model->row[2].size = 6;
    model->row[2].chars = strdup("Line 3");
}

/* Helper: Free model resources */
static void cleanup_test_model(EditorModel *model) {
    if (model->filename) {
        free(model->filename);
        model->filename = NULL;
    }
    for (int i = 0; i < model->numrows; i++) {
        free(model->row[i].chars);
        free(model->row[i].render);
        free(model->row[i].hl);
    }
    free(model->row);
    model->row = NULL;
    model->numrows = 0;
}

/* Test: Serialized size calculation */
TEST(serialize_size_empty) {
    EditorModel model;
    memset(&model, 0, sizeof(EditorModel));

    size_t size = editor_model_serialized_size(&model);
    /* Header (6) + filename_len (4) + dirty (1) + numrows (4) = 15 */
    ASSERT_EQ(size, 15);
}

TEST(serialize_size_with_data) {
    EditorModel model;
    setup_test_model(&model);

    size_t size = editor_model_serialized_size(&model);
    /* Header (6) + filename_len (4) + filename (13) + dirty (1) + numrows (4)
     * + row0: size(4) + data(13) + row1: size(4) + data(0) + row2: size(4) + data(6) */
    size_t expected = 6 + 4 + 13 + 1 + 4 + (4 + 13) + (4 + 0) + (4 + 6);
    ASSERT_EQ(size, expected);

    cleanup_test_model(&model);
}

/* Test: Basic serialize/deserialize roundtrip */
TEST(serialize_roundtrip) {
    EditorModel src, dst;
    setup_test_model(&src);
    memset(&dst, 0, sizeof(EditorModel));

    /* Serialize */
    char *buf = NULL;
    size_t len = 0;
    ASSERT_EQ(editor_model_serialize(&src, &buf, &len), 0);
    ASSERT_NOT_NULL(buf);
    ASSERT_TRUE(len > 0);

    /* Deserialize */
    ASSERT_EQ(editor_model_deserialize(&dst, buf, len), 0);

    /* Verify */
    ASSERT_STR_EQ(dst.filename, "test_file.txt");
    ASSERT_EQ(dst.dirty, 1);
    ASSERT_EQ(dst.numrows, 3);
    ASSERT_STR_EQ(dst.row[0].chars, "Hello, World!");
    ASSERT_EQ(dst.row[0].size, 13);
    ASSERT_STR_EQ(dst.row[1].chars, "");
    ASSERT_EQ(dst.row[1].size, 0);
    ASSERT_STR_EQ(dst.row[2].chars, "Line 3");
    ASSERT_EQ(dst.row[2].size, 6);

    free(buf);
    cleanup_test_model(&src);
    cleanup_test_model(&dst);
}

/* Test: Empty model serialize/deserialize */
TEST(serialize_empty_model) {
    EditorModel src, dst;
    memset(&src, 0, sizeof(EditorModel));
    memset(&dst, 0, sizeof(EditorModel));

    char *buf = NULL;
    size_t len = 0;
    ASSERT_EQ(editor_model_serialize(&src, &buf, &len), 0);
    ASSERT_EQ(editor_model_deserialize(&dst, buf, len), 0);

    ASSERT_NULL(dst.filename);
    ASSERT_EQ(dst.dirty, 0);
    ASSERT_EQ(dst.numrows, 0);

    free(buf);
}

/* Test: Model with no filename */
TEST(serialize_no_filename) {
    EditorModel src, dst;
    memset(&src, 0, sizeof(EditorModel));
    memset(&dst, 0, sizeof(EditorModel));

    src.numrows = 1;
    src.row = calloc(1, sizeof(t_erow));
    src.row[0].idx = 0;
    src.row[0].size = 4;
    src.row[0].chars = strdup("test");

    char *buf = NULL;
    size_t len = 0;
    ASSERT_EQ(editor_model_serialize(&src, &buf, &len), 0);
    ASSERT_EQ(editor_model_deserialize(&dst, buf, len), 0);

    ASSERT_NULL(dst.filename);
    ASSERT_EQ(dst.numrows, 1);
    ASSERT_STR_EQ(dst.row[0].chars, "test");

    free(buf);
    cleanup_test_model(&src);
    cleanup_test_model(&dst);
}

/* Test: Deserialize invalid data */
TEST(deserialize_invalid_magic) {
    EditorModel model;
    memset(&model, 0, sizeof(EditorModel));

    char bad_data[] = "XXXX\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
    ASSERT_EQ(editor_model_deserialize(&model, bad_data, sizeof(bad_data)), -1);
}

TEST(deserialize_truncated) {
    EditorModel model;
    memset(&model, 0, sizeof(EditorModel));

    /* Valid header but truncated */
    char truncated[] = "LOKI\x01\x00";
    ASSERT_EQ(editor_model_deserialize(&model, truncated, 6), -1);
}

/* Test: Save/load snapshot */
TEST(save_load_snapshot) {
    EditorModel src, dst;
    setup_test_model(&src);
    memset(&dst, 0, sizeof(EditorModel));

    const char *path = "/tmp/test_loki_snapshot.bin";

    /* Save */
    ASSERT_EQ(editor_model_save_snapshot(&src, path), 0);

    /* Load */
    ASSERT_EQ(editor_model_load_snapshot(&dst, path), 0);

    /* Verify */
    ASSERT_STR_EQ(dst.filename, "test_file.txt");
    ASSERT_EQ(dst.numrows, 3);
    ASSERT_STR_EQ(dst.row[0].chars, "Hello, World!");

    /* Cleanup */
    unlink(path);
    cleanup_test_model(&src);
    cleanup_test_model(&dst);
}

/* Test: Load from non-existent file */
TEST(load_nonexistent_file) {
    EditorModel model;
    memset(&model, 0, sizeof(EditorModel));

    ASSERT_EQ(editor_model_load_snapshot(&model, "/nonexistent/path/file.bin"), -1);
}

/* Test: Serialize to pre-allocated buffer */
TEST(serialize_to_buf) {
    EditorModel model;
    setup_test_model(&model);

    size_t required = editor_model_serialized_size(&model);
    char *buf = malloc(required);
    ASSERT_NOT_NULL(buf);

    int written = editor_model_serialize_to_buf(&model, buf, required);
    ASSERT_EQ(written, (int)required);

    /* Verify by deserializing */
    EditorModel dst;
    memset(&dst, 0, sizeof(EditorModel));
    ASSERT_EQ(editor_model_deserialize(&dst, buf, written), 0);
    ASSERT_STR_EQ(dst.row[0].chars, "Hello, World!");

    free(buf);
    cleanup_test_model(&model);
    cleanup_test_model(&dst);
}

/* Test: Serialize to undersized buffer fails */
TEST(serialize_to_small_buf) {
    EditorModel model;
    setup_test_model(&model);

    char buf[10];  /* Too small */
    ASSERT_EQ(editor_model_serialize_to_buf(&model, buf, sizeof(buf)), -1);

    cleanup_test_model(&model);
}

/* Test: Deserialize replaces existing data */
TEST(deserialize_replaces_existing) {
    EditorModel model;
    setup_test_model(&model);

    /* Serialize current state */
    char *buf = NULL;
    size_t len = 0;
    ASSERT_EQ(editor_model_serialize(&model, &buf, &len), 0);

    /* Modify model */
    free(model.filename);
    model.filename = strdup("modified.txt");
    model.dirty = 0;
    free(model.row[0].chars);
    model.row[0].chars = strdup("Modified");
    model.row[0].size = 8;

    /* Deserialize should restore original state */
    ASSERT_EQ(editor_model_deserialize(&model, buf, len), 0);
    ASSERT_STR_EQ(model.filename, "test_file.txt");
    ASSERT_EQ(model.dirty, 1);
    ASSERT_STR_EQ(model.row[0].chars, "Hello, World!");

    free(buf);
    cleanup_test_model(&model);
}

BEGIN_TEST_SUITE("EditorModel Serialization")
    RUN_TEST(serialize_size_empty);
    RUN_TEST(serialize_size_with_data);
    RUN_TEST(serialize_roundtrip);
    RUN_TEST(serialize_empty_model);
    RUN_TEST(serialize_no_filename);
    RUN_TEST(deserialize_invalid_magic);
    RUN_TEST(deserialize_truncated);
    RUN_TEST(save_load_snapshot);
    RUN_TEST(load_nonexistent_file);
    RUN_TEST(serialize_to_buf);
    RUN_TEST(serialize_to_small_buf);
    RUN_TEST(deserialize_replaces_existing);
END_TEST_SUITE()
