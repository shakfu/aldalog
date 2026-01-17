/**
 * @file serialize.h
 * @brief Serialization helpers for EditorModel
 *
 * Provides functions to serialize/deserialize EditorModel state.
 * Use cases:
 * - Snapshot/restore: Save editor state, restore after crash
 * - RPC transport: Send model state over network for remote editing
 * - Test fixtures: Load predefined model states for testing
 *
 * The serialization format is a simple binary format:
 * - Header: magic (4 bytes) + version (2 bytes)
 * - Filename: length (4 bytes) + data
 * - Flags: dirty (1 byte)
 * - Rows: count (4 bytes) + [size (4 bytes) + data] for each row
 */

#ifndef LOKI_SERIALIZE_H
#define LOKI_SERIALIZE_H

#include <stddef.h>

/* Forward declaration */
struct editor_ctx;
typedef struct editor_ctx editor_ctx_t;
struct EditorModel;
typedef struct EditorModel EditorModel;

/* Serialization format version */
#define LOKI_SERIALIZE_VERSION 1

/* Magic bytes: "LOKI" */
#define LOKI_SERIALIZE_MAGIC 0x494B4F4C

/**
 * Serialize EditorModel to a binary buffer.
 *
 * Serializes the document content (rows, filename, dirty flag).
 * Does NOT serialize runtime state (undo, indent config, language states).
 *
 * @param model Source model to serialize
 * @param out_buf Output buffer (allocated by this function, caller must free)
 * @param out_len Output buffer length
 * @return 0 on success, -1 on error
 */
int editor_model_serialize(const EditorModel *model, char **out_buf, size_t *out_len);

/**
 * Deserialize EditorModel from a binary buffer.
 *
 * Restores document content into the model. The model should be
 * initialized (via editor_ctx_init) before calling this function.
 * Existing rows in the model will be freed and replaced.
 *
 * @param model Destination model (must be initialized)
 * @param data Input buffer
 * @param len Input buffer length
 * @return 0 on success, -1 on error (invalid format, version mismatch, etc.)
 */
int editor_model_deserialize(EditorModel *model, const char *data, size_t len);

/**
 * Save EditorModel to a snapshot file.
 *
 * Convenience wrapper that serializes and writes to file.
 *
 * @param model Source model to save
 * @param path File path to write
 * @return 0 on success, -1 on error
 */
int editor_model_save_snapshot(const EditorModel *model, const char *path);

/**
 * Load EditorModel from a snapshot file.
 *
 * Convenience wrapper that reads file and deserializes.
 * The model should be initialized before calling this function.
 *
 * @param model Destination model (must be initialized)
 * @param path File path to read
 * @return 0 on success, -1 on error
 */
int editor_model_load_snapshot(EditorModel *model, const char *path);

/**
 * Get the serialized size of an EditorModel without allocating.
 *
 * Useful for pre-allocating buffers or estimating storage needs.
 *
 * @param model Model to measure
 * @return Size in bytes that serialize would produce
 */
size_t editor_model_serialized_size(const EditorModel *model);

/**
 * Serialize EditorModel to a pre-allocated buffer.
 *
 * Unlike editor_model_serialize, this writes to a caller-provided buffer.
 * Use editor_model_serialized_size to determine required size.
 *
 * @param model Source model to serialize
 * @param buf Output buffer (must be at least serialized_size bytes)
 * @param buf_len Buffer length
 * @return Bytes written on success, -1 on error (buffer too small)
 */
int editor_model_serialize_to_buf(const EditorModel *model, char *buf, size_t buf_len);

#endif /* LOKI_SERIALIZE_H */
