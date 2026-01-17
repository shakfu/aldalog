/**
 * @file serialize.c
 * @brief Serialization helpers for EditorModel
 *
 * Binary format (little-endian):
 *   [Header]
 *     magic:    4 bytes (0x494B4F4C = "LOKI")
 *     version:  2 bytes
 *   [Filename]
 *     length:   4 bytes (0 if no filename)
 *     data:     length bytes
 *   [Flags]
 *     dirty:    1 byte
 *   [Rows]
 *     count:    4 bytes
 *     For each row:
 *       size:   4 bytes
 *       data:   size bytes
 */

#include "serialize.h"
#include "internal.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Header size: magic (4) + version (2) */
#define HEADER_SIZE 6

/* Helper: write uint32 little-endian */
static void write_u32(char *buf, uint32_t val) {
    buf[0] = (char)(val & 0xFF);
    buf[1] = (char)((val >> 8) & 0xFF);
    buf[2] = (char)((val >> 16) & 0xFF);
    buf[3] = (char)((val >> 24) & 0xFF);
}

/* Helper: read uint32 little-endian */
static uint32_t read_u32(const char *buf) {
    return (uint32_t)(unsigned char)buf[0] |
           ((uint32_t)(unsigned char)buf[1] << 8) |
           ((uint32_t)(unsigned char)buf[2] << 16) |
           ((uint32_t)(unsigned char)buf[3] << 24);
}

/* Helper: write uint16 little-endian */
static void write_u16(char *buf, uint16_t val) {
    buf[0] = (char)(val & 0xFF);
    buf[1] = (char)((val >> 8) & 0xFF);
}

/* Helper: read uint16 little-endian */
static uint16_t read_u16(const char *buf) {
    return (uint16_t)(unsigned char)buf[0] |
           ((uint16_t)(unsigned char)buf[1] << 8);
}

size_t editor_model_serialized_size(const EditorModel *model) {
    if (!model) return 0;

    size_t size = HEADER_SIZE;  /* magic + version */

    /* Filename: length (4) + data */
    size += 4;
    if (model->filename) {
        size += strlen(model->filename);
    }

    /* Flags: dirty (1) */
    size += 1;

    /* Rows: count (4) + [size (4) + data] for each */
    size += 4;
    for (int i = 0; i < model->numrows; i++) {
        size += 4;  /* row size */
        size += model->row[i].size;  /* row data */
    }

    return size;
}

int editor_model_serialize_to_buf(const EditorModel *model, char *buf, size_t buf_len) {
    if (!model || !buf) return -1;

    size_t required = editor_model_serialized_size(model);
    if (buf_len < required) return -1;

    char *p = buf;

    /* Header */
    write_u32(p, LOKI_SERIALIZE_MAGIC);
    p += 4;
    write_u16(p, LOKI_SERIALIZE_VERSION);
    p += 2;

    /* Filename */
    if (model->filename) {
        uint32_t len = (uint32_t)strlen(model->filename);
        write_u32(p, len);
        p += 4;
        memcpy(p, model->filename, len);
        p += len;
    } else {
        write_u32(p, 0);
        p += 4;
    }

    /* Flags */
    *p++ = (char)(model->dirty ? 1 : 0);

    /* Rows */
    write_u32(p, (uint32_t)model->numrows);
    p += 4;

    for (int i = 0; i < model->numrows; i++) {
        t_erow *row = &model->row[i];
        write_u32(p, (uint32_t)row->size);
        p += 4;
        if (row->size > 0 && row->chars) {
            memcpy(p, row->chars, row->size);
            p += row->size;
        }
    }

    return (int)(p - buf);
}

int editor_model_serialize(const EditorModel *model, char **out_buf, size_t *out_len) {
    if (!model || !out_buf || !out_len) return -1;

    size_t size = editor_model_serialized_size(model);
    char *buf = malloc(size);
    if (!buf) return -1;

    int written = editor_model_serialize_to_buf(model, buf, size);
    if (written < 0) {
        free(buf);
        return -1;
    }

    *out_buf = buf;
    *out_len = (size_t)written;
    return 0;
}

/* Helper: free existing rows in model */
static void free_model_rows(EditorModel *model) {
    if (!model->row) return;

    for (int i = 0; i < model->numrows; i++) {
        free(model->row[i].chars);
        free(model->row[i].render);
        free(model->row[i].hl);
    }
    free(model->row);
    model->row = NULL;
    model->numrows = 0;
}

int editor_model_deserialize(EditorModel *model, const char *data, size_t len) {
    if (!model || !data) return -1;
    if (len < HEADER_SIZE) return -1;

    const char *p = data;
    const char *end = data + len;

    /* Check magic */
    uint32_t magic = read_u32(p);
    p += 4;
    if (magic != LOKI_SERIALIZE_MAGIC) return -1;

    /* Check version */
    uint16_t version = read_u16(p);
    p += 2;
    if (version > LOKI_SERIALIZE_VERSION) return -1;  /* Future version */

    /* Read filename */
    if (p + 4 > end) return -1;
    uint32_t filename_len = read_u32(p);
    p += 4;

    if (p + filename_len > end) return -1;

    /* Free existing filename */
    free(model->filename);
    model->filename = NULL;

    if (filename_len > 0) {
        model->filename = malloc(filename_len + 1);
        if (!model->filename) return -1;
        memcpy(model->filename, p, filename_len);
        model->filename[filename_len] = '\0';
        p += filename_len;
    }

    /* Read flags */
    if (p + 1 > end) return -1;
    model->dirty = (*p++ != 0);

    /* Read rows */
    if (p + 4 > end) return -1;
    uint32_t numrows = read_u32(p);
    p += 4;

    /* Free existing rows */
    free_model_rows(model);

    /* Allocate new rows */
    if (numrows > 0) {
        model->row = calloc(numrows, sizeof(t_erow));
        if (!model->row) return -1;
    }
    model->numrows = (int)numrows;

    /* Read each row */
    for (uint32_t i = 0; i < numrows; i++) {
        if (p + 4 > end) {
            free_model_rows(model);
            return -1;
        }

        uint32_t row_size = read_u32(p);
        p += 4;

        if (p + row_size > end) {
            free_model_rows(model);
            return -1;
        }

        t_erow *row = &model->row[i];
        row->idx = (int)i;
        row->size = (int)row_size;
        row->rsize = 0;
        row->render = NULL;
        row->hl = NULL;
        row->hl_oc = 0;
        row->cb_lang = 0;
        row->csd_section = 0;

        if (row_size > 0) {
            row->chars = malloc(row_size + 1);
            if (!row->chars) {
                free_model_rows(model);
                return -1;
            }
            memcpy(row->chars, p, row_size);
            row->chars[row_size] = '\0';
            p += row_size;
        } else {
            row->chars = malloc(1);
            if (!row->chars) {
                free_model_rows(model);
                return -1;
            }
            row->chars[0] = '\0';
        }
    }

    return 0;
}

int editor_model_save_snapshot(const EditorModel *model, const char *path) {
    if (!model || !path) return -1;

    char *buf = NULL;
    size_t len = 0;

    if (editor_model_serialize(model, &buf, &len) != 0) {
        return -1;
    }

    FILE *f = fopen(path, "wb");
    if (!f) {
        free(buf);
        return -1;
    }

    size_t written = fwrite(buf, 1, len, f);
    fclose(f);
    free(buf);

    return (written == len) ? 0 : -1;
}

int editor_model_load_snapshot(EditorModel *model, const char *path) {
    if (!model || !path) return -1;

    FILE *f = fopen(path, "rb");
    if (!f) return -1;

    /* Get file size */
    fseek(f, 0, SEEK_END);
    long file_size = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (file_size <= 0) {
        fclose(f);
        return -1;
    }

    /* Read file */
    char *buf = malloc((size_t)file_size);
    if (!buf) {
        fclose(f);
        return -1;
    }

    size_t read_size = fread(buf, 1, (size_t)file_size, f);
    fclose(f);

    if (read_size != (size_t)file_size) {
        free(buf);
        return -1;
    }

    /* Deserialize */
    int result = editor_model_deserialize(model, buf, read_size);
    free(buf);

    return result;
}
