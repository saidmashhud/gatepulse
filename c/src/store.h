#ifndef HL_STORE_H
#define HL_STORE_H

#include <stdint.h>
#include <stddef.h>

/* Record magic bytes */
#define HL_MAGIC        0x47505354u  /* "GPST" */
#define HL_VERSION      1

/* Record types */
#define HL_TYPE_EVENT        1
#define HL_TYPE_JOB          2
#define HL_TYPE_ATTEMPT      3
#define HL_TYPE_DLQ          4
#define HL_TYPE_ENDPOINT     5
#define HL_TYPE_SUBSCRIPTION 6
#define HL_TYPE_ACK          7   /* job ACK tombstone for crash recovery */
#define HL_TYPE_AUDIT     0x09
#define HL_TYPE_TOMBSTONE    8   /* delete tombstone for endpoints/subscriptions */

/* Segment size: 256MB */
#define HL_SEGMENT_SIZE  (256 * 1024 * 1024)

/* Record header layout:
 *   [magic:4][version:1][type:1][length:4][payload:N][crc32:4]
 */
#define HL_RECORD_HEADER_SIZE 10
#define HL_RECORD_FOOTER_SIZE 4
#define HL_RECORD_OVERHEAD    (HL_RECORD_HEADER_SIZE + HL_RECORD_FOOTER_SIZE)

typedef struct {
    char     *data_dir;
    int       current_seg;   /* current segment index */
    int       seg_fd;        /* current segment file descriptor */
    uint64_t  seg_offset;    /* write offset within current segment */
} hl_store_t;

typedef struct {
    uint32_t magic;
    uint8_t  version;
    uint8_t  type;
    uint32_t length;
    /* payload follows */
    /* crc32 follows payload */
} __attribute__((packed)) hl_record_hdr_t;

/* Store offset: segment + offset within segment */
typedef struct {
    uint32_t segment;
    uint64_t offset;
} hl_offset_t;

int  hl_store_init(hl_store_t *store, const char *data_dir);
void hl_store_close(hl_store_t *store);

/* Returns byte offset of the appended record */
int  hl_store_append(hl_store_t *store, uint8_t type,
                     const uint8_t *payload, uint32_t length,
                     hl_offset_t *out_offset);

/* Read payload at offset; caller frees *payload */
int  hl_store_read(hl_store_t *store, hl_offset_t offset,
                   uint8_t **payload, uint32_t *length, uint8_t *type);

uint32_t hl_crc32(const uint8_t *data, size_t length);

#endif /* HL_STORE_H */
