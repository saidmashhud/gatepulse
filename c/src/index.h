#ifndef HL_INDEX_H
#define HL_INDEX_H

#include "store.h"
#include <stdint.h>

#define HL_INDEX_BUCKETS 65536

typedef struct hl_index_entry {
    char              key[128];
    hl_offset_t       offset;
    struct hl_index_entry *next;
} hl_index_entry_t;

typedef struct {
    hl_index_entry_t *buckets[HL_INDEX_BUCKETS];
} hl_index_t;

void hl_index_init(hl_index_t *idx);
void hl_index_free(hl_index_t *idx);

int  hl_index_put(hl_index_t *idx, const char *key, hl_offset_t offset);
int  hl_index_get(hl_index_t *idx, const char *key, hl_offset_t *offset);
int  hl_index_delete(hl_index_t *idx, const char *key);
void hl_index_purge_segment(hl_index_t *idx, uint32_t segment);

#endif /* HL_INDEX_H */
