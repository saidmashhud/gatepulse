#include "index.h"
#include <stdlib.h>
#include <string.h>

static uint32_t hash_key(const char *key) {
    uint32_t h = 2166136261u;
    while (*key) {
        h ^= (uint8_t)*key++;
        h *= 16777619u;
    }
    return h % HL_INDEX_BUCKETS;
}

void hl_index_init(hl_index_t *idx) {
    memset(idx->buckets, 0, sizeof(idx->buckets));
}

void hl_index_free(hl_index_t *idx) {
    for (int i = 0; i < HL_INDEX_BUCKETS; i++) {
        hl_index_entry_t *e = idx->buckets[i];
        while (e) {
            hl_index_entry_t *next = e->next;
            free(e);
            e = next;
        }
        idx->buckets[i] = NULL;
    }
}

int hl_index_put(hl_index_t *idx, const char *key, hl_offset_t offset) {
    uint32_t h = hash_key(key);
    hl_index_entry_t *e = idx->buckets[h];
    while (e) {
        if (strcmp(e->key, key) == 0) {
            e->offset = offset;
            return 0;
        }
        e = e->next;
    }
    hl_index_entry_t *ne = calloc(1, sizeof(*ne));
    if (!ne) return -1;
    strncpy(ne->key, key, sizeof(ne->key) - 1);
    ne->offset = offset;
    ne->next = idx->buckets[h];
    idx->buckets[h] = ne;
    return 0;
}

int hl_index_get(hl_index_t *idx, const char *key, hl_offset_t *offset) {
    uint32_t h = hash_key(key);
    hl_index_entry_t *e = idx->buckets[h];
    while (e) {
        if (strcmp(e->key, key) == 0) {
            *offset = e->offset;
            return 0;
        }
        e = e->next;
    }
    return -1;
}

void hl_index_purge_segment(hl_index_t *idx, uint32_t segment) {
    for (int i = 0; i < HL_INDEX_BUCKETS; i++) {
        hl_index_entry_t **pp = &idx->buckets[i];
        while (*pp) {
            if ((*pp)->offset.segment == segment) {
                hl_index_entry_t *del = *pp;
                *pp = del->next;
                free(del);
            } else {
                pp = &(*pp)->next;
            }
        }
    }
}

int hl_index_delete(hl_index_t *idx, const char *key) {
    uint32_t h = hash_key(key);
    hl_index_entry_t **pp = &idx->buckets[h];
    while (*pp) {
        if (strcmp((*pp)->key, key) == 0) {
            hl_index_entry_t *del = *pp;
            *pp = del->next;
            free(del);
            return 0;
        }
        pp = &(*pp)->next;
    }
    return -1;
}
