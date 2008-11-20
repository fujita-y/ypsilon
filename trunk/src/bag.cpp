/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "object.h"
#include "bag.h"

static inline bool
slot_match(const sharedbag_slot_t* datum, const char* key, int len)
{
    return (*(uint32_t*)(datum->key) == len) && (memcmp((char*)datum->key + sizeof(uint32_t), key, len) == 0);
}

static inline bool
slot_allocate(sharedbag_slot_t* datum, const char* key, int len)
{
    datum->key = (char*)malloc(sizeof(uint32_t) + len);
    if (datum->key == NULL) fatal("%s:%u memory overflow", __FILE__, __LINE__);
    *(uint32_t*)(datum->key) = len;
    memcpy((char*)datum->key + sizeof(uint32_t), key, len);
}

sharedbag_slot_t*
lookup_sharedbag(scm_sharedbag_t bag, const char* key, int len)
{
    scoped_lock lock(bag->lock);
    int slot = -1;
    for (int i = 0; i < bag->capacity; i++) {
        sharedbag_slot_t* datum = bag->datum[i];
        if (datum->key == NULL) {
            slot = i;
            continue;
        }
        if (slot_match(datum, key, len)) {
            slot = i;
            break;
        }
    }
    if (slot < 0) {
        for (int i = 0; i < bag->capacity; i++) {
            if (bag->datum[i]->buf.empty()) {
                free(bag->datum[i]->key);
                bag->datum[i]->key = NULL;
                slot = i;
            }
        }
        if (slot < 0) {
            int prev = bag->capacity;
            bag->capacity += bag->capacity;
            bag->datum = (sharedbag_slot_t**)realloc(bag->datum, sizeof(sharedbag_slot_t*) * bag->capacity);
            if (bag->datum == NULL) fatal("%s:%u memory overflow",__FILE__ , __LINE__);
            for (int i = prev; i < bag->capacity; i++) {
                bag->datum[i]->key = NULL;
                bag->datum[i]->buf.init(bag->depth + MAX_VIRTUAL_MACHINE);
                bag->datum[i]->queue.init(bag->depth);
            }
            slot = prev;
        }
    }
    if (bag->datum[slot]->key == NULL) slot_allocate(bag->datum[slot], key, len);
    return bag->datum[slot];
}
