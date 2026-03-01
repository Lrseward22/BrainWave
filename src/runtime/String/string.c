#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "bw_string.h"

static bw_String bw_string_alloc(int32_t len) {
    bw_String s;
    s.length = len;
    s.data = (char*)malloc((size_t)len);

    if (len > 0 && !s.data) {
        fprintf(stderr, "String Allocation: Out of memory\n");
        exit(1);
    }

    return s;
}

void bw_string_free(bw_String* s) {
    if (s->data) {
        free(s->data);
        s->data = NULL;
        s->length = 0;
    }
}

bw_String bw_string_from_cstr(const char* cstr) {
    if (!cstr) {
        bw_String empty = {0, NULL};
        return empty;
    }

    int32_t len = (int32_t)strlen(cstr);
    bw_String s = bw_string_alloc(len);
    memcpy(s.data, cstr, (size_t)len);
    return s;
}

int32_t bw_string_length(const bw_String* s) {
    return s->length;
}

bw_String bw_string_concat(const bw_String* a, const bw_String* b) {
    int32_t len = a->length + b->length;
    bw_String s = bw_string_alloc(len);

    if (a->length > 0)
        memcpy(s.data, a->data, (size_t)a->length);

    if (b->length > 0)
        memcpy(s.data + a->length, b->data, (size_t)b->length);

    return s;
}

bw_String bw_string_add(const bw_String* a, const bw_String* b) {
    return bw_string_concat(a, b);
}

bw_String bw_string_at(bw_String* s, int32_t index) {
    if (index < 0 || index >= s->length) {
        index %= s->length;
    }
    bw_String ret = bw_string_alloc(1);
    *(ret.data) = s->data[index];
    return ret;
}

bw_String bw_string_slice(bw_String* s, int32_t start, int32_t stop) {
    if (start < 0) start = 0;
    if (stop > s->length) stop = s->length;
    if (start >= stop) {
        bw_String empty = {0, NULL};
        return empty;
    }

    int32_t len = stop-start;
    bw_String res = bw_string_alloc(len);
    memcpy(res.data, s->data+start, (size_t)len);
    return res;
}

int32_t bw_string_compare(bw_String* a, bw_String* b) {
    int32_t minLen = a->length < b->length ? a->length : b->length;
    if (minLen > 0) {
        int cmp = memcmp(a->data, b->data, (size_t)minLen);
        return cmp;
    }
    return a->length - b->length;
}

int32_t bw_string_eq(bw_String* a, bw_String* b) {
    return bw_string_compare(a, b) == 0;
}
int32_t bw_string_ne(bw_String* a, bw_String* b) {
    return bw_string_compare(a, b) != 0;
}
int32_t bw_string_gt(bw_String* a, bw_String* b) {
    return bw_string_compare(a, b) > 0;
}
int32_t bw_string_ge(bw_String* a, bw_String* b) {
    return bw_string_compare(a, b) >= 0;
}
int32_t bw_string_lt(bw_String* a, bw_String* b) {
    return bw_string_compare(a, b) < 0;
}
int32_t bw_string_le(bw_String* a, bw_String* b) {
    return bw_string_compare(a, b) <= 0;
}
