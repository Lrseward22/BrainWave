#pragma once
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif
    
typedef struct {
    int32_t length;
    char* data;
} bw_String;

bw_String bw_string_from_cstr(const char* cstr);
int32_t bw_string_length(const bw_String* s);
bw_String bw_string_concat(const bw_String* a, const bw_String* b);
bw_String bw_string_add(const bw_String* a, const bw_String* b);
bw_String bw_string_at(bw_String* s, int32_t index);
bw_String bw_string_slice(bw_String* s, int32_t start, int32_t stop);
int32_t bw_string_compare(bw_String* a, bw_String* b);
int32_t bw_string_eq(bw_String* a, bw_String* b);
int32_t bw_string_ne(bw_String* a, bw_String* b);
int32_t bw_string_gt(bw_String* a, bw_String* b);
int32_t bw_string_ge(bw_String* a, bw_String* b);
int32_t bw_string_lt(bw_String* a, bw_String* b);
int32_t bw_string_le(bw_String* a, bw_String* b);

#ifdef __cplusplus
}
#endif
