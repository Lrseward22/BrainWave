#pragma once
#include <stdint.h>
#include "../String/bw_string.h"

#ifdef __cplusplus
extern "C" {
#endif

bool to_bool(bw_String s);
int32_t to_int(bw_String s);
float to_float(bw_String s);
double to_double(bw_String s);
bw_String to_string_from_int(int32_t n);
bw_String to_string_from_float(float f);
bw_String to_string_from_double(double d);
bw_String to_string_from_bool(bool b);
bw_String to_string_from_Class(bw_String s, void* ptr);

#ifdef __cplusplus
}
#endif
