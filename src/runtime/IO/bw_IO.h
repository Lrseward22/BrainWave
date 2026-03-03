#pragma once
#include <stdint.h>
#include "../String/bw_string.h"

#ifdef __cplusplus
extern "C" {
#endif

// Prints
void bw_print(bw_String* str);

// Reads
void read_bool(bool* b);
void read_int(int32_t* n);
void read_float(float* f);
void read_double(double* f);
void read_string(bw_String* s);

#ifdef __cplusplus
}
#endif
