#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "bw_cast.h"

bool to_bool(bw_String s) {
    if (s.length == 4 && memcmp(s.data, "true", 4) == 0) return true;
    if (s.length == 5 && memcmp(s.data, "false", 5) == 0) return false;
    return to_int(s) != 0;
}

int32_t to_int(bw_String s) {
    return (int32_t)strtol(s.data, NULL, 10);
}

float to_float(bw_String s) {
    return strtof(s.data, NULL);
}

double to_double(bw_String s) {
    return strtod(s.data, NULL);
}

bw_String to_string_from_int(int32_t n) {
    char buf[32];
    snprintf(buf, sizeof(buf), "%d", n);
    return bw_string_from_cstr(buf);
}

bw_String to_string_from_float(float f) {
    char buf[32];
    snprintf(buf, sizeof(buf), "%g", f);
    return bw_string_from_cstr(buf);
}

bw_String to_string_from_double(double d) {
    char buf[64];
    snprintf(buf, sizeof(buf), "%g", d);
    return bw_string_from_cstr(buf);
}

bw_String to_string_from_bool(bool b) {
    return b ? bw_string_from_cstr("true") : bw_string_from_cstr("false");
}

bw_String to_string_from_Class(bw_String s, void* ptr) {
    char buf[64];
    snprintf(buf, sizeof(buf), "%p", ptr);
    bw_String colon = bw_string_from_cstr(": ");
    bw_String pstr = bw_string_from_cstr(buf);
    bw_String temp = bw_string_concat(&s, &colon);
    bw_String result = bw_string_concat(&temp, &pstr);
    bw_string_free(&colon);
    bw_string_free(&pstr);
    bw_string_free(&temp);
    return result;
}
