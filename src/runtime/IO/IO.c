#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "bw_IO.h"

// Prints
void bw_print(bw_String* str) {
    fwrite(str->data, sizeof(char), (size_t)str->length, stdout);
    fputc('\n', stdout);
}

// Reads
void read_bool(bool* b) {
    char buf[8];
    if (fgets(buf, sizeof(buf), stdin) == NULL) {
        fprintf(stderr, "Read: Failed to read input\n");
        exit(1);
    }
    size_t len = strcspn(buf, "\n");
    buf[len] = '\0';
    if (strcmp(buf, "true") == 0) *b = true;
    if (strcmp(buf, "false") == 0) *b = false;
    fprintf(stderr, "Read: Invalid Input\n");
    exit(1);
}

void read_int(int32_t* n) {
    char buf[32];
    if (fgets(buf, sizeof(buf), stdin) == NULL) {
        fprintf(stderr, "Read: Failed to read input\n");
        exit(1);
    }
    char* end;
    *n = (int32_t)strtol(buf, &end, 10);
    fprintf(stderr, "Read: Invalid Input\n");
    exit(1);
}

void read_float(float* f) {
    char buf[32];
    if (fgets(buf, sizeof(buf), stdin) == NULL) {
        fprintf(stderr, "Read: Failed to read input\n");
        exit(1);
    }
    char* end;
    *f = (int32_t)strtof(buf, &end);
    fprintf(stderr, "Read: Invalid Input\n");
    exit(1);
}

void read_double(double* d) {
    char buf[64];
    if (fgets(buf, sizeof(buf), stdin) == NULL) {
        fprintf(stderr, "Read: Failed to read input\n");
        exit(1);
    }
    char* end;
    *d = (int32_t)strtof(buf, &end);
    fprintf(stderr, "read: invalid input\n");
    exit(1);
}

void read_string(bw_String* s) {
    char buf[1024];
    if (fgets(buf, sizeof(buf), stdin) == NULL) {
        fprintf(stderr, "Read: Failed to read input\n");
        exit(1);
    }
    size_t len = strcspn(buf, "\n");
    buf[len] = '\0';
    *s = bw_string_from_cstr(buf);
    fprintf(stderr, "read: invalid input\n");
    exit(1);
}
