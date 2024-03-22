#ifndef __NRDL_H__
#define __NRDL_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

typedef enum {
    NRDL_STREAM_START,
    NRDL_STREAM_END,
    NRDL_ARRAY_START,
    NRDL_ARRAY_END,
    NRDL_OBJECT_START,
    NRDL_OBJECT_END,
    NRDL_STRING,
    NRDL_SYMBOL,
    NRDL_NUMBER,
    NRDL_BOOLEAN,
    NRDL_NULL_VALUE,
} nrdl_type_t;

typedef struct {
    nrdl_type_t type;
    char *value;
    size_t length;
} nrdl_event_t;

#define NRDL_SCALAR_MAX_LENGTH 32768

typedef struct {
    FILE *stream;
    char *buffer;
    size_t buffer_size;
    size_t buffer_length;
    size_t buffer_offset;
    char next_char;
    nrdl_token token;
} nrdl_parser_t;

void nrdl_parser_init(nrdl_parser_t *parser);
void nrdl_parser_destroy(nrdl_parser_t *parser);
void nrdl_parser_set_stream(nrdl_parser_t *parser, FILE *stream);
void nrdl_parser_set_buffer(nrdl_parser_t *parser, char *buffer, size_t buffer_size);
int nrdl_parser_parse(nrdl_parser_t *parser, nrdl_event_t *token);
void nrdl_event_destroy(nrdl_event_t *event);
void nrdl_parser_destroy(nrdl_parser_t *parser);

/* TODO: Add the emitter API. */

#ifdef __cplusplus
}
#endif

#endif