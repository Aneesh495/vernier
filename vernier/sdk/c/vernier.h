/**
 * vernier.h
 * Zero-dependency C99 client SDK for the Vernier distributed tracing engine.
 *
 * Provides high-resolution nanosecond span instrumentation, W3C TraceContext
 * generation, and JSON payload serialization for systems programming.
 */

#ifndef VERNIER_H
#define VERNIER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define VERNIER_TRACE_ID_HEX_LEN 32
#define VERNIER_SPAN_ID_HEX_LEN  16
#define VERNIER_MAX_ATTRS        32
#define VERNIER_MAX_STR_LEN      128

typedef enum {
    VERNIER_SPAN_INTERNAL = 0,
    VERNIER_SPAN_SERVER,
    VERNIER_SPAN_CLIENT,
    VERNIER_SPAN_PRODUCER,
    VERNIER_SPAN_CONSUMER
} vernier_span_kind_t;

typedef enum {
    VERNIER_STATUS_UNSET = 0,
    VERNIER_STATUS_OK,
    VERNIER_STATUS_ERROR
} vernier_status_t;

typedef enum {
    VERNIER_ATTR_STRING = 0,
    VERNIER_ATTR_INT,
    VERNIER_ATTR_DOUBLE,
    VERNIER_ATTR_BOOL
} vernier_attr_type_t;

typedef struct {
    char key[VERNIER_MAX_STR_LEN];
    vernier_attr_type_t type;
    union {
        char str_val[VERNIER_MAX_STR_LEN];
        int64_t int_val;
        double double_val;
        bool bool_val;
    } val;
} vernier_attribute_t;

typedef struct vernier_span {
    char trace_id[VERNIER_TRACE_ID_HEX_LEN + 1];
    char span_id[VERNIER_SPAN_ID_HEX_LEN + 1];
    char parent_id[VERNIER_SPAN_ID_HEX_LEN + 1];
    bool has_parent;

    char name[VERNIER_MAX_STR_LEN];
    char service_name[VERNIER_MAX_STR_LEN];
    vernier_span_kind_t kind;

    int64_t start_time_ns;
    int64_t end_time_ns;
    int64_t duration_ns;

    vernier_status_t status;
    char status_message[VERNIER_MAX_STR_LEN];

    vernier_attribute_t attributes[VERNIER_MAX_ATTRS];
    size_t attr_count;
} vernier_span_t;

typedef struct vernier_tracer {
    char service_name[VERNIER_MAX_STR_LEN];
    char collector_host[VERNIER_MAX_STR_LEN];
    uint16_t collector_port;
} vernier_tracer_t;

/* Time utility */
int64_t vernier_time_nanos(void);

/* Tracer lifecycle */
vernier_tracer_t* vernier_tracer_create(const char* service_name, const char* host, uint16_t port);
void vernier_tracer_destroy(vernier_tracer_t* tracer);

/* Span lifecycle */
void vernier_span_start(vernier_tracer_t* tracer,
                        vernier_span_t* span,
                        const char* name,
                        vernier_span_kind_t kind,
                        const vernier_span_t* parent);

void vernier_span_end(vernier_span_t* span);

void vernier_span_set_status(vernier_span_t* span, vernier_status_t status, const char* message);

/* Attribute setters */
bool vernier_span_set_attribute_str(vernier_span_t* span, const char* key, const char* value);
bool vernier_span_set_attribute_int(vernier_span_t* span, const char* key, int64_t value);
bool vernier_span_set_attribute_double(vernier_span_t* span, const char* key, double value);
bool vernier_span_set_attribute_bool(vernier_span_t* span, const char* key, bool value);

/* Serialization */
int vernier_span_serialize_json(const vernier_span_t* span, char* buf, size_t buf_size);

/* W3C Header helpers */
int vernier_span_inject_w3c(const vernier_span_t* span, char* header_buf, size_t buf_size);

#ifdef __cplusplus
}
#endif

#endif /* VERNIER_H */
