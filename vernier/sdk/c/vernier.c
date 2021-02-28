/**
 * vernier.c
 * Implementation of zero-dependency C99 client SDK for Vernier.
 */

#include "vernier.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if defined(__APPLE__) || defined(__linux__) || defined(__unix__)
#include <fcntl.h>
#include <unistd.h>
#define VERNIER_HAS_URANDOM 1
#else
#define VERNIER_HAS_URANDOM 0
#endif

/* Random hex helper */
static void generate_hex(char* out, size_t num_bytes) {
    static const char hex_digits[] = "0123456789abcdef";
    uint8_t raw[16];
    size_t clamped = num_bytes > sizeof(raw) ? sizeof(raw) : num_bytes;

#if VERNIER_HAS_URANDOM
    int fd = open("/dev/urandom", O_RDONLY);
    if (fd >= 0) {
        ssize_t r = read(fd, raw, clamped);
        close(fd);
        if (r == (ssize_t)clamped) {
            for (size_t i = 0; i < clamped; ++i) {
                out[i * 2]     = hex_digits[(raw[i] >> 4) & 0x0F];
                out[i * 2 + 1] = hex_digits[raw[i] & 0x0F];
            }
            out[clamped * 2] = '\0';
            return;
        }
    }
#endif

    /* Pseudo-random fallback */
    for (size_t i = 0; i < clamped; ++i) {
        int val = rand() % 256;
        out[i * 2]     = hex_digits[(val >> 4) & 0x0F];
        out[i * 2 + 1] = hex_digits[val & 0x0F];
    }
    out[clamped * 2] = '\0';
}

int64_t vernier_time_nanos(void) {
#if defined(CLOCK_REALTIME)
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return ((int64_t)ts.tv_sec * 1000000000LL) + (int64_t)ts.tv_nsec;
#else
    return (int64_t)time(NULL) * 1000000000LL;
#endif
}

vernier_tracer_t* vernier_tracer_create(const char* service_name, const char* host, uint16_t port) {
    vernier_tracer_t* t = (vernier_tracer_t*)malloc(sizeof(vernier_tracer_t));
    if (!t) return NULL;

    strncpy(t->service_name, service_name ? service_name : "unknown-c-service", VERNIER_MAX_STR_LEN - 1);
    t->service_name[VERNIER_MAX_STR_LEN - 1] = '\0';

    strncpy(t->collector_host, host ? host : "127.0.0.1", VERNIER_MAX_STR_LEN - 1);
    t->collector_host[VERNIER_MAX_STR_LEN - 1] = '\0';

    t->collector_port = port ? port : 9876;
    return t;
}

void vernier_tracer_destroy(vernier_tracer_t* tracer) {
    if (tracer) {
        free(tracer);
    }
}

void vernier_span_start(vernier_tracer_t* tracer,
                        vernier_span_t* span,
                        const char* name,
                        vernier_span_kind_t kind,
                        const vernier_span_t* parent) {
    if (!span) return;
    memset(span, 0, sizeof(vernier_span_t));

    if (parent && parent->trace_id[0] != '\0') {
        strncpy(span->trace_id, parent->trace_id, VERNIER_TRACE_ID_HEX_LEN);
        span->trace_id[VERNIER_TRACE_ID_HEX_LEN] = '\0';

        strncpy(span->parent_id, parent->span_id, VERNIER_SPAN_ID_HEX_LEN);
        span->parent_id[VERNIER_SPAN_ID_HEX_LEN] = '\0';
        span->has_parent = true;
    } else {
        generate_hex(span->trace_id, 16);
        span->has_parent = false;
        span->parent_id[0] = '\0';
    }

    generate_hex(span->span_id, 8);

    const char* svc = tracer ? tracer->service_name : "c-client";
    strncpy(span->service_name, svc, VERNIER_MAX_STR_LEN - 1);
    span->service_name[VERNIER_MAX_STR_LEN - 1] = '\0';

    strncpy(span->name, name ? name : "unnamed_operation", VERNIER_MAX_STR_LEN - 1);
    span->name[VERNIER_MAX_STR_LEN - 1] = '\0';

    span->kind = kind;
    span->status = VERNIER_STATUS_UNSET;
    span->start_time_ns = vernier_time_nanos();
}

void vernier_span_end(vernier_span_t* span) {
    if (!span) return;
    span->end_time_ns = vernier_time_nanos();
    span->duration_ns = span->end_time_ns - span->start_time_ns;
    if (span->duration_ns < 0) {
        span->duration_ns = 0;
    }
    if (span->status == VERNIER_STATUS_UNSET) {
        span->status = VERNIER_STATUS_OK;
    }
}

void vernier_span_set_status(vernier_span_t* span, vernier_status_t status, const char* message) {
    if (!span) return;
    span->status = status;
    if (message) {
        strncpy(span->status_message, message, VERNIER_MAX_STR_LEN - 1);
        span->status_message[VERNIER_MAX_STR_LEN - 1] = '\0';
    } else {
        span->status_message[0] = '\0';
    }
}

bool vernier_span_set_attribute_str(vernier_span_t* span, const char* key, const char* value) {
    if (!span || !key || !value || span->attr_count >= VERNIER_MAX_ATTRS) return false;
    vernier_attribute_t* attr = &span->attributes[span->attr_count++];
    strncpy(attr->key, key, VERNIER_MAX_STR_LEN - 1);
    attr->key[VERNIER_MAX_STR_LEN - 1] = '\0';
    attr->type = VERNIER_ATTR_STRING;
    strncpy(attr->val.str_val, value, VERNIER_MAX_STR_LEN - 1);
    attr->val.str_val[VERNIER_MAX_STR_LEN - 1] = '\0';
    return true;
}

bool vernier_span_set_attribute_int(vernier_span_t* span, const char* key, int64_t value) {
    if (!span || !key || span->attr_count >= VERNIER_MAX_ATTRS) return false;
    vernier_attribute_t* attr = &span->attributes[span->attr_count++];
    strncpy(attr->key, key, VERNIER_MAX_STR_LEN - 1);
    attr->key[VERNIER_MAX_STR_LEN - 1] = '\0';
    attr->type = VERNIER_ATTR_INT;
    attr->val.int_val = value;
    return true;
}

bool vernier_span_set_attribute_double(vernier_span_t* span, const char* key, double value) {
    if (!span || !key || span->attr_count >= VERNIER_MAX_ATTRS) return false;
    vernier_attribute_t* attr = &span->attributes[span->attr_count++];
    strncpy(attr->key, key, VERNIER_MAX_STR_LEN - 1);
    attr->key[VERNIER_MAX_STR_LEN - 1] = '\0';
    attr->type = VERNIER_ATTR_DOUBLE;
    attr->val.double_val = value;
    return true;
}

bool vernier_span_set_attribute_bool(vernier_span_t* span, const char* key, bool value) {
    if (!span || !key || span->attr_count >= VERNIER_MAX_ATTRS) return false;
    vernier_attribute_t* attr = &span->attributes[span->attr_count++];
    strncpy(attr->key, key, VERNIER_MAX_STR_LEN - 1);
    attr->key[VERNIER_MAX_STR_LEN - 1] = '\0';
    attr->type = VERNIER_ATTR_BOOL;
    attr->val.bool_val = value;
    return true;
}

static const char* status_to_str(vernier_status_t s) {
    switch (s) {
        case VERNIER_STATUS_OK: return "ok";
        case VERNIER_STATUS_ERROR: return "error";
        default: return "unset";
    }
}

static const char* kind_to_str(vernier_span_kind_t k) {
    switch (k) {
        case VERNIER_SPAN_SERVER: return "server";
        case VERNIER_SPAN_CLIENT: return "client";
        case VERNIER_SPAN_PRODUCER: return "producer";
        case VERNIER_SPAN_CONSUMER: return "consumer";
        default: return "internal";
    }
}

int vernier_span_serialize_json(const vernier_span_t* span, char* buf, size_t buf_size) {
    if (!span || !buf || buf_size == 0) return -1;

    char attr_buf[2048] = "{";
    size_t attr_len = 1;

    for (size_t i = 0; i < span->attr_count; ++i) {
        const vernier_attribute_t* a = &span->attributes[i];
        char pair[256];
        int written = 0;

        switch (a->type) {
            case VERNIER_ATTR_STRING:
                written = snprintf(pair, sizeof(pair), "\"%s\":\"%s\"", a->key, a->val.str_val);
                break;
            case VERNIER_ATTR_INT:
                written = snprintf(pair, sizeof(pair), "\"%s\":%lld", a->key, (long long)a->val.int_val);
                break;
            case VERNIER_ATTR_DOUBLE:
                written = snprintf(pair, sizeof(pair), "\"%s\":%f", a->key, a->val.double_val);
                break;
            case VERNIER_ATTR_BOOL:
                written = snprintf(pair, sizeof(pair), "\"%s\":%s", a->key, a->val.bool_val ? "true" : "false");
                break;
        }

        if (written > 0 && attr_len + (size_t)written + 2 < sizeof(attr_buf)) {
            if (attr_len > 1) {
                attr_buf[attr_len++] = ',';
            }
            memcpy(attr_buf + attr_len, pair, (size_t)written);
            attr_len += (size_t)written;
        }
    }
    attr_buf[attr_len++] = '}';
    attr_buf[attr_len] = '\0';

    char parent_part[64];
    if (span->has_parent) {
        snprintf(parent_part, sizeof(parent_part), "\"parent_id\":\"%s\",", span->parent_id);
    } else {
        snprintf(parent_part, sizeof(parent_part), "\"parent_id\":null,");
    }

    return snprintf(buf, buf_size,
                    "{"
                    "\"trace_id\":\"%s\","
                    "\"span_id\":\"%s\","
                    "%s"
                    "\"name\":\"%s\","
                    "\"service_name\":\"%s\","
                    "\"kind\":\"%s\","
                    "\"start_time\":%lld,"
                    "\"end_time\":%lld,"
                    "\"duration\":%lld,"
                    "\"status_code\":\"%s\","
                    "\"status_message\":\"%s\","
                    "\"attributes\":%s,"
                    "\"events\":[],"
                    "\"links\":[]"
                    "}",
                    span->trace_id,
                    span->span_id,
                    parent_part,
                    span->name,
                    span->service_name,
                    kind_to_str(span->kind),
                    (long long)span->start_time_ns,
                    (long long)span->end_time_ns,
                    (long long)span->duration_ns,
                    status_to_str(span->status),
                    span->status_message,
                    attr_buf);
}

int vernier_span_inject_w3c(const vernier_span_t* span, char* header_buf, size_t buf_size) {
    if (!span || !header_buf) return -1;
    return snprintf(header_buf, buf_size, "00-%s-%s-01", span->trace_id, span->span_id);
}
