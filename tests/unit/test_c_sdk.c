/**
 * test_c_sdk.c
 * Unit test suite for C99 client SDK.
 */

#include "../../vernier/sdk/c/vernier.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

int main(void) {
    printf("Running Vernier C99 SDK Unit Tests...\n");

    /* 1. Initialize tracer */
    vernier_tracer_t* tracer = vernier_tracer_create("order-gateway", "127.0.0.1", 9876);
    assert(tracer != NULL);
    assert(strcmp(tracer->service_name, "order-gateway") == 0);

    /* 2. Start root span */
    vernier_span_t root_span;
    vernier_span_start(tracer, &root_span, "checkout_order", VERNIER_SPAN_SERVER, NULL);
    assert(strlen(root_span.trace_id) == 32);
    assert(strlen(root_span.span_id) == 16);
    assert(!root_span.has_parent);

    /* Set attributes */
    bool ok_attr1 = vernier_span_set_attribute_str(&root_span, "customer.tier", "platinum");
    bool ok_attr2 = vernier_span_set_attribute_int(&root_span, "cart.items", 5);
    bool ok_attr3 = vernier_span_set_attribute_double(&root_span, "cart.total", 149.50);
    assert(ok_attr1 && ok_attr2 && ok_attr3);
    assert(root_span.attr_count == 3);

    /* 3. Start child span with context propagation */
    vernier_span_t child_span;
    vernier_span_start(tracer, &child_span, "authorize_payment", VERNIER_SPAN_CLIENT, &root_span);
    assert(child_span.has_parent);
    assert(strcmp(child_span.trace_id, root_span.trace_id) == 0);
    assert(strcmp(child_span.parent_id, root_span.span_id) == 0);

    /* End child span */
    vernier_span_end(&child_span);
    assert(child_span.status == VERNIER_STATUS_OK);
    assert(child_span.duration_ns >= 0);

    /* End root span with error */
    vernier_span_set_status(&root_span, VERNIER_STATUS_ERROR, "payment declined");
    vernier_span_end(&root_span);
    assert(root_span.status == VERNIER_STATUS_ERROR);

    /* 4. Verify W3C header injection */
    char w3c_buf[128];
    int w3c_len = vernier_span_inject_w3c(&root_span, w3c_buf, sizeof(w3c_buf));
    assert(w3c_len > 0);
    assert(strncmp(w3c_buf, "00-", 3) == 0);

    /* 5. Verify JSON serialization */
    char json_buf[4096];
    int json_len = vernier_span_serialize_json(&root_span, json_buf, sizeof(json_buf));
    assert(json_len > 0);
    assert(strstr(json_buf, "\"name\":\"checkout_order\"") != NULL);
    assert(strstr(json_buf, "\"service_name\":\"order-gateway\"") != NULL);
    assert(strstr(json_buf, "\"status_code\":\"error\"") != NULL);
    assert(strstr(json_buf, "\"customer.tier\":\"platinum\"") != NULL);

    /* Cleanup */
    vernier_tracer_destroy(tracer);

    printf("All C99 SDK unit tests passed successfully!\n");
    return 0;
}
