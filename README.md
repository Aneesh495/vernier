# Vernier

<p align="center">
  <strong>A distributed tracing and performance telemetry engine in Racket that reconstructs causal call graphs across services, calculates the critical path of distributed requests, detects latency anomalies, and serves interactive flamegraphs and dependency topologies.</strong>
</p>

<p align="center">
  <img src="https://img.shields.io/badge/language-Racket%209.3+-blue.svg" alt="Language Racket" />
  <img src="https://img.shields.io/badge/polyglot-Python%203%20%7C%20C99-brightgreen.svg" alt="Polyglot SDKs" />
  <img src="https://img.shields.io/badge/wire-W3C%20TraceContext%20%7C%20OTLP%20%7C%20Binary-orange.svg" alt="Wire Protocols" />
  <img src="https://img.shields.io/badge/storage-WAL%20%2B%20Inverted%20Index-purple.svg" alt="Storage Engine" />
  <img src="https://img.shields.io/badge/license-MIT-green.svg" alt="License" />
</p>

---

## Overview

In microservice architectures, end-to-end request latency is governed by intricate causal relationships. Parallel sub-operations, asynchronous job dispatch, cascading fanouts, and clock skew obscure the true performance bottlenecks. Standard APM systems treat spans as flat, isolated intervals and rely on simple duration sums, failing to pinpoint which specific operations actually delayed user requests.

**Vernier** is an observability and distributed tracing engine built from first principles in Racket. It reconstructs multi-tier transactions into rigorous Directed Acyclic Graphs (DAGs), compensates for physical clock skew using causal convex hulls, computes the topological critical path using dynamic programming in linear time, detects structural latency anomalies (such as N+1 query loops and retry storms), and serves real-time flamegraphs and service topologies through an embedded zero-dependency web console.

```
[api-gateway]      [========================================] 185ms (Total Duration)
[auth-service]        [====] 22ms
[order-service]             [==============================] 148ms
[catalog-service]             [=======] 35ms                 (Concurrent Branch A)
[payment-service]                     [================] 82ms (Concurrent Branch B - CRITICAL)
[stripe-api]                             [==========] 52ms   (CRITICAL BOTTLENECK)
[order-db]                                              [==] 15ms

Critical Path: api-gateway -> order-service -> payment-service -> stripe-api (154ms, 83.2% of latency)
Optimization Insight: Accelerating catalog-service yields 0ms gain; optimizing stripe-api reduces total latency.
```

---

## Key Highlights

- **Causal Graph Reconstruction & Cycle Breaking**: Parses parent-child relationships into a directed execution graph $G = (V, E)$, verifies topological order via Kahn's algorithm, isolates distributed cycles with Tarjan's Strongly Connected Components algorithm, and compensates for clock drift using affine convex hull transformations.
- **Topological Critical Path Decomposition**: Solves the longest-path dynamic program over the causal DAG in $O(|V| + |E|)$ time, decomposing end-to-end latency into critical vs. concurrent slack and computing exact percentage attributions per service.
- **Bounded-Memory Streaming Sketches**:
  - **DDSketch**: Quantile estimator guaranteeing bounded relative error ($\alpha = 0.01$) over arbitrary latency distributions without storing raw samples.
  - **HyperLogLog**: Cardinality estimator with 64-bit MurmurHash3 avalanche mixing for tracking unique trace IDs and client IPs (< 2% error).
  - **Kirsch-Mitzenmacher Bloom Filter**: Double-hashing probabilistic filter with zero false negatives for rapid tag membership testing.
  - **Exponentially Decaying Reservoir (EDR)**: Forward-decaying time-biased sampling prioritizing recent latency anomalies.
- **Durable Storage & Crash Recovery**:
  - **Lock-Free Ring Buffer**: 32,768-slot pre-allocated buffer for absorbing bursty traffic spikes.
  - **Segmented Write-Ahead Log (WAL)**: Binary framed log with IEEE 802.3 CRC-32 checksums, automatic segment rotation, and torn-write recovery.
  - **Multi-Dimensional Inverted Index**: Instantaneous multi-attribute queries over service, operation, status, duration, and custom span tags.
- **VQL (Vernier Query Language)**: Full lexer and recursive descent parser supporting boolean logic (`and`, `or`, `not`), duration thresholds (`duration > 150ms`), string search (`contains`), set membership (`in`), and automatic percentile calculations.
- **Real-Time Embedded Web Console**: Zero-dependency Single Page Application served directly by Racket, featuring live Server-Sent Events (SSE) telemetry, ASCII terminal waterfall layouts, and interactive SVG flamegraphs.
- **Polyglot Client SDKs**:
  - **Racket**: Idiomatic dynamic extent tracking via thread-local parameters and `with-span` syntax macro.
  - **Python**: Zero-dependency standard library client (`vernier_client.py`) with thread-safe context managers and asynchronous background batching.
  - **C99**: Portable zero-dependency C client (`vernier.h` / `vernier.c`) with nanosecond timers and W3C header injection.

---

## System Architecture

```
                          +-------------------------------------------------+
                          |             Vernier Ingestion Layer             |
                          | (W3C TraceContext / OTLP JSON / Compact Binary) |
                          +-------------------------------------------------+
                                                   |
                                                   v
                          +-------------------------------------------------+
                          |              High-Throughput Ring Buffer        |
                          |            (Lock-Free Fixed-Capacity FIFO)      |
                          +-------------------------------------------------+
                                    |                              |
                                    v                              v
                    +------------------------------+   +------------------------------+
                    |    Write-Ahead Log (WAL)     |   |    Multi-Dimensional Index   |
                    | (CRC32 Segmented Persistence)|   | (Posting Lists & Time Slices)|
                    +------------------------------+   +------------------------------+
                                                                   |
                                                                   v
                                                   +----------------------------------+
                                                   |     Causal DAG Engine & Graph    |
                                                   |  (Tarjan SCC / Clock Skew Hull)  |
                                                   +----------------------------------+
                                                                   |
                                                   +----------------------------------+
                                                   |   Critical Path Analysis (DP)    |
                                                   |   (Longest Path over Span Nodes) |
                                                   +----------------------------------+
                                                                   |
                                    +------------------------------+------------------------------+
                                    |                              |                              |
                                    v                              v                              v
                    +------------------------------+   +------------------------------+   +------------------------------+
                    |   Anomaly Detection Engine   |   |    VQL Query Processing      |   |   Real-Time Telemetry Hub    |
                    | (Welford Z-Score / Patterns) |   | (Lexer / Recursive Parser)   |   | (Embedded Console & SSE)     |
                    +------------------------------+   +------------------------------+   +------------------------------+
```

---

## Quickstart

### 1. Requirements
- Racket 8.0+ or 9.0+ (`racket`, `raco`)
- Python 3.8+ (for Python SDK)
- Standard C99 compiler (GCC or Clang, for C SDK)
- `make`

### 2. Run the Full Test Suite
Execute the comprehensive test harness spanning unit, integration, and property invariant suites:

```bash
make test
```

### 3. Start the Vernier Telemetry Daemon
Launch the engine, REST API, and web console on port 9876:

```bash
racket vernier.rkt server --port 9876 --wal-dir ./data/wal
```

Open your browser to `http://127.0.0.1:9876/` to access the embedded real-time console.

### 4. Run the Synthetic Microservices Cluster Simulator
In a separate terminal, stream realistic synthetic transactions (including normal multi-tier checkouts, N+1 query regressions, and payment retry storms):

```bash
racket vernier.rkt demo --count 30 --delay 150
```

Watch traces stream in real-time on the web console via Server-Sent Events (SSE).

### 5. Run the In-Memory Throughput Benchmark
```bash
racket vernier.rkt benchmark --count 10000
```

Typical benchmark performance:
```
Benchmarking Vernier engine with 10000 spans...
Ingested 10000 spans in 0.385 s (25974 spans/sec)
Queried 1000 matching traces in 0.812 ms
Calculated critical path for 1000 traces in 18.241 ms (18.2 us/trace)
Benchmark completed successfully.
```

---

## Terminal Waterfall & Critical Path Inspection

Inspect any trace directly from the command line:

```bash
racket vernier.rkt trace 4f29a0b1c3d4e5f67890abcdef123456
```

Example terminal output:

```
Trace: 4f29a0b1c3d4e5f67890abcdef123456
Duration: 185.000 ms
Spans: 9 | Services: api-gateway, auth-service, order-service, catalog-service, payment-service, notification-service

Timeline:
[api-gateway]       POST /v2/checkout       |==================================================| (185.0 ms) [CRIT]
  [auth-service]    validate_jwt            |==                                                | (22.0 ms)
  [order-service]   create_order_saga       |   ============================================== | (148.0 ms) [CRIT]
    [catalog-svc]   reserve_stock           |    ==========                                    | (35.0 ms)
    [payment-svc]   process_payment         |                =========================         | (82.0 ms) [CRIT]
      [payment-svc] eval_fraud_risk         |                ======                            | (18.0 ms)
      [payment-svc] POST stripe/charges     |                       ================           | (52.0 ms) [CRIT]
    [order-service] UPDATE orders           |                                         ====     | (15.0 ms)
    [notify-svc]    dispatch_email          |                                           ==     | (8.0 ms)

Critical Path Duration: 154.000 ms (83.2% of total)
Service Critical Attributions:
  - api-gateway: 15.000 ms (9.7%)
  - order-service: 14.000 ms (9.1%)
  - payment-service: 73.000 ms (47.4%)
  - stripe-api: 52.000 ms (33.8%)
```

---

## Vernier Query Language (VQL)

Vernier includes an expressive query language designed specifically for distributed trace analysis:

```sql
-- Find slow checkout transactions originating at the API gateway
service = "api-gateway" and operation = "POST /v2/checkout" and duration > 200ms

-- Isolate database query regressions across any service
tag("db.system") = "postgresql" and duration >= 50ms

-- Filter traces with payment errors or retry cascades
(service = "payment-service" or service = "billing") and errors > 0
```

Execute queries via CLI:
```bash
racket vernier.rkt query 'service = "api-gateway" and duration > 100ms'
```

Or via REST API:
```bash
curl "http://127.0.0.1:9876/api/v1/traces?q=service+%3D+%22api-gateway%22"
```

---

## Polyglot Client SDKs

### Racket SDK

```racket
#lang racket/base
(require "vernier/sdk/racket/tracer.rkt")

(define t (make-tracer "billing-engine" #:host "127.0.0.1" #:port 9876))

(parameterize ([current-tracer t])
  (with-span ("process-invoice" #:attributes (hash 'invoice.id "inv_8812"))
    ;; Child spans automatically inherit parent trace context
    (with-span ("authorize-funds" #:attributes (hash 'amount 99.50))
      (printf "Executing payment...\n"))))

(tracer-flush! t)
```

### Python SDK

```python
from vernier.sdk.python.vernier_client import VernierTracer

tracer = VernierTracer("order-gateway", endpoint="http://127.0.0.1:9876")

with tracer.span("handle_order", attributes={"order.id": "ord_104"}) as span:
    # Context automatically injected into downstream HTTP headers
    headers = {}
    tracer.inject_w3c(headers, span)
    
    with tracer.span("reserve_inventory", kind="CLIENT"):
        # Executes within the scope of handle_order
        pass

tracer.close()
```

### C99 SDK

```c
#include "vernier/sdk/c/vernier.h"

int main(void) {
    vernier_tracer_t* tracer = vernier_tracer_create("cache-proxy", "127.0.0.1", 9876);
    
    vernier_span_t root_span;
    vernier_span_start(tracer, &root_span, "memcached_get", VERNIER_SPAN_SERVER, NULL);
    vernier_span_set_attribute_str(&root_span, "cache.key", "user:session:495");
    
    /* Simulate operation */
    vernier_span_end(&root_span);
    
    vernier_tracer_destroy(tracer);
    return 0;
}
```

---

## Repository Structure

```
vernier/
├── info.rkt                      # Racket package manifest
├── vernier.rkt                   # Executable CLI entrypoint
├── Makefile                      # Build, test, benchmark, and demo automation
├── docs/                         # Technical documentation & algorithm specifications
│   ├── architecture.md           # System architecture & dataflow diagrams
│   ├── critical-path.md          # Mathematical specification of critical path DP
│   ├── vql.md                    # Vernier Query Language reference manual
│   └── storage.md                # WAL framing, ring buffer & inverted index
├── vernier/
│   ├── core/                     # Core primitive data models
│   │   ├── time.rkt              # Nanosecond timestamps & monotonic intervals
│   │   ├── id.rkt                # 128-bit TraceId, 64-bit SpanId & W3C flags
│   │   ├── span.rkt              # Span record, events, links & JSON mappings
│   │   ├── trace.rkt             # In-memory trace collection & parent indexing
│   │   └── contract.rkt          # Formal system boundary contracts
│   ├── dag/                      # Graph reconstruction & critical path
│   │   ├── graph.rkt             # Causal DAG construction & Kahn topological sort
│   │   ├── cycle.rkt             # Tarjan SCC & cycle breaking
│   │   ├── clock_skew.rkt        # Convex hull clock skew compensation
│   │   ├── critical_path.rkt     # Dynamic programming longest path engine
│   │   └── diff.rkt              # Structural diffing between trace graphs
│   ├── sketch/                   # Bounded-memory streaming probabilistic sketches
│   │   ├── quantile.rkt          # DDSketch bounded relative error quantile estimator
│   │   ├── histogram.rkt         # Log-linear streaming histogram
│   │   ├── reservoir.rkt         # Exponentially decaying reservoir (EDR)
│   │   ├── hll.rkt               # HyperLogLog with Murmur64 avalanche mixer
│   │   └── bloom.rkt             # Kirsch-Mitzenmacher double-hashing Bloom filter
│   ├── anomaly/                  # Anomaly detection & statistical scoring
│   │   ├── baseline.rkt          # Welford online mean and variance tracking
│   │   ├── pattern.rkt           # Structural patterns (N+1 query, retry storms, fanout)
│   │   ├── detector.rkt          # Anomaly detection engine & report builder
│   │   └── scorer.rkt            # Health scoring & bottleneck attribution
│   ├── sampler/                  # Dynamic tail-based sampling
│   │   ├── rules.rkt             # Sampling rule definitions & criteria
│   │   ├── tail_buffer.rkt       # Circular retention buffer for trace completion
│   │   ├── evaluator.rkt         # Multi-predicate sampling decision engine
│   │   └── adaptive.rkt          # Token bucket rate-limiting adaptive sampler
│   ├── storage/                  # Ingestion buffer, WAL & columnar indexing
│   │   ├── buffer.rkt            # Fixed-capacity lock-free circular ring buffer
│   │   ├── wal.rkt               # Write-Ahead Log with IEEE 802.3 CRC-32 framing
│   │   ├── index.rkt             # Multi-attribute inverted index
│   │   ├── column.rkt            # Run-length encoded columnar span blocks
│   │   ├── partition.rkt         # Time-sliced partition manager
│   │   └── engine.rkt            # Unified storage orchestrator & auto-replay
│   ├── query/                    # Vernier Query Language (VQL)
│   │   ├── ast.rkt               # Abstract Syntax Tree nodes
│   │   ├── lexer.rkt             # VQL lexical analyzer & token stream
│   │   ├── parser.rkt            # Recursive descent parser
│   │   └── executor.rkt          # Index-accelerated query evaluation
│   ├── profiler/                 # Visualizers & profiling renderers
│   │   ├── waterfall.rkt         # Chronological waterfall layout & ASCII visualizer
│   │   ├── flamegraph.rkt        # Collapsed stack aggregator & SVG flamegraph
│   │   └── topology.rkt          # Multi-service dependency topology generator
│   ├── protocol/                 # Wire protocols & serialization
│   │   ├── w3c.rkt               # W3C TraceContext header parser & injector
│   │   ├── otlp.rkt              # OpenTelemetry JSON parser
│   │   └── binary.rkt            # Fixed-header compact binary serializer
│   ├── server/                   # HTTP daemon & telemetry hub
│   │   ├── router.rkt            # HTTP request/response parser & query matcher
│   │   ├── handler.rkt           # REST API routes & error handlers
│   │   ├── sse.rkt               # Server-Sent Events real-time broadcast hub
│   │   └── server.rkt            # Multi-threaded TCP server with custodian isolation
│   ├── ui/                       # Real-time Web Console
│   │   └── console.rkt           # Embedded zero-dependency Single Page Application
│   ├── cli/                      # Command line interface
│   │   └── main.rkt              # Subcommand dispatcher (server, trace, query, bench, demo)
│   ├── simulator/                # Microservice cluster simulator
│   │   └── mesh.rkt              # Multi-tier synthetic traffic & anomaly injector
│   └── sdk/                      # Polyglot Client SDKs
│       ├── racket/               # Racket SDK (tracer.rkt)
│       ├── python/               # Python SDK (vernier_client.py)
│       └── c/                    # C99 SDK (vernier.h, vernier.c)
└── tests/                        # Comprehensive Test Suites
    ├── unit/                     # Granular module unit tests
    ├── integration/              # End-to-end pipeline & WAL crash recovery tests
    └── property/                 # Algorithmic invariant verification tests
```

---

## Verification & Invariant Testing

Vernier includes rigorous property-based invariant tests verifying:
- **DAG Acyclicity**: Kahn's topological sort preserves causality ($u$ appears before $v$ for every directed edge $u \to v$).
- **Critical Path Bounds**: $0 < T_{\text{crit}} \le T_{\text{total}}$ and exact conservation of service attributions ($\sum A(s) \le T_{\text{crit}}$).
- **Quantile Monotonicity**: DDSketch percentiles strictly satisfy $q_{0.1} \le q_{0.25} \le q_{0.5} \le q_{0.75} \le q_{0.9} \le q_{0.99}$.
- **HyperLogLog Error Bounds**: Distinct cardinality estimation error $< 5\%$ at $N = 5,000$.
- **Bloom Filter Zero False Negatives**: $100\%$ recall for all inserted items.
- **Torn Write & Checksum Rejection**: CRC-32 bit-flip detection ensures corrupted WAL frames are rejected without crashing.

Run all test suites:
```bash
make test
```

---

## License

Vernier is open-source software licensed under the [MIT License](LICENSE).
