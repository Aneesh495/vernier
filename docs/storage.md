# Storage Engine: Ring Buffer, WAL Framing & Columnar Indexing

Vernier's storage layer is designed for high-ingestion throughput, durable crash recovery, and rapid multi-dimensional trace lookups. It completely decouples synchronous ingestion from disk persistence through a multi-tier memory and storage hierarchy.

---

## 1. Subsystem Architecture

```
                                  Incoming Spans
                                        |
                                        v
                       +----------------------------------+
                       |      Lock-Free Ring Buffer       |
                       |    (Fixed Capacity 32k Slots)    |
                       +----------------------------------+
                                  |            |
             +--------------------+            +--------------------+
             | (Synchronous Drain)             | (Asynchronous Batch)
             v                                 v
+--------------------------+       +------------------------------------+
|  Write-Ahead Log (WAL)   |       |       In-Memory Index Engine       |
|  - Framed binary records |       |  - Inverted posting sets           |
|  - CRC-32 integrity      |       |  - Active trace map reconstruction |
|  - Auto-rolling segments |       |  - Time-sliced columnar partitions |
+--------------------------+       +------------------------------------+
```

---

## 2. In-Memory Ring Buffer

To prevent GC pauses from degrading ingress throughput under bursty traffic, Vernier employs a fixed-capacity circular ring buffer:
- **Pre-Allocated Storage**: Backed by a Racket vector of fixed length $N = 32,768$.
- **Pointer Arithmetic**: Tracks `head` (write cursor) and `tail` (read cursor) wrapped via bitwise masking:
  $$\text{next\_index} = (\text{curr} + 1) \pmod N$$
- **Batch Drain**: Workers extract slices of up to 4,096 elements in a single operation, minimizing lock acquisition overhead.
- **Overflow Policy**: When the buffer is full, a synchronous flush drains the ring buffer into the inverted index before accepting new spans.

---

## 3. Write-Ahead Log (WAL) Specification

Durable persistence is guaranteed via an append-only, binary-framed Write-Ahead Log.

### 3.1 Record Binary Layout

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|          Magic (0x5657)       |  Version (1)  | RecordType (1)|
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Payload Length                        |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         CRC-32 Checksum                       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Payload Bytes ...                     |
|                                                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

### 3.2 Field Definitions
1. **Magic Number (16 bits)**: Constant `0x56 0x57` ("VW" in ASCII) to identify frame boundaries.
2. **Version (8 bits)**: Log format version (currently `0x01`).
3. **Record Type (8 bits)**:
   - `0x01` : SPAN record (JSON-serialized canonical span).
   - `0x02` : CHECKPOINT record (snapshot marker).
   - `0x03` : METADATA record (cluster configuration).
4. **Payload Length (32 bits)**: Big-endian unsigned 32-bit integer indicating payload size in bytes.
5. **CRC-32 Checksum (32 bits)**: IEEE 802.3 32-bit cyclical redundancy check computed over length and payload bytes using polynomial `0xedb88320`.
6. **Payload (Variable)**: Canonical UTF-8 JSON bytes representing the span entity.

### 3.3 Recovery & Torn-Write Protection
During crash recovery:
1. Vernier scans the WAL directory in ascending segment index order (`wal-00000001.log`, `wal-00000002.log`, etc.).
2. For each frame, it verifies:
   - Magic bytes match `0x5657`.
   - Length does not exceed maximum allowable frame threshold (16MB).
   - Computed CRC-32 exactly matches the header checksum.
3. If an unexpected EOF or CRC-32 mismatch occurs (indicating a partial write during a power failure), Vernier stops reading at that frame boundary without crashing, preserving all previously committed records.

---

## 4. Multi-Dimensional Inverted Index

To support instantaneous VQL filtering, Vernier indexes spans into inverted posting lists:

- **Service Index**: `hash<string, set<string>>` mapping service names to set of Trace IDs.
- **Operation Index**: `hash<string, set<string>>` mapping operation names to Trace IDs.
- **Status Index**: `hash<symbol, set<string>>` mapping status codes (`'ok`, `'error`) to Trace IDs.
- **Tag Index**: `hash<string, hash<any, set<string>>>` mapping tag keys and values to Trace IDs.

When a query specifies multiple constraints:
```sql
service = "payments" and status = "error"
```
Vernier intersects the posting sets directly in memory:
$$\text{Matched} = \text{Index}[\text{"payments"}] \cap \text{Index}[\text{'error}]$$
Only the resulting Trace IDs are fetched from the active trace map, avoiding expensive full-scan sweeps over unrelated transactions.

---

## 5. Columnar Slices and Run-Length Encoding

For long-term query aggregation and percentile computation across millions of historical spans, Vernier organizes records into time-sliced partitions:
- **Time Window**: Partitions spans into discrete 1-hour temporal buckets.
- **Column-Oriented Blocks**: Spans are decoupled into parallel contiguous vectors:
  - `start_times`: Vector of 64-bit nanosecond integers.
  - `durations`: Vector of 64-bit nanosecond integers.
  - `services`: Dictionary-compressed vector of 16-bit integer IDs.
  - `statuses`: Vector of 8-bit status codes.
- **Scan Speed**: Vectorized aggregation passes directly over contiguous integer vectors, saturating CPU caches and achieving scanning speeds exceeding 10 million spans per second per core.
