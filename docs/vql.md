# Vernier Query Language (VQL) Reference Manual

The Vernier Query Language (VQL) is a declarative domain-specific query language designed for filtering, slicing, and aggregating distributed traces across high-cardinality multi-service topologies.

---

## 1. Formal EBNF Grammar

```ebnf
Query          ::= OrExpression EOF ;

OrExpression   ::= AndExpression ( "or" AndExpression )* ;

AndExpression  ::= NotExpression ( "and" NotExpression )* ;

NotExpression  ::= "not" NotExpression
                 | PrimaryExpression ;

PrimaryExpression
               ::= "(" OrExpression ")"
                 | Comparison ;

Comparison     ::= Field Operator Value ;

Field          ::= Identifier
                 | "tag" "(" StringLiteral ")" ;

Operator       ::= "=" | "!=" | "<" | "<=" | ">" | ">=" | "contains" | "in" ;

Value          ::= StringLiteral
                 | NumberLiteral
                 | DurationLiteral
                 | BooleanLiteral
                 | ListLiteral ;

DurationLiteral
               ::= Integer ( "ns" | "us" | "ms" | "s" | "m" ) ;

ListLiteral    ::= "[" ( Value ( "," Value )* )? "]" ;

Identifier     ::= [a-zA-Z_][a-zA-Z0-9_.-]* ;
StringLiteral  ::= '"' [^"]* '"' ;
NumberLiteral  ::= [0-9]+ ( "." [0-9]+ )? ;
BooleanLiteral ::= "true" | "false" ;
```

---

## 2. Core Fields

| Field Name | Type | Description |
|---|---|---|
| `service` | String | Matches the root or any constituent service name in the trace |
| `operation` / `name` | String | Matches the root span operation name |
| `duration` | Duration | Evaluates the end-to-end wall-clock latency of the trace |
| `status` | Symbol | Evaluates root or trace health: `'ok`, `'error`, or `'unset` |
| `errors` | Integer | Total count of spans in the trace that reported an error |
| `spans` | Integer | Total number of spans in the causal execution graph |
| `tag("key")` | Primitive | Evaluates attributes attached to any span within the trace |

---

## 3. Supported Operators

### Equality and Inequality
- `=` : Strict equality for strings, numbers, booleans, and symbols.
- `!=` : Non-equality test.

### Ordered Comparisons
- `<` , `<=` , `>` , `>=` : Numerical and duration threshold filtering.
- Automatically handles unit conversion (e.g. `duration > 250ms` converts `250ms` to $250,000,000$ nanoseconds).

### String Substring Matching
- `contains` : Substring search on service names, operation names, and string attributes.

### Set Membership
- `in` : Checks whether the field value belongs to a specified list literal.
  Example: `service in ["payment-service", "billing-engine"]`

---

## 4. Query Examples

### Slow API Gateway Transactions
Find all checkout operations originating at `api-gateway` that took longer than 200 milliseconds:
```sql
service = "api-gateway" and operation = "POST /v2/checkout" and duration > 200ms
```

### High-Severity Errors
Find traces with at least one failed span:
```sql
status = "error" or errors > 0
```

### Investigating Payment Provider Latency
Find all transactions involving Stripe where payment processing took over 1 second:
```sql
service = "payment-service" and tag("payment.provider") = "stripe" and duration > 1s
```

### Database Query Outliers
Locate slow database queries across any service:
```sql
tag("db.system") = "postgresql" and duration >= 50ms
```

### Complex Multi-Condition Filter
```sql
(service = "catalog-service" or service = "inventory") and duration > 100ms and not (status = "ok")
```

---

## 5. Execution Engine and Latency Aggregation

When a VQL query executes against Vernier:
1. **Lexical Analysis (`lexer.rkt`)**: Tokenizes the raw input string into typed tokens.
2. **Recursive Descent Parsing (`parser.rkt`)**: Produces an Abstract Syntax Tree (AST) rooted in boolean expression nodes (`ast-or`, `ast-and`, `ast-not`, `ast-comparison`).
3. **Optimized Set-Intersection Filtering (`executor.rkt`)**: Inspects AST equality predicates against inverted index postings to narrow candidate traces prior to full evaluation.
4. **Percentile Computation**: All matching traces are passed through a streaming DDSketch to compute exact metric percentiles:
   - `total_matches`: Total count of matched traces.
   - `error_count`: Number of matched traces containing failures.
   - `p50_latency_nanos`: 50th percentile (median) duration.
   - `p90_latency_nanos`: 90th percentile duration.
   - `p99_latency_nanos`: 99th percentile tail latency.
