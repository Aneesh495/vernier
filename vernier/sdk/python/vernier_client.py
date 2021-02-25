"""
vernier_client.py
Official Python SDK client for Vernier distributed tracing engine.
Zero external dependencies; standard library only (Python 3.8+).

Provides thread-safe context propagation, automatic parent-child linking,
W3C TraceContext headers, and background batch exporting to Vernier server.
"""

from __future__ import annotations

import contextvars
import json
import logging
import queue
import secrets
import threading
import time
import urllib.error
import urllib.request
from contextlib import contextmanager
from dataclasses import dataclass, field
from typing import Any, Dict, Iterator, List, Optional, Tuple

logger = logging.getLogger("vernier")


def _generate_trace_id() -> str:
    """Generates a 128-bit random hex TraceId (32 hex characters)."""
    return secrets.token_hex(16)


def _generate_span_id() -> str:
    """Generates a 64-bit random hex SpanId (16 hex characters)."""
    return secrets.token_hex(8)


def _current_time_nanos() -> int:
    """Returns current system time in nanoseconds."""
    return time.time_ns()


@dataclass
class Span:
    """Represents an execution unit in a distributed trace."""
    trace_id: str
    span_id: str
    parent_id: Optional[str]
    name: str
    service_name: str
    kind: str
    start_time: int
    end_time: int = 0
    duration: int = 0
    status_code: str = "UNSET"
    status_message: str = ""
    attributes: Dict[str, Any] = field(default_factory=dict)
    events: List[Dict[str, Any]] = field(default_factory=list)

    def set_attribute(self, key: str, value: Any) -> Span:
        self.attributes[key] = value
        return self

    def add_event(self, name: str, attributes: Optional[Dict[str, Any]] = None) -> Span:
        self.events.append({
            "name": name,
            "timestamp": _current_time_nanos(),
            "attributes": attributes or {}
        })
        return self

    def set_status(self, code: str, message: str = "") -> Span:
        self.status_code = code.upper()
        self.status_message = message
        return self

    def to_dict(self) -> Dict[str, Any]:
        """Converts span to JSON-compatible dictionary for Vernier API."""
        return {
            "trace_id": self.trace_id,
            "span_id": self.span_id,
            "parent_id": self.parent_id,
            "name": self.name,
            "service_name": self.service_name,
            "kind": self.kind.lower(),
            "start_time": self.start_time,
            "end_time": self.end_time,
            "duration": self.duration,
            "status_code": self.status_code.lower(),
            "status_message": self.status_message,
            "attributes": self.attributes,
            "events": self.events,
            "links": []
        }


# Context variable for tracking active span in current thread or async task
_ACTIVE_SPAN: contextvars.ContextVar[Optional[Span]] = contextvars.ContextVar(
    "vernier_active_span", default=None
)


class VernierTracer:
    """
    Client tracer managing span lifecycles, context propagation,
    and asynchronous background telemetry transmission.
    """

    def __init__(
        self,
        service_name: str,
        endpoint: str = "http://127.0.0.1:9876",
        batch_size: int = 100,
        flush_interval_sec: float = 1.0,
        disabled: bool = False
    ) -> None:
        self.service_name = service_name
        self.endpoint = endpoint.rstrip("/")
        self.batch_size = batch_size
        self.flush_interval_sec = flush_interval_sec
        self.disabled = disabled

        self._queue: queue.Queue[Span] = queue.Queue(maxsize=10000)
        self._stop_event = threading.Event()
        self._worker_thread: Optional[threading.Thread] = None

        if not self.disabled:
            self._worker_thread = threading.Thread(
                target=self._worker_loop,
                name="vernier-batch-exporter",
                daemon=True
            )
            self._worker_thread.start()

    def get_current_span(self) -> Optional[Span]:
        """Returns the currently active span in this context."""
        return _ACTIVE_SPAN.get()

    def start_span(
        self,
        name: str,
        parent: Optional[Span] = None,
        kind: str = "INTERNAL",
        attributes: Optional[Dict[str, Any]] = None
    ) -> Span:
        """Starts a new span without activating it in context."""
        if parent is None:
            parent = self.get_current_span()

        trace_id = parent.trace_id if parent else _generate_trace_id()
        parent_id = parent.span_id if parent else None
        span_id = _generate_span_id()
        now_ns = _current_time_nanos()

        span = Span(
            trace_id=trace_id,
            span_id=span_id,
            parent_id=parent_id,
            name=name,
            service_name=self.service_name,
            kind=kind,
            start_time=now_ns,
            attributes=attributes.copy() if attributes else {}
        )
        return span

    def finish_span(
        self,
        span: Span,
        status_code: Optional[str] = None,
        status_message: str = ""
    ) -> None:
        """Completes span and enqueues for export."""
        now_ns = _current_time_nanos()
        span.end_time = now_ns
        span.duration = max(0, now_ns - span.start_time)
        if status_code:
            span.set_status(status_code, status_message)
        elif span.status_code == "UNSET":
            span.set_status("OK")

        if not self.disabled:
            try:
                self._queue.put_nowait(span)
            except queue.Full:
                logger.warning("Vernier export queue is full; dropping span.")

    @contextmanager
    def span(
        self,
        name: str,
        kind: str = "INTERNAL",
        attributes: Optional[Dict[str, Any]] = None
    ) -> Iterator[Span]:
        """
        Context manager for cleanly wrapping a code block in a span.
        Handles parent-child chaining and automatic error status capture.
        """
        s = self.start_span(name=name, kind=kind, attributes=attributes)
        token = _ACTIVE_SPAN.set(s)
        try:
            yield s
            if s.status_code == "UNSET":
                s.set_status("OK")
        except Exception as exc:
            s.set_status("ERROR", str(exc))
            s.set_attribute("error.type", exc.__class__.__name__)
            s.set_attribute("error.message", str(exc))
            raise
        finally:
            _ACTIVE_SPAN.reset(token)
            self.finish_span(s)

    def inject_w3c(self, carrier: Dict[str, str], span: Optional[Span] = None) -> None:
        """Injects W3C traceparent header into an HTTP headers dictionary."""
        target = span or self.get_current_span()
        if target:
            carrier["traceparent"] = f"00-{target.trace_id}-{target.span_id}-01"

    def extract_w3c(self, carrier: Dict[str, str]) -> Tuple[Optional[str], Optional[str]]:
        """
        Extracts trace_id and parent_span_id from W3C traceparent header.
        Returns (trace_id, parent_id) or (None, None).
        """
        val = carrier.get("traceparent") or carrier.get("Traceparent")
        if not val or not isinstance(val, str):
            return None, None
        parts = val.strip().split("-")
        if len(parts) >= 4 and len(parts[1]) == 32 and len(parts[2]) == 16:
            return parts[1], parts[2]
        return None, None

    def flush(self, timeout_sec: float = 3.0) -> None:
        """Flushes all queued spans synchronously to the collector."""
        batch: List[Span] = []
        while not self._queue.empty():
            try:
                batch.append(self._queue.get_nowait())
            except queue.Empty:
                break
        if batch:
            self._send_batch(batch)

    def close(self) -> None:
        """Shuts down background export thread."""
        self._stop_event.set()
        self.flush()
        if self._worker_thread and self._worker_thread.is_alive():
            self._worker_thread.join(timeout=2.0)

    def _worker_loop(self) -> None:
        """Background thread loop draining queue and posting batches."""
        last_flush = time.monotonic()
        batch: List[Span] = []

        while not self._stop_event.is_set():
            try:
                span = self._queue.get(timeout=0.2)
                batch.append(span)
            except queue.Empty:
                pass

            now = time.monotonic()
            if len(batch) >= self.batch_size or (batch and (now - last_flush) >= self.flush_interval_sec):
                self._send_batch(batch)
                batch = []
                last_flush = now

        if batch:
            self._send_batch(batch)

    def _send_batch(self, batch: List[Span]) -> bool:
        """Transmits JSON batch of spans to Vernier server endpoint."""
        url = f"{self.endpoint}/api/v1/spans"
        payload = json.dumps([s.to_dict() for s in batch]).encode("utf-8")
        req = urllib.request.Request(
            url,
            data=payload,
            headers={
                "Content-Type": "application/json",
                "User-Agent": f"vernier-python-sdk/{self.service_name}"
            },
            method="POST"
        )
        try:
            with urllib.request.urlopen(req, timeout=5.0) as resp:
                return resp.status in (200, 202)
        except (urllib.error.URLError, OSError) as exc:
            logger.debug("Failed to send trace batch to %s: %s", url, exc)
            return False
