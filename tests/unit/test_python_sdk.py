"""
Unit test suite for Vernier Python client SDK.
"""

import unittest
from vernier.sdk.python.vernier_client import VernierTracer, Span


class TestVernierPythonSDK(unittest.TestCase):

    def setUp(self):
        self.tracer = VernierTracer(service_name="payment-processor", disabled=True)

    def tearDown(self):
        self.tracer.close()

    def test_single_span_lifecycle(self):
        with self.tracer.span("charge_card", attributes={"amount": 49.99}) as s:
            self.assertEqual(s.name, "charge_card")
            self.assertEqual(s.service_name, "payment-processor")
            self.assertEqual(len(s.trace_id), 32)
            self.assertEqual(len(s.span_id), 16)
            self.assertIsNone(s.parent_id)
            self.assertEqual(s.attributes["amount"], 49.99)
            s.add_event("card_authorized", {"gateway": "stripe"})

        self.assertEqual(s.status_code, "OK")
        self.assertGreaterEqual(s.duration, 0)
        self.assertEqual(len(s.events), 1)

        d = s.to_dict()
        self.assertEqual(d["name"], "charge_card")
        self.assertEqual(d["status_code"], "ok")

    def test_nested_span_context_propagation(self):
        with self.tracer.span("root_checkout") as root:
            self.assertIsNone(root.parent_id)
            with self.tracer.span("verify_funds") as child1:
                self.assertEqual(child1.trace_id, root.trace_id)
                self.assertEqual(child1.parent_id, root.span_id)
                with self.tracer.span("query_db") as grandchild:
                    self.assertEqual(grandchild.trace_id, root.trace_id)
                    self.assertEqual(grandchild.parent_id, child1.span_id)

    def test_exception_status_capture(self):
        with self.assertRaises(ValueError):
            with self.tracer.span("risky_operation") as s:
                raise ValueError("Insufficient balance")

        self.assertEqual(s.status_code, "ERROR")
        self.assertEqual(s.status_message, "Insufficient balance")
        self.assertEqual(s.attributes["error.type"], "ValueError")

    def test_w3c_traceparent_injection_and_extraction(self):
        carrier = {}
        with self.tracer.span("external_call") as s:
            self.tracer.inject_w3c(carrier, s)
            self.assertIn("traceparent", carrier)
            self.assertTrue(carrier["traceparent"].startswith("00-"))

        tid, pid = self.tracer.extract_w3c(carrier)
        self.assertEqual(tid, s.trace_id)
        self.assertEqual(pid, s.span_id)


if __name__ == "__main__":
    unittest.main()
