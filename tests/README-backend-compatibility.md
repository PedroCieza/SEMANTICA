# SEMANTICA backend compatibility test policy

The compatibility framework is split deliberately into four classes:

- **A — mandatory mocked CI:** deterministic, offline tests of parsing, transport contracts, capabilities, provenance, retries, and failure handling. These must not require credentials or services.
- **B — optional live integration:** provider API tests. They run only when explicitly enabled and credentials are present; absence is **SKIP/NOT RUN**, not a analysis-code failure.
- **C — optional local service:** Ollama, llama.cpp server, or local Python-service integration. They run only when explicitly enabled and the service exists.
- **D — optional GPU:** accelerator tests run only when the optional runtime/hardware is usable and must compare low-level deterministic operations by tolerance.

`tests/testthat/fixtures/backend-compatibility.csv` is the maintained capability inventory. No fixture may contain a real API key, bearer token, password, or authentication header value.
