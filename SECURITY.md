# Security Policy

## Supported versions

Security fixes are targeted to the latest tagged SEMANTICA release. If a report
also affects an older release, the maintainer will decide whether a backport is
practical.

## Reporting a vulnerability

Please do **not** open a public issue for a vulnerability, exposed credential, or
report that contains sensitive local/provider information.

If GitHub private vulnerability reporting is enabled for this repository, use
that channel. Otherwise, contact the package maintainer privately using the
maintainer email listed in `DESCRIPTION` and include only the information needed
to reproduce the problem.

Useful reports include the affected SEMANTICA version, operating system, R
version, backend/protocol involved, impact, and a minimal reproduction. Redact
API keys, tokens, authorization headers, participant data, private URLs, and
local filesystem details that are not necessary to reproduce the issue.

## Security-relevant scope

Relevant reports include, among other things, credential sanitization,
serialization/bundle disclosure, provider authentication handling, local HTTP
backend exposure, Python/Conda environment handling, unsafe file/path handling,
and accidental leakage of private model or result metadata.

SEMANTICA's checksum-based bundle integrity checks are corruption-detection
mechanisms, not cryptographic signatures or authenticity guarantees.
