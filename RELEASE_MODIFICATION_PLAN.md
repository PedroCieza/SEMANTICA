# SEMANTICA 0.2 working-tree validation and release plan

This document supersedes the earlier release-preparation plan that treated the
analytical engine as frozen. The current working tree contains intentional
analytical changes, including the factor-pool/top-up feasibility patch and its
related ACO/ESEM hardening. It must therefore be validated as a new behavioral
baseline rather than represented as byte-identical to the earlier 0.2 source.

## Current invariants

The current code is intended to preserve these scientific and engineering
boundaries:

1. Sample-free semantic PFA/ESEM/DFI outputs remain representation-derived
   proxy diagnostics, not participant-response evidence of construct validity.
2. ACO must not start from a factor pool that violates the configured eligible
   slack, independent-redundancy-unit, or factor-alignment-calibration
   feasibility invariants unless an explicit compatibility/warning mode is
   requested.
3. Factor-specific top-up generation may use one explicitly logged
   wording/forbidden-conflict relaxation after strict recovery fails; only the
   factors that remain infeasible advance to that rung. Robust construct
   mismatch, unresolved factor alignment, and polarity remain hard, and the
   named effective policy must be propagated unchanged into ACO.
4. Expensive structural repair is bounded and surrogate-first, and is skipped
   when the candidate pool has no meaningful replacement slack.
5. Explicit provider/model requests are never silently replaced by a different
   model. Provider-key forwarding to a changed endpoint origin requires an
   explicit opt-in.
6. External item/factor labels are preserved as SEMANTICA identities. Any
   lavaan-safe transport renaming is private to the modeling boundary and must
   be mapped back before SEMANTICA scoring/diagnostics.
7. Bundle checksums detect accidental corruption; they are not signatures and
   do not make an untrusted RDS object safe to deserialize.
8. Search-time PSOCK dispatch recovery is condition-message independent: one
   bounded cluster rebuild is attempted for a dispatch-level failure before the
   deterministic serial fallback reuses the same task seeds.

## Static validation completed in this working tree

The current review has performed the following non-R checks on the exact source
that is to be packaged:

- balanced string/delimiter scanning across all `R/*.R` and
  `tests/testthat/*.R` files;
- public function/Rd usage-signature parity checks for the APIs changed by the
  security/provider hardening;
- call-path review of ESEM transport-name mapping, including DFI and residual
  diagnostics that consume model-implied covariance matrices;
- regression-test additions for factor-specific recovery policy, unresolved
  alignment recovery, calibrated forbidden-conflict severity, locale-independent
  PSOCK recovery, pre-ACO/top-up completion, lavaan name transport, early factor
  validation, credential routing, and provider-default behavior;
- packaging review to ensure the distributable ZIP excludes `.git`, `.RData`,
  `.Rhistory`, and R-check output directories.

These checks are useful but are **not** substitutes for parsing/executing the
package in R.

## Blocking runtime validation still required

The current analysis environment does not contain `R`/`Rscript`, and package
installation could not be provisioned because the environment has no working
package-network resolution. Before tagging or publishing this source, run the
following on the exact extracted ZIP contents in a supported R environment:

```r
# Regenerate documentation from source comments first.
devtools::document()

# Then exercise the full regression suite.
devtools::test()

# Build/check the package artifact, not only the working directory.
rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"))

# Verify the documentation site if it is part of the release surface.
pkgdown::build_site()
```

Run `devtools::document()` a second time and require no unexpected source or
Rd drift. Any runtime failure must be resolved before creating a release tag.

## Deliberately deferred changes

The following work is intentionally **not** mixed into this analytical patch:

- large-scale decomposition of `R/pipeline_core.R` or removal of all
  `parent.frame()` coupling; that refactor should follow a passing
  characterization baseline so behavior changes can be attributed cleanly;
- renumbering the package solely to reconcile historical higher version-like
  NEWS headings. `DESCRIPTION` remains at 0.2 until the maintainer makes an
  explicit release-version decision;
- arbitrary pinning of remote/model dependencies without an approved release
  dependency policy and a tested lock/snapshot strategy.

## Distribution rule

Create distributable source from a clean export/build of the validated tree.
Do not distribute the development workspace wholesale. In particular, exclude
Git object history, saved R workspaces/history, and `*.Rcheck` output trees.
