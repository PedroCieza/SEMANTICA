# Contributing to SEMANTICA

Thank you for helping improve SEMANTICA. The project combines user-facing R
interfaces with a heavily tested semantic/psychometric search engine, so changes
should be scoped and reviewed according to the layer they affect.

## Before opening a pull request

1. Start from an up-to-date branch and keep the change focused.
2. Never commit API keys, tokens, credentials, private model paths, participant
   data, or other sensitive material.
3. Add or update tests for behavioral changes.
4. Update roxygen source comments rather than hand-editing generated `man/*.Rd`
   files or `NAMESPACE`.
5. Run the release checks below on a clean R installation.

## Change classes

### Surface and documentation changes

Examples include README/pkgdown material, result presentation, diagnostics
wording, setup guidance, and GitHub metadata. These changes should preserve the
canonical result and existing analytical calculations unless the pull request
explicitly proposes a methodological change.

### Backend and dependency changes

Keep capability detection explicit and degrade gracefully where SEMANTICA
already has a documented fallback. Provider-specific behavior must not silently
change the semantics of another backend. Tests should not require live paid API
credentials unless they are clearly optional/local-only.

### Methodological or analytical changes

Changes to ACO scoring/search, semantic objectives, PFA/ESEM/DFI calculations,
selection rules, participant-validation evidence, RNG behavior, or robustness
logic require:

- a precise statement of the problem being solved;
- scientific or statistical justification where applicable;
- tests for the intended behavior and relevant negative/adversarial cases;
- documentation distinguishing published methodology from SEMANTICA-specific
  policy or adaptation; and
- explicit confirmation that unrelated evidence families remain unchanged.

Do not refactor the analytical engine as collateral work for an unrelated user-
experience or documentation change.

## Local checks

From the package root, run:

```r
devtools::document()
devtools::test()
rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"))
pkgdown::build_site()
```

Run `devtools::document()` a second time and confirm that it produces no further
changes. Review `git diff` before committing generated documentation.

## Reporting reproducibility issues

Include the SEMANTICA version, `sessionInfo()`, operating system, backend/model
identifiers, relevant configuration, and a minimal reproducible example. Remove
credentials and private/local paths first. For security-sensitive reports, use
the process in `SECURITY.md` instead of a public issue.
