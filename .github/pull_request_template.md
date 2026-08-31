## Purpose

Describe the user/research problem this change addresses.

## Change class

- [ ] Documentation/repository surface only
- [ ] User-facing R interface with unchanged calculations
- [ ] Backend/dependency compatibility
- [ ] Methodological/analytical change (requires scientific/statistical rationale)

## Integrity checklist

- [ ] I kept the change scoped and did not perform unrelated refactors.
- [ ] I added/updated tests where behavior changed.
- [ ] I updated roxygen source rather than hand-editing generated help/NAMESPACE.
- [ ] I documented literature sources where a calculation is literature-derived.
- [ ] I distinguish SEMANTICA-specific policy/adaptation from published constants.
- [ ] I verified that no credentials, participant data, or private paths are committed.
- [ ] `devtools::document()` is stable on a second run.
- [ ] `devtools::test()` passes.
- [ ] `R CMD check --as-cran`/`rcmdcheck` passes.
- [ ] pkgdown builds successfully when documentation changed.
