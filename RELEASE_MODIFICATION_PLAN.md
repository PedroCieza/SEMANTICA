# SEMANTICA 0.2 GitHub release modification plan

This plan is intentionally constrained to release engineering and user-facing
presentation. The analytical engine is treated as frozen.

## Invariants

The release-preparation work must not change ACO scoring, pheromone updates,
elite/archive ranking, semantic objectives, item generation, embedding
calculations, PFA/ESEM estimation or scoring, DFI mathematics, evidence rules,
participant-validation calculations, RNG policy, resource-allocation algorithms,
or serialization semantics.

The sole permitted analytical-source compatibility change is removal of hard
namespace imports for optional `dynamic::catHB`/`catOne` helpers. When those
helpers exist, SEMANTICA calls them with the same arguments as before. When they
do not exist, SEMANTICA enters the simulation fallback that was already present
in `safe_compute_dfi()`.

## Ordered gates

1. Repair the optional `dynamic` dependency contract and add a regression test.
2. Synchronize software citation, version presentation, GitHub installation, and
   full repository-license text.
3. Add a methodological-foundations article that maps calculations and
   interpretation rules to peer-reviewed sources, while explicitly identifying
   SEMANTICA-specific policies and semantic-proxy adaptations.
4. Add GitHub community/security/contribution files, issue/PR templates, and
   repository ignore/line-ending hygiene.
5. Harden CI with least-privilege permissions, documentation-drift checks, a
   release `--as-cran` gate, and dependency-contract checks without weakening
   existing cross-platform or secret-scan coverage.
6. Remove stale public version labels and separate historical/maintainer material
   from current user guidance.
7. Run final static integrity checks, compare all analytical R files with the
   baseline, inventory the exact changed files, and package the modified source.

## Release-machine validation still required

Because the current execution environment does not contain R, the final tagged
commit must additionally pass, on a machine/CI runner with R installed:

```r
devtools::document()
devtools::test()
rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"))
pkgdown::build_site()
```

`devtools::document()` should be run twice; the second run must leave a clean Git
working tree. The release tag should be created only after those checks pass.

## Execution status

Release-preparation gates 1-6 were completed in order, with an integrity check
between each gate. Static gate 7 passed with the following source invariant:

- the original archive contained 22 `R/*.R` files;
- 20 remain byte-for-byte identical to the uploaded 0.2 archive;
- `R/SEMANTICA-package.R` changes only the optional `dynamic` roxygen import;
- `R/pipeline_core.R` changes only `safe_compute_dfi()` so `catHB`/`catOne` are
  feature-detected before the already-existing fallback is used;
- no ACO, ESEM, PFA, semantic-objective, item-generation, embedding, resource,
  RNG, result-assembly, participant-validation, or serialization calculation was
  modified;
- `tests/testthat/fixtures/backend-compatibility.csv` is byte-identical to the
  uploaded archive and is now trackable because the broad `*.csv` ignore rule
  was removed.

The repository is prepared for the R-based release-validation workflow. The
runtime gate remains intentionally blocking: do not create the release tag until
that workflow confirms roxygen stability, tests, `R CMD check --as-cran`, and
pkgdown construction on the exact commit intended for release.
