# SEMANTICA technical audit

Audit date: 2026-09-24  
Scope: the supplied working tree, read-only code and package review. No package
code or user data was changed.

## Executive summary

SEMANTICA is a substantial, thoughtfully bounded research-software package for
semantic-assisted item development. Its strongest feature is intellectual
honesty: representation-derived PFA, ESEM, DFI, HTMT-like, and optimisation
outputs are repeatedly labelled as pre-data proxy diagnostics, while
participant-response validation is kept separate. The package also has unusually
good safeguards around provider credentials, result serialization, resource
telemetry, reproducibility metadata, ESEM admissibility, and factor alignment.

It is not release-ready in the audited state. One public cache-cleaning function
can delete arbitrary RDS files in a user-selected directory; generated analysis
artifacts are being included in the source tarball; the installed-package test
suite has one cross-locale failure; and R CMD check identifies a missing public
argument description. These are remediable engineering defects, not evidence
that the analytical engine is generally incorrect. For a scientific publication,
the central remaining gap is empirical method validation: the package correctly
does not claim it has established ACO superiority or respondent-level construct
validity, and the repository does not ship the independent held-out benchmark
results needed to make such claims.

## Scope, evidence, and limits

The audit examined 23 R files (38,664 lines; 576 function definitions), 114
exports, 129 Rd files, 8 vignettes, 97 test files, examples, package metadata,
build files, and GitHub workflows. All R and Rd files parsed successfully.

There is no `.git` repository in this supplied directory, so commit history,
tracked-versus-untracked status, and CI run history could not be audited. Live
provider, local-service, GPU, and slow real ACO/ESEM tests were intentionally
not run because they require credentials, hardware, or opt-in infrastructure.
Pandoc is unavailable locally, so full vignette rendering could not be
independently completed; the CI configuration does provision it.

## Architecture map

```text
semantica_run() / semantica_run_custom()
  -> configuration resolution and run plan
  -> semantica_pipeline()
       -> provider session -> item generation -> embedding/cache
       -> embedding validation -> cosine matrix -> content diagnostics/top-up
  -> ACO_with_ESEM()
       -> semantic/PFA proposal scoring
       -> evaluation broker -> proxy ESEM/DFI -> archive/final diagnostics
  -> canonical full result + provenance/evidence records
  -> compact presentation facade, summaries, plots, exports, bundle
  -> optional later participant-response validation
```

### Source modules

| File | Responsibility |
| --- | --- |
| `SEMANTICA-package.R` | Package-level help and namespace imports. |
| `integrity_helpers.R` | Classed validation errors, configuration canonicalization, provenance sanitation. |
| `decision_policy.R` | Central decision-policy metadata. |
| `session_security.R` | Credential and URL sanitization for returned/serialized objects. |
| `backend_health.R` | Backend HTTP health and model preflight. |
| `compute_backend.R` | CPU/GPU capability detection and dense cosine computation. |
| `resource_management.R` | Worker planning, PSOCK-pool lifecycle, memory telemetry. |
| `item_generation.R` | Conda setup, backend registry/session construction, requests, parsing, item pools, embedding, wrapping, import/export. |
| `provider_reasoning.R` | Provider request formats, reasoning-token and rate-limit accounting. |
| `representation_quality.R` | Embedding contracts, task instructions, representation/content alignment. |
| `analysis_extensions.R` | Cache, thresholds, coverage/polarity, calibration, robustness, semantic diagnostics. |
| `research_extensions.R` | Experimental comparison, translation, review-queue, and preregistration helpers. |
| `validation_benchmarks.R` | Ensemble matrices, empirical calibration, and equal-budget comparison helpers. |
| `esem_quality.R` | ESEM axis alignment, technical admissibility, and ESEM state. |
| `evaluation_broker.R` | Search-time ESEM deduplication, budgets, caches, and telemetry. |
| `utils_metrics.R` | Small metric and policy helpers. |
| `pipeline_core.R` | DFI/PFA/ESEM mechanics and the ACO optimisation engine. |
| `full_pipeline.R` | Public configuration objects and full workflow orchestration. |
| `minimal_pipeline.R` | Simpler `semantica_run()` facade and ACO presets. |
| `result_summary.R` | Summary and print methods for canonical results. |
| `user_experience.R` | Views, plans, setup checks, review tables, cache utilities, and later validation. |
| `visualizations.R` | Static/interactive diagnostic visualisations and plot orchestration. |
| `bundle_io.R` | Sanitized, checksummed bundle save/load. |

The non-code surface is mature: generated `man/` help, tutorial and methodology
vignettes, examples, citation files, contributor/security guidance, optional
Python requirements, pkgdown configuration, and CI for Windows, macOS, Linux,
oldrel R, release validation, pkgdown, Dependabot, and secret scanning.

## Detailed findings

| Location | Severity | Evidence and impact | Recommendation |
| --- | --- | --- | --- |
| `R/user_experience.R:1042-1060` | High | `semantica_clear_cache()` promises to delete only hashed cache entries, but selects every `*.rds` recursively under a caller-supplied `cache_dir`. An isolated reproduction placed `user-analysis.rds` beside a valid cache entry; after `confirm = TRUE`, both were removed. This can destroy unrelated user analyses. | Delete only files satisfying the exact cache ownership contract: a 32-hex filename in a two-hex shard directory created by SEMANTICA. Reject a non-canonical user directory or require a separate, explicitly named destructive override. Add a regression fixture containing an unrelated RDS. |
| `.Rbuildignore`, `.gitignore`, `semantica_bug_confirmation/` | High | `R CMD build` includes the top-level `semantica_bug_confirmation/` directory. It contains 18 generated CSV/RDS audit and replay artifacts (1,456,738 bytes), including generated-item and replay payloads. This creates a reproducibility, privacy, and accidental-distribution risk. | Remove these local artifacts from a release export; add a narrow ignore/build-ignore rule for `^semantica_bug_confirmation$` (and any other documented runtime-output root). Add a CI tarball assertion covering it. Do not delete the current user directory without explicit approval. |
| `tests/testthat/test-patch-factor-pool-completion.R:490-504` | High | Installed-package testing yielded 1 failure, 1,994 passes, and 11 documented skips. The failure is an `identical()` comparison of visually identical Spanish error text with differing encoding metadata. The recovery behavior itself succeeds, so this is low production risk but a release blocker on this Windows locale. | Test the intended invariant--successful locale-independent retry--not message bytes. If text must be tested, normalize both values with `enc2utf8()` or compare a structured condition field/class. |
| Historical flat compatibility interface | Medium | `R CMD check` reported `embedding_dimension_action` in a deprecated public usage but undocumented in its Rd page. | The deprecated interface was removed; configuration is now documented through `semantica_llm_config()`. |
| `DESCRIPTION:25-28` | Low | `Author:` uses an unaccented third given name while `Authors@R` uses `U+00E9`; `R CMD check` produces a metadata note. | Make the fields identical or remove the redundant `Author:` field and rely on `Authors@R`. |
| `DESCRIPTION:80` | Medium | `Remotes: melissagwolf/dynamic` is an unpinned GitHub reference; the installed dependency records `RemoteRef: HEAD`. Package checks flag `Remotes` as non-standard. This makes GitHub installs non-reproducible and is unsuitable as a release dependency policy. | Verify that the required `dynamic` API exists in its CRAN release and use a versioned `Imports` constraint for release. If a development dependency is truly required, pin an immutable commit for development and document a separate non-CRAN distribution policy. |
| `tests/testthat/test-next-version-interpretation-boundary-docs.R:22-23` | Medium developer-experience defect | `devtools::test()` source-mode run produced nine false documentation failures because `utils::help(..., package = "SEMANTICA")` queries the separately installed package, not the source tree loaded by `load_all()`. The installed-package check demonstrates that the aliases are present; this test should not fail in the documented local workflow. | In source-mode tests inspect `man/` via the package root, or run this assertion only in an installed-package check. Keep the installed-help assertion as a release-check test if desired. |
| `R/pipeline_core.R:8063-8078` and the 15,438-line module | Low technical debt | The private scorer gets many inputs by searching `parent.frame()`, while the principal engine has more than 100 public controls plus compatibility dots. It is tested and currently works, but makes dependencies implicit, refactoring risky, and focused unit tests expensive. | After the release blockers are fixed, extract immutable `SearchContext`, `Scoring`, `DFI`, `ESEM`, and `Finalization` modules. Pass context explicitly. Characterize current results before any extraction; this is not a stylistic rewrite. |
| `vignettes/aco-esem-roadmap.Rmd:144-282`; absence of shipped held-out benchmark results | High scientific release gate | The roadmap itself correctly calls EAS/RAS/MMAS/ACS comparisons, held-out equal-budget benchmarks, and broader empirical validation planned/deferred. Current tests establish contracts and synthetic fixtures, not independent evidence of optimiser superiority or response-level validity. | Before publication claims, preregister and publish exact/synthetic/empirical matched-budget benchmarks, external or nested held-out participant validation, ablations, failed-fit rates, all seeds, and no-claim comparison baselines. Keep current proxy wording until then. |

### Check results

* Parsed: 23 R files and 129 Rd files, with no parse errors.
* Source-mode `devtools::test(stop_on_failure = FALSE)`: reached the suite but
  reported 10 failures before its cap: nine source-vs-installed help lookups and
  the locale encoding assertion above.
* Installed `R CMD check --as-cran --no-manual --no-build-vignettes` under a
  valid `C` locale: source build, installation, namespace, dependency, syntax,
  S3, static-code, Rd metadata/cross-reference, examples, and unstated-dependency
  checks passed. It ended with **1 error, 3 warnings, 4 notes**: the locale test
  failure; undocumented argument; expected no-vignette warnings; and the
  generated-directory/metadata/remote notes described above. Network URL/CRAN
  feasibility notes and the future-timestamp note are environment-caused.
* A full vignette build could not be executed locally because Pandoc is absent.
  The workflow `.github/workflows/release-validation.yaml` installs Pandoc;
  re-run the full release check in that environment after the fixes.
* The installed test run recorded **1,994 passes, 1 failure, 11 skips**. Skips
  are explicit for live services, GPU runtime/hardware, an opt-in full parallel
  integration run, and an opt-in real multi-seed ESEM run.

## Strengths worth preserving

1. **Evidence boundary.** README, package help, summaries, result evidence
   records, and vignettes consistently distinguish semantic proxies from
   respondent evidence. This is scientifically essential and should remain
   non-negotiable.
2. **Credential safety.** The session sanitizer is allow-list based; URL user
   information and query tokens are removed; provider-key forwarding and
   non-loopback insecure HTTP need explicit opt-in.
3. **Reproducibility artifacts.** Configuration hashes, seeds, task metadata,
   canonical result bundles, and an explicit warning that RDS checksums are not
   authenticity signatures are excellent practice.
4. **Structural safeguards.** One-to-one factor alignment, sign anchoring,
   technical ESEM admissibility, pre-ACO feasibility gates, bounded recovery,
   and broker accounting are appropriate protections for a costly search.
5. **Public API design.** The compact `semantica_run()` result retains the full
   canonical analysis under `advanced`; this improves usability without silently
   discarding evidence.
6. **Test and CI culture.** The regression suite is broad, mocks external
   services safely, and CI covers multiple operating systems and an old R
   release. Preserve its explicit opt-in policy for paid/live backends.

## Prioritized roadmap

### Immediate, mandatory before a public/research release

1. Repair `semantica_clear_cache()` scope and add a destructive-path regression
   test. Benefit: prevents user data loss; complexity: low; risk: must retain
   valid legacy cache cleanup.
2. Exclude `semantica_bug_confirmation/` from the build and distribute only a
   clean source tarball. Benefit: avoids unintended artifact/data publication;
   complexity: low; risk: retain any desired evidence in a deliberately reviewed
   archival location.
3. Fix the locale-fragile test and the missing Rd argument, then require a clean
   installed-package test run. Benefit: restores deterministic CI and valid help;
   complexity: low.
4. Reconcile author metadata and adopt a stable `dynamic` dependency policy.
   Benefit: removes release notes and improves reproducibility; complexity: low
   to medium, depending on the minimum supported `dynamic` version.

### Medium-term, strongly recommended

1. Add a CI source-tarball deny-list test for every runtime-output directory and
   an explicit test that an unrelated RDS survives cache cleanup.
2. Add a scheduled, credential-free compatibility matrix and keep a separate
   secure opt-in workflow for live providers, local servers, GPU, and full PSOCK
   integration. Benefit: detects provider/runtime drift without putting secrets
   in ordinary CI.
3. Treat exact remote model revision, embedding specification, and cache
   namespace as required report fields when external models are used.

### Long-term, evidence-gated

1. Split `pipeline_core.R` only behind characterization tests and an explicit
   context object. Benefit: lower maintenance and safer methodological changes;
   complexity: high.
2. Conduct and publish the roadmap's matched-budget optimiser and independent
   response-data studies before asserting comparative performance. This is a
   scientific validation task, not a refactor.

## Scientific assessment and references

The use of ACO for short-form selection, ESEM, RMSEA-power anchors, dynamic fit
indices, and embedding geometry has legitimate methodological antecedents. The
repository accurately treats its combination of embedding-derived matrices with
PFA/ESEM/DFI as a package-specific proxy adaptation, not a transfer of
respondent-data validity claims. The appropriate next step is empirical
calibration and external validation, not stronger proxy cutoffs.

* Leite, Huang, and Marcoulides (2008), ACO scale-shortening framework,
  https://doi.org/10.1080/00273170802285743
* Asparouhov and Muthen (2009), ESEM framework,
  https://doi.org/10.1080/10705510903008204
* MacCallum, Browne, and Sugawara (1996), RMSEA power,
  https://doi.org/10.1037/1082-989X.1.2.130
* Wolf and McNeish (2023), `dynamic` fit-index cutoffs,
  https://doi.org/10.1080/00273171.2022.2163476
* Wulff and Mata (2025), semantic embeddings in psychological measurement,
  https://doi.org/10.1038/s41562-024-02089-y
* CRAN Repository Policy, especially source dependencies, minimal artifacts,
  test resource use, and clean checks:
  https://cran.r-project.org/web/packages/policies.html
* R Packages (2e), dependency, testing, documentation, and release guidance:
  https://r-pkgs.org/

