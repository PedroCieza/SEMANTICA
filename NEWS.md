# SEMANTICA 0.2

## GitHub release preparation and citation

- Synchronizes GitHub-facing version and installation guidance with 0.2.
- Adds machine-readable `CITATION.cff`, fixes `inst/CITATION` version reporting,
  and adds a methodological-foundations article that distinguishes software
  citation from method-specific references.
- Adds repository community/security templates and release-integrity CI checks.
- Removes hard namespace imports for the optional categorical `dynamic` helpers
  `catHB`/`catOne`. When present they are called exactly as before; when absent,
  the already-existing SEMANTICA DFI simulation fallback is used.
- No ACO objective, pheromone update, ESEM/PFA calculation, semantic score, item
  generation, embedding calculation, evidence rule, RNG policy, resource policy,
  or participant-response validation calculation is changed.


## Core plots retained in high-level results

- Extends the high-level result surface only: no ACO, ESEM, PFA, embedding, item-selection, scoring, evidence, RNG, resource, or backend calculation is changed.
- The default `semantica_full_pipeline()` plot level now retains a compact five-plot report set under `$plots`: `plot_summary_of_results`, `plot_fitness_evolution`, `plot_esem_before`, `plot_esem_after`, and `plot_pfa_diagnostics`.
- Reuses the already-established visualization functions (`plot_fitness_evolution()`, `plot_esem_path_diagrams()`, `plot_pfa_diagnostics()`, and `plot_summary_of_results()`) rather than introducing parallel plotting logic.
- Keeps the default BEFORE path panel on the established fast sample-free proxy representation. A new full-pool ESEM is not fitted merely to populate the result wrapper; the existing explicit `before_path_model = "refit"` advanced option remains available.
- `semantica_run()` now exposes a sixth regular-user group, `$plots`, alongside `scale`, `items`, `diagnostics`, `provenance`, and `advanced`. The complete canonical result remains under `$advanced`.
- Direct `semantica_full_pipeline()` results expose the same five core plots at `$plots` with the default `plots = semantica_plot_config(level = "summary")`; `level = "full"` continues to retain the complete visualization set and `level = "none"` disables plot generation.
- Saving at the summary plot level now saves the same core report plot set through the existing plot manifest/save machinery.

# SEMANTICA 0.5.8.2

## Compact `semantica_run()` result surface

- Changes only the regular-user return surface: `semantica_run()` now returns five visible top-level groups (`scale`, `items`, `diagnostics`, `provenance`, `advanced`) instead of exposing the complete canonical result as the first level in RStudio.
- Preserves the complete canonical full-pipeline result unchanged under `$advanced`; no analytical component is deleted, renamed, recomputed, or simplified.
- Consequently, `names(result)` and `length(result)` intentionally describe the five regular-user groups; use `names(result$advanced)` / `length(result$advanced)` when code needs the canonical top-level inventory.
- Keeps historical direct access such as `result$optimization`, `result$fit_indices`, `result[["esem_state"]]`, and character-name subsetting through compatibility accessors, reducing breakage for existing scripts.
- Direct `semantica_full_pipeline()` output is intentionally unchanged and continues to expose the complete advanced result object directly.
- `semantica_view(..., view = "raw")` now explicitly returns the canonical result (`result$advanced` for regular-interface results), while the advanced component map is built from that canonical object rather than from the five facade groups.
- `semantica_result_info()` distinguishes the visible facade size from the retained canonical component count.
- Participant-response attachment and bundle save/load unwrap and re-wrap the regular-user facade at their boundaries so validation, serialization integrity, and canonical evidence storage remain unchanged.
- Adds regression tests for five-group visibility, complete canonical retention, legacy field access, advanced-map coverage, regular-interface provenance, and raw-view identity.

No ACO scoring, pheromone update, archive logic, item generation, embedding, semantic objective, PFA/ESEM/DFI calculation, evidence rule, matrix repair, RNG, resource allocation, backend request, or other analytical procedure is changed.

# SEMANTICA 0.5.8.1

## Result-surface documentation hotfix

- Fixes an R parse error in the new compact result facade caused by using the reserved control-flow keyword `next` as an unquoted list-field name; the presentation-only field is now named `next_steps`.
- Co-locates the two result-view print methods with `semantica_view()` in the roxygen source so `devtools::document()` can regenerate their aliases and arguments consistently.
- Adds regression coverage for the `next_steps` field name.
- No canonical result component, analytical calculation, ACO behavior, generation/embedding procedure, ESEM/PFA/DFI method, evidence rule, RNG behavior, resource allocation, backend request, or serialization logic is changed.

# SEMANTICA 0.5.8

## Result-surface quality-of-life release

- Adds `semantica_view()` as a read-only result facade so completed runs no longer require users to navigate dozens of top-level list components for routine interpretation.
- The default view is interface-aware: results created through `semantica_run()` open as a compact scale/review view, while direct `semantica_full_pipeline()` results open as a nine-section advanced component map.
- The advanced map groups every retained top-level component into `scale`, `generation`, `content`, `semantic`, `structural`, `optimization`, `evidence`, `outputs`, or `provenance`; requesting a section returns those original stored components without recomputation. No component is removed from or renamed inside the canonical result object.
- Compact views foreground what a scale developer usually needs first: selected wording, dimensionality, factor count, best observed objective, participant-data status, reporting-level review flags, factor review, evidence status, and model identity.
- `print(result)` now delegates to the interface-aware facade while `summary(result)` remains the detailed diagnostic report. `semantica_items()`, `semantica_diagnostics()`, `semantica_provenance()`, bundle serialization, and direct `$` access remain unchanged.
- `semantica_result_info()` now reports the originating interface, retained top-level component count, and the section counts used by the result facade.
- Documentation now explains why RStudio may still display `list [N]` in the Environment pane: that inspector reflects the intentionally preserved canonical list, whereas `semantica_view()` is the supported human-facing presentation layer.

No ACO scoring, pheromone update, archive ranking, generation, embedding, semantic objective, PFA/ESEM/DFI calculation, evidence rule, matrix repair, RNG, resource allocation, backend request, participant-validation, or serialization-integrity procedure is changed by this release.

# SEMANTICA 0.5.7.3

- Restores the default `summary(result)` path after the 0.5.7.2 section-filtering QoL addition; `sections = "all"` is again the default while explicit section filtering remains available.
- Repairs documentation generation for provenance/result-info and multi-seed S3 methods, and wraps the diagnostics usage declaration for portable PDF manuals.
- Replaces non-ASCII decorative plot-caption separators with ASCII equivalents for portable R source checks.
- No analytical, optimization, embedding, ESEM/PFA/DFI, evidence, RNG, resource, or backend procedures were changed.

# SEMANTICA 0.5.7.2

## Advanced user-experience refinement

- Makes `print(result)` concise while retaining the complete diagnostic report in `summary(result)`; detailed summaries can now be filtered by section.
- Adds credential-safe printing for configuration objects and stable accessors for provenance, result metadata, item/factor review tables, and diagnostic sections.
- Adds `semantica_execute()` for executing a previously inspected run plan without duplicating the run specification; credentials are not stored in the plan.
- Adds `semantica_validate()` to attach participant-response validation to an existing selected scale using the established response-data ESEM path without regenerating items or rerunning ACO.
- Adds human-readable evidence labels as an opt-in while preserving raw machine-readable status tokens by default.
- Adds selective plotting, quiet plot generation, and a plot manifest; presentation wording now refers to the best observed objective and configured review thresholds rather than a global optimum or danger zone.
- Clarifies report export versus optimizer-interchange reload semantics, adds `semantica_reload_optimizer()`, and qualifies `utils::capture.output()` to remove the R CMD check NOTE seen in 0.5.7.1.
- Adds S3 print/summary/plot behavior for multi-seed results without changing multi-seed optimization calculations.
- Finishes task-oriented documentation cleanup and removes repetitive audience labels and `research-track` qualifiers from package-authored help text.

No ACO scoring, pheromone update, archive ranking, semantic objective, embedding calculation, PFA/ESEM/DFI estimator, fit calibration, evidence rule, resource-allocation algorithm, RNG behavior, or backend request logic is changed by this release.

# SEMANTICA 0.5.7.1

## Documentation and naming cleanup

- Refines the package help, README, and workflow documentation so the main and extended interfaces are presented by task rather than by user labels or defensive API explanations.
- Replaces redundant research-context qualifiers in internal files, helper identifiers, tests, benchmarks, headings, and package-authored prose with context-specific terms such as `decision`, `analysis`, `validation`, `method`, or `interpretation`.
- Renames the validation vignette to `evidence-interpretation` and updates pkgdown references.
- No scoring formulas, optimizer mechanics, embedding calculations, PFA/ESEM/DFI procedures, evidence rules, resource algorithms, RNG behavior, or backend request logic are changed.

# SEMANTICA 0.5.7

## User-facing quality-of-life release

This release intentionally leaves SEMANTICA's calibrated analytical core unchanged. It reorganizes the regular-user experience around the existing canonical engine.

- Makes `semantica_run()` the unambiguous standard entry point across package help, README, vignettes, and pkgdown.
- Adds direct casual controls for generation language, response format, item style, temperature, and structured-output preference.
- Adds concise/default, detailed, and quiet progress modes at the casual-wrapper layer without changing warnings or analysis.
- Adds `semantica_run_plan()` for no-call workload/count/facet previews and `semantica_check_setup()` for user-oriented backend readiness.
- Adds stable high-level accessors: `semantica_items()`, `semantica_overview()`, `semantica_config()`, and `semantica_models()`.
- Adds `plot()` support for high-level results and lets `semantica_plot_all()` accept them directly; optional plot failures are summarized at the end.
- Extends `semantica_export()` so high-level results export the selected scale, evidence status, readable summary, and sanitized configuration while preserving the legacy component export contract.
- Adds cache inspection/explicit clearing helpers and surfaces resource reset/recovery documentation.
- Promotes the validated `semantica_import_embeddings()` boundary and documents three distinct starting-material workflows: construct-only, existing item text, and external embeddings.
- Adds dedicated regular-user workflow, troubleshooting, and glossary vignettes, plus participant-data and local-backend recipes.
- Reorganizes the pkgdown reference by user task and separates regular, advanced applied, component, and research-track APIs.

No ACO scoring, pheromone logic, semantic objective, embedding computation, PFA, ESEM, DFI, evidence accounting, matrix repair, RNG, resource-allocation algorithm, serialization-integrity, or participant-validation procedure was modified by this release.

# SEMANTICA 0.5.6.2

## Bundle integrity hotfix

- Fixed false bundle-integrity failures when complete results contain S4 fitted-model objects (notably lavaan ESEM fits) whose runtime-only internals can serialize differently after an otherwise lossless RDS round trip.
- Bundle schema 4 now verifies the existing canonical checksum as the primary integrity check. Its canonical projection preserves ordinary analysis data and recursively covers S4 slots, while replacing environments, functions, external pointers, and other runtime-only representation details with deterministic placeholders.
- The full fitted-model object is still stored unchanged in the bundle. Only checksum construction is representation-tolerant. Changes to covered analysis data/slots still invalidate the bundle.
- Schema-3 and earlier bundles retain the previous exact-MD5/canonical-fallback verification path.
- No ACO, semantic objective, PFA, ESEM estimation/scoring, DFI, embedding, generation, or resource-management behavior changed.

# SEMANTICA 0.5.6.1

## Integration and check hardening

- Synchronizes the 0.5.6 regression contracts with strict fail-fast handling of unknown analysis-configuration fields.
- Documents `elite_multicriteria_rerank` as the canonical scalar final rerank and retains `elite_pareto_rerank` only as a warning-producing compatibility alias.
- Removes incidental deprecation warnings from ordinary optimizer tests while retaining a dedicated compatibility test.
- Records the explicit `held_out_empirical_calibration` upgrade path in objective/evidence provenance, clarifying that agreement among embedding-derived subdiagnostics is not independent validation.
- Removes an invalid Rd cross-reference to the internal `estimate_within_similarity_targets()` helper.
- No ACO pheromone mechanics, semantic scoring formulas, PFA/ESEM estimation, DFI calibration, or backend-selection behavior is changed by this patch.

# SEMANTICA 0.5.5

## Low-risk release and reproducibility integrity patch

- Expands the narrow build/VCS ignore rules to exclude SEMANTICA-generated `*_test` replay/stress directories (including versioned test directories and frozen-pool test directories) from source-package assembly, and adds a single Linux-release CI tarball-content guard for those directory shapes. Runtime package behavior is unchanged.
- Makes persistent embedding cache identity endpoint-aware by hashing a credential-free embedding endpoint into the cache key and bumps the internal cache schema from 6 to 7. This prevents different OpenAI-compatible servers that reuse the same model label from sharing cached vectors. Raw endpoint URLs and query credentials are not stored in cache keys.
- Fixes `estimate_recommended_validation_n()` RNG cleanup so a call made when `.Random.seed` did not previously exist removes the temporary seed on exit; an existing caller RNG state is restored exactly. The Monte Carlo calculations themselves are unchanged.
- Adds regression coverage for endpoint cache separation and both absent/present caller-RNG restoration paths. No generation, embedding computation, ACO, PFA, ESEM, DFI, semantic scoring, matrix-repair, response-validation, or public API behavior is changed.

# SEMANTICA 0.5.4

## Seed-replay semantics clarification

- Clarifies rather than changes LLM generation behavior. Ollama seed propagation, temperature, output mode, prompt construction, retry logic, parsing, lexical curation, and all downstream analysis procedures are unchanged.
- Separates the stable generation seed schedule from the observed execution ledger. `generation_seed_schedule` contains only seed-control fields, while `task_seed_ledger` remains the richer execution trace whose prompt fingerprints may legitimately diverge when earlier stochastic backend outputs alter later anti-duplication context.
- Replaces the ambiguous provenance interpretation of `generation_contract_fingerprint` with an explicit `generation_spec_fingerprint`, while retaining `generation_contract_fingerprint` with its 0.5.3 hashing contract for backward compatibility.
- Adds `generation_replay_plan_fingerprint`, which fingerprints the generation specification, requested master seed, and realized seed schedule. It identifies the controlled replay plan, not the realized LLM text.
- Retains `item_pool_fingerprint` as the identity of the realized candidate text. Exact downstream analysis replay therefore means saving and reusing the realized item pool, not assuming a backend will regenerate byte-identical text from the same seed.
- Records `exact_text_replay_guaranteed = FALSE` and prints this limitation when a backend seed is controlled. This reflects observed backend/runtime nondeterminism and avoids presenting seed control as a text-replay guarantee.
- Does not switch Ollama structured output from JSON to numbered text: controlled stress tests showed replay variance under both modes.
- Adds regression coverage showing that two same-seed runs may realize different item pools while preserving the same SEMANTICA seed schedule/replay-plan fingerprint. No ACO, PFA, ESEM, DFI, embedding, resource, or scoring behavior is changed.

# SEMANTICA 0.5.3

## Generation reproducibility and provenance hardening

- Adds backend-aware generation seeding without changing SEMANTICA's analysis objectives. A run master seed is now inherited by LLM generation in the high-level pipeline; for Ollama, SEMANTICA derives deterministic per-call task seeds and forwards them through the documented `options$seed` contract. Unsupported generation protocols are explicitly recorded as uncontrolled rather than silently treated as reproducible.
- Keeps the new lower-level `generation_seed` control name-only after `...`, preserving the complete positional argument order of existing `semantica_pipeline()` and `semantica_full_pipeline_custom()` calls. `semantica_generate_items()` adds `seed` only as a trailing optional argument.
- Derives generation task seeds from a stable hash of the master seed and generation-unit identity rather than consuming caller RNG state. This preserves SEMANTICA's existing RNG isolation while giving distinct factors/retries distinct deterministic seeds.
- Adds `semantica-generation-provenance-v1`: per-call prompt fingerprints, task-seed ledger, generation contract fingerprint, exact retained item-pool fingerprint, backend seed-control mechanism/status, and downstream content-screening state. The provenance record is propagated through `semantica_pipeline()`, full-pipeline reproducibility metadata, and the casual-run configuration.
- Clarifies the generation stage as candidate production rather than construct validation. Console output now says `Retained ... generated candidates (pre-alignment)` and explicitly records that parsing/lexical curation precede downstream construct-definition screening. No generated item is declared content-valid merely because it survived generation-stage curation.
- Decomposes the existing representation-aware content guard into transparent reason provenance: robust factor-definition mismatch, robust analyst-specified forbidden-concept conflict, overlap of both, and raw conflicts retained because preprocessing sensitivity disagreed. This adds no new exclusion criterion.
- Refines operational pool-health wording so ordinary ambiguity without a clear mismatch is reported as `adequate_capacity_with_ambiguity` rather than automatically `content_mixed`. Counts remain primary and no new validity cutoff is introduced.
- Makes patience-based ACO termination explicit in verbose output. `patience_exhausted` now prints a STOP reason and retains the existing statement that global optimality is not established. Search mechanics are unchanged.
- Reorients multidimensional semantic reporting around the within-minus-between separation gap, which is already implied by the `relative_conservative` objective. The within-similarity target is still reported as a cohesion guard, but moving away from that target is no longer described as inherently undesirable when relative factor separation improves.
- Labels the final ESEM quantity as a `Structure-weighted ESEM proxy score` when that scoring mode is active and prints its scaled components. The score formula, fit references, PFA/ESEM weights, and DFI machinery are unchanged.
- Adds regression tests for deterministic task-seed derivation, caller-RNG neutrality, seeded Ollama propagation/provenance, explicit unsupported-protocol status, item-pool fingerprint stability, and relative-separation reporting.
- Preserves ACO pheromone mechanics, evaporation, archive logic, semantic/PFA/ESEM weights and equations, stochastic-superiority and robust-gap calculations, PFA continuous scoring, HTMT references, representation-aware guarding, embedding preprocessing policy, resource planning, and empirical calibration.

# SEMANTICA 0.5.2

## Representation-aware content guard and frozen-item backend diagnostics

- Makes automatic construct-content exclusion robust to a representation perturbation without changing the active semantic representation. Raw clear factor mismatches or explicit forbidden-concept conflicts can exclude an item only when the same exclusionary conclusion is reproduced after mean-centering the item/reference embedding space. Raw/centered disagreement retains the item for ACO and is recorded as uncertainty. Mean-centered values never enter the ACO objective, PFA/ESEM scores, semantic scores, or backend selection.
- Adds explicit raw-versus-sensitivity alignment metadata (`semantica_*_centered`, `semantica_content_guard_pass_raw`, and `semantica_content_guard_sensitivity`) so downstream audits can distinguish a robust exclusion from a representation-sensitive one. Ambiguity remains non-exclusionary.
- Extends the representation evidence state to schema `representation-evidence-v2`, preserving the established summary status for compatibility while adding separate concentration and continuous preprocessing-sensitivity axes (off-diagonal correlation, q95 absolute change, top-pair Jaccard, random-overlap reference, and excess-over-random). No new universal anisotropy cutoff is introduced.
- Adds descriptive guard-pressure provenance to ACO results: guard retention, semantic retention after guarding, target/post-guard selection pressure, and target/eligible selection pressure. These quantities describe how constrained the search space is; they do not alter optimization.
- Adds `semantica_compare_embedding_representations()` for frozen-item backend comparisons. It requires identical item IDs across similarity matrices and reports per-representation semantic geometry plus cross-backend Pearson/Spearman agreement, top-pair overlap, and optional selected-item overlap. It never ensembles representations or automatically declares one backend valid.
- Adds regression tests for representation-sensitive exclusion behavior, the v2 representation evidence schema, and frozen-item backend comparison invariants.
- Adds a narrow `.Rbuildignore` rule for `SEMANTICA_bounded_audit_*` development-output directories so locally generated audit artifacts do not create top-level-file NOTEs in source-package checks.
- Preserves ACO pheromone mechanics, evaporation, semantic/PFA/ESEM weights, `relative_conservative` scoring, stochastic-superiority calculations, PFA continuous scoring, ESEM/DFI formulas, backend routing, resource planning, and empirical-calibration estimators.

# SEMANTICA 0.5.1

## Objective-regression and clustering-matrix patch

- Fixes `semantica_cluster_consensus()` and the optional ensemble-clustering helper so conversion from similarity to distance preserves matrix dimensions on R versions where scalar-first `pmax()` drops matrix attributes.
- Makes the `relative_conservative` multidimensional cohesion guard genuinely location-shift invariant by comparing each factor's median within-factor similarity with its correspondingly shifted adaptive target. Redundancy remains handled separately by the duplicate penalty; no new absolute cosine cutoff is introduced.
- Fixes ACO proposal tie handling so contrastive construct-definition margins can break exact relative-semantic ties lexicographically, while equal semantic/alignment evidence remains exactly tied and alignment can never overturn a superior relative semantic score.
- Corrects new 0.5.0 regression fixtures that used unsupported `diag(..., dimnames=...)` calls or a vector-valued `grepl()` pattern, and replaces an inappropriate "overlapping" negative-control expectation with a genuinely collapsed representation in which within- and between-factor relations are identical.
- Clarifies the collapsed-representation invariant test to use the explicit `robust_median_gap` field rather than relying on R's partial `$` matching against `robust_gap_scale`.
- Adds regression coverage for the ensemble-clustering matrix-dimension path. No ACO pheromone mechanics, PFA/ESEM equations, backend routing, resource planning, or empirical-calibration estimators are changed.

# SEMANTICA 0.5.0

## Representation-qualified semantic scale construction

- Adds an explicit representation-evidence state that records embedding-space concentration and preprocessing sensitivity and propagates that qualification to downstream semantic, PFA, ESEM, and clustering evidence. SEMANTICA does not silently center, whiten, or otherwise choose a more favorable representation.
- Replaces multidimensional target-centered semantic optimization with `relative_conservative`, which combines within-versus-between stochastic superiority and a robust scale-relative separation component. The 0.4.x objective remains available as `legacy_target_burden`; unidimensional scales retain target-centered scoring because between-factor comparisons are undefined.
- Demotes adaptive within-factor cosine targets to cohesion/redundancy guards rather than treating the current embedding model's absolute cosine level as the primary definition of multidimensional semantic quality.
- Replaces the legacy random half-pair zeroing stability heuristic and its uncalibrated binary 0.10 boundary with stratified within/between pair resampling and leave-one-item-out sensitivity summaries. These are explicitly representation-sensitivity diagnostics, not respondent-population confidence intervals.
- Changes sample-free ESEM and HTMT-style output language from psychometric PASS/FAIL terminology to proxy `REFERENCE MET` / `REFERENCE NOT MET` terminology. Search-time reference values remain available to the optimizer.
- Promotes the existing construct-definition alignment machinery as a conservative feasibility-aware guard in the casual pipeline. Clear intended-factor mismatches or explicit exclusion conflicts can be removed only when enough alternatives remain; ambiguous items remain available.
- Improves generation-pool diversity by using the established 2x casual overgeneration default, deterministic lexical duplicate/near-duplicate screening, max-min lexical diversity curation, and the existing deficit-aware replenishment mechanism. Embedding-based diagnostics do not automatically trigger LLM regeneration, avoiding circular backend feedback.
- Adds `semantica_cluster_consensus()` as an embedding-semantic structural diagnostic using multiple deterministic hierarchical clustering views. It remains diagnostic and does not add an unvalidated new ACO weight.
- Redesigns sample-free PFA proposal scoring so a value of 1.0 is no longer reached solely because threshold references are crossed. Threshold-attainment fields are retained for compatibility, while optimization uses continuous primary-loading magnitude, positive loading-margin information, and chance-adjusted partition recovery when estimable.
- Adds explicit evidence-family provenance. Semantic discrimination, PFA, ESEM, and clustering derived from the same embeddings are marked as dependent `embedding_semantic` corroboration; participant-response analyses are a separate `response_data` family.
- Versions the changed optimization/proposal contracts (`SEMANTICA-objective-v4`, `semantic-relative-v2`, and `pfa-proposal-v2`) so old and new utilities cannot be silently interpreted as the same scale.
- Strengthens empirical-calibration provenance. Leave-one-scale-out calibration requires unambiguous scale identities and records held-out versus training instruments for each fold; pilot calibration remains explicitly developmental and requires independent confirmatory validation.
- Adds adversarial regression tests for monotone-similarity invariance, collapsed representations, correlated-factor structures, conservative contrastive alignment, generation diversity, evidence provenance, benchmark protocol, and the new PFA/semantic objectives.
- Formalizes the benchmark contract for future ACO reweighting: competing methods should receive the same declared resource budget and seed design, and default weights should not be changed without external validation evidence, preferably held-out participant responses. No arbitrary new semantic/PFA/ESEM weight vector is introduced in 0.5.0.
- Preserves ACO pheromone mechanics, evaporation, elite archives, convergence controls, PFA/ESEM cadence, cache/coalescing behavior, backend routing, adaptive resource management, and explicit user worker overrides.

# SEMANTICA 0.4.7

## Adaptive auto-worker headroom patch

- Fixes automatic PSOCK CPU accounting so the main/coordinator R process is budgeted separately from `reserve.cores`. Under the default `reserve.cores = 1`, one CPU slot is now genuine user/OS headroom instead of being implicitly consumed by the coordinating R session.
- Applies a physical-core worker budget whenever physical-core information is available, including high-memory systems where 0.4.6 previously allowed the memory cap to dominate and could select nearly every logical CPU. The physical count is clamped to scheduler/cgroup-visible CPU allocation before use.
- Preserves the existing memory-aware PSOCK cap and combines it with the CPU cap by taking the most conservative detectable automatic limit. Explicit numeric worker requests and serial mode remain unchanged and user-authoritative.
- Extends resource-plan/telemetry metadata with coordinator and physical-worker-cap fields and labels the casual automatic policy as `adaptive_auto_psock_v2`.
- No change is made to item generation, embeddings, cosine computation, ACO scoring/search equations, PFA, ESEM, DFI, evidence archives, or finalist reranking.

# SEMANTICA 0.4.6

## Memory-aware auto resources and telemetry-clarity patch

- Makes `n.cores = "auto"` memory-aware for PSOCK execution. When host available memory and the current R process resident-set size can be measured, SEMANTICA caps automatic workers using a scale-free two-copy transient allowance reflecting PSOCK serialization plus private worker materialization. Explicit numeric worker requests remain user-authoritative. If memory cannot be measured portably, automatic mode falls back to the physical-core allocation when available rather than inventing a fixed worker ceiling.
- Adds `workers = "auto"` to the casual `semantica_run()` interface. Users may supply a positive integer or `"serial"` without dropping to the advanced pipeline. The resolved worker request/effective count is recorded in casual-run provenance.
- Updates `semantica_capabilities()` and resource-plan printing so the reported default auto worker count reflects the memory-aware policy rather than simply `available cores - 1`.
- Improves one-factor ACO observability only: tiny nonzero semantic raw losses now use scientific notation when fixed four-decimal formatting would misleadingly print `0.0000`. Scoring is unchanged.
- Corrects ESEM checkpoint console telemetry to report checkpoint-local request/coalescing counts instead of the cumulative broker coalescing counter. Labels now distinguish requests, unique candidates, cache hits, coalesced duplicates, newly started fits, and admissible requests. Cumulative broker telemetry and all scoring remain unchanged.
- Clarifies final ESEM summary/warning wording so fit success counts refer to unique fitted candidates rather than all requested/cached/coalesced candidate evaluations.
- No change is made to ACO scoring, Huber target-centered unidimensional scoring, PFA, ESEM fit formulas, DFI, embeddings, generation, candidate guards, evidence archives, or finalist reranking.

# SEMANTICA 0.4.5

## ESEM cadence type-consistency patch

- Fixes `.semantica_resolve_esem_interval()` so adaptive ESEM checkpoint intervals are returned as integer scalars, matching the fixed-cadence branch, downstream iteration-index semantics, and the package regression contract. The numerical cadence is unchanged (for example, `10L` with entropy `0.80` still resolves to `8L`).
- Adds regression coverage for integer return type across fixed and adaptive cadence modes, including non-finite entropy fallback.
- No change is made to unidimensional target-centered scoring, ESEM checkpoint timing, ACO search behavior, PFA, DFI, embeddings, generation, resource planning, or multidimensional behavior.

# SEMANTICA 0.4.4

## Unidimensional objective-resolution and reporting patch

- Replaces the flat one-factor within-target band with a target-centered Huber loss whose transition point is the existing `within_similarity_band`. This gives one-factor ACO meaningful semantic ranking resolution inside the acceptable cohesion region without maximizing raw similarity or adding a new arbitrary tuning weight. Multidimensional semantic scoring is unchanged.
- Casual `semantica_run()` ACO presets now use fixed ESEM checkpoint cadences matching their documented contracts (`standard`/`fast`: every 10 iterations; `full`: every 5). The advanced/full pipeline remains adaptive by default through `semantica_esem_config(cadence_mode = "adaptive")`, preserving backward compatibility while reducing unnecessary expensive ESEM fits in casual runs.
- One-factor HTMT contribution is represented as `NA`/not applicable in score decomposition and console output rather than as a favorable `1.0` multiplier; the numerical one-factor ESEM score is unchanged by this reporting correction.
- One-factor summaries no longer report tautological 100% top-factor content alignment. They report relative factor alignment as not applicable and retain median item-to-definition similarity as a descriptive representation metric.
- Semantic-discrimination objects now retain the true number of available within-factor pairs when between-factor pairs do not exist.
- Recovered partial-generation responses are recorded in generation provenance and verbose replenishment messages without leaking an end-of-run parser warning after the requested pool is successfully completed. Non-faceted generation wording no longer claims that counts were allocated across facets.
- No change is made to multidimensional ACO scoring, PFA, ESEM fit formulas, DFI, embedding backends, model-specific embedding instructions, duplicate feasibility, or participant-validation boundaries.

# SEMANTICA 0.4.3

## Deficit-aware LLM generation replenishment

- Fixes generation retries after a partial usable LLM response. SEMANTICA now preserves accepted items and sizes the next request from the remaining deficit and the observed new-item yield, rather than regenerating at least the full original target. For example, 36 usable items from a 40-item request imply a 90% observed yield, so a four-item deficit requests five new candidates rather than another 40.
- Replenishment requests never exceed the larger of the unresolved deficit and the initial request size, and fall back to the unresolved deficit when no successful yield estimate exists. A backend timeout therefore does not erase the last valid yield estimate or cause a later retry to expand back to the full target.
- Retry parsing now treats the number still needed as the minimum usable response for that attempt. Responses that are shorter than the replenishment request but sufficient to complete the target are retained without a misleading below-minimum warning; shorter-than-requested metadata is still recorded.
- Generation provenance now records `needed_before`, `needed_after`, `newly_retained`, per-attempt new-item yield, cross-attempt duplicate counts, and `replenishment_policy = "yield_adaptive_v1"`. Verbose runs print the remaining deficit, replenishment request size, and observed new-item yield before each retry.
- No changes are made to unidimensional handling, item-count semantics, ACO, PFA, ESEM, DFI, embeddings, quality thresholds, or the advanced/casual analytical defaults.

# SEMANTICA 0.4.2

## Backward-compatible summary reporting fix

- Fixes `summary.semantica_full_pipeline_result()` when `fit_indices` is stored as a named atomic vector, as occurs in legacy/lightweight results and several context-reporting regression fixtures. The new unidimensional reporting branch now reads nested diagnostics only when the fit-index object is a list.
- Consolidates the type-safe fit-index accessor used by compact diagnostic sections and the main summary method, preventing `$ operator is invalid for atomic vectors` without changing any unidimensional or multidimensional calculations.
- No ACO, PFA, ESEM, DFI, embedding, dimensionality-detection, item-count, threshold, scoring, or selection behavior is changed.

# SEMANTICA 0.4.1

## Unidimensional casual-analysis branch

- `semantica_run()` now detects a one-factor theoretical model automatically; no new user flag is required.
- Casual one-factor runs use a dimensionality-aware final-form default of 4 selected items. Three-indicator one-factor covariance models have zero degrees of freedom, so the casual interface rejects fewer than 4 selected items rather than reporting globally perfect fit by construction. The advanced pipeline remains available for deliberately shorter forms.
- PFA partition/factor-recovery guidance is marked not applicable for one-factor models instead of being interpreted as failed or zero-quality evidence. Multidimensional PFA defaults are unchanged.
- One-factor ESEM uses no rotation and retains automatic proxy reference N and structure-weighted scoring. Comparative HTMT, cross-loading/dominance, between-factor gap, and stochastic-superiority A evidence are explicitly not applicable.
- One-factor structural screening now combines loading strength, AVE-like semantic proxy information, residual reproduction, centered residual-dependence summaries, descriptive eigenvalue dominance, and proxy-N sensitivity. These remain sample-free semantic proxies and are not participant-based tests of unidimensionality.
- Semantic ACO scoring renormalizes over evidence components that actually exist; absence of between-factor pairs no longer dilutes a one-factor semantic objective.
- Legacy/full result reports now use the same one-factor evidence semantics, avoiding favorable zeros or comparative language for quantities that are undefined in a unidimensional model.
- Multidimensional `semantica_run()` and advanced `semantica_full_pipeline()` behavior remain unchanged except for additive reporting support needed to represent dimensionality correctly.

# SEMANTICA 0.4.0

## Casual-user pipeline and literature-informed ACO presets

- Adds `semantica_run()`, a thin progressive-disclosure wrapper around `semantica_full_pipeline()`. The wrapper simplifies configuration only; it delegates all analytical work to the established full pipeline and preserves representation diagnostics, duplicate feasibility, evidence-stratified archives, canonical finalist reranking, PFA/ESEM diagnostics, and evidence/provenance boundaries.
- Adds `semantica_aco_config()` with `fast`, `standard`, and `full` evidence/resource profiles. The presets are literature-informed rather than presented as universally optimal: psychometric ACO work commonly uses colony sizes from roughly 20 to 60+ ants, recommends increasing effort with the search space, and illustrates 60 ants with a 40-iteration non-improvement rule.
- `fast` uses semantic + ESEM evidence during ACO; PFA remains enabled for final diagnostics but does not guide search. `standard` uses objective-mode PFA every 5 iterations and ESEM every 10. `full` uses PFA and ESEM every 5 iterations and enables strict ESEM-parametric DFI calibration.
- The casual interface fixes PFA search/final extraction at ML, rotation at oblimin, and failure policy at `semantic_fallback`; ESEM uses `proxy_reference_n = "auto"`, geomin rotation, structure-weighted scoring, and semantic fallback. These choices affect only `semantica_run()` presets and do not change advanced API defaults.
- Casual ACO presets use one adaptive evaporation schedule from `rho = .20` to `.05` (80% to 95% pheromone retention), keeping the pheromone model constant across evidence-depth profiles. This follows the ACO principle of higher early exploration and later exploitation while remaining within retention/evaporation ranges used in psychometric ACO applications.
- `pool_items` and `selected_items` remain explicit casual-user controls. `overgenerate` is also explicit and defaults to `1` in `semantica_run()` as requested; the existing `semantica_generation_config()` default used by advanced pipelines is unchanged.
- Casual factor definitions accept the concise `Factor = "definition"` syntax and are normalized to the full factor contract before delegation. Empty substantive factor descriptions are rejected early.
- `prompts` provides safe prompt augmentation (`global` and optional `by_factor`) and appends user instructions to SEMANTICA's internal generation contracts instead of replacing structural/formatting safeguards.
- Adds regression tests that verify preset evidence regimes, PFA/ESEM settings, DFI mode, factor shorthand, prompt augmentation, model routing, item-count/overgeneration controls, and strict delegation to the existing full pipeline.

# SEMANTICA 0.3.3

## Check/test hygiene fixes

- Fixed the 0.3.2 targeted-integrity regression test to use the existing deterministic `semantica_test_three_factor_fixture()` helper. This is a test-suite wiring correction only; production ACO, patience, evaporation, PFA, ESEM, embedding, and diagnostic behavior are unchanged.
- Added a narrow `.Rbuildignore` rule for conventional user-generated `SEMANTICA_*_demo`, `SEMANTICA_*_results`, and `SEMANTICA_*_output` directories created while running examples from a package checkout. This prevents such analysis artifacts from producing top-level-file NOTEs during `R CMD check`; it does not alter runtime output paths or package behavior.

# SEMANTICA 0.3.2

## Targeted integrity and reporting patch

- Makes Ollama registry preflight resilient to a one-off local transport failure and preserves the actual probe error when availability still cannot be confirmed. The message no longer equates every registry-probe exception with a definitively unreachable Ollama server; actual provider requests remain authoritative.
- Prevents the simplified and custom pipeline wrappers from manufacturing a legacy `max.iter` conflict when users supply only `search_patience`. A compatibility warning is retained when both values are genuinely supplied with different values.
- Corrects the final verbose score line so `proposal objective` reports `proposal_objective_score` rather than the semantic-only objective.
- Corrects objective provenance for ESEM-guided final solutions: objective metadata now records PFA as a contributing component when objective-mode PFA actually entered the semantic/PFA proposal score. Additive proposal/final score-schema labels clarify canonical final reranking without changing scoring.
- Changes the default proxy reference-N RMSEA power contrast from close-fit `.05` versus misfit `.08` to close-fit `.05` versus misfit `.06`. This affects only automatic semantic-proxy reference-N calculation; ESEM fit cutoffs and other psychometric thresholds are unchanged.
- Simplifies redundant research-context qualifiers in summary, quality, and documentation labels. Function names, evidence boundaries, and calculations are unchanged.
- No ACO search mathematics, archive logic, PFA/ESEM formulas, embedding transformations, content thresholds, or selection criteria are changed in this patch.

# SEMANTICA 0.3.1

## Integrity fixes after 0.3.0 check

- Preserves names in `semantica_embedding_spec(task_map=...)`, restoring the intended analysis-intent to provider-task mapping (including Nomic `psychometric_similarity -> clustering`) without hard-coding psychometric thresholds.
- Restores backward-compatible embedding provenance labels (`source = "model_card"` and `source = "user"`) while retaining the richer `capability_source` metadata introduced in 0.3.0.
- Updates the ESEM admissibility regression to reflect evidence-stratified finalization: every unique finalist may receive a canonical full-ESEM refit, while inadmissible finalists remain ineligible to win.
- Removes spurious patience-alias warnings from the new invariance test without weakening the user-facing warning when both legacy `max.iter` and `search_patience` are genuinely supplied.
- Documents `embedding_spec` for `semantica_pipeline()` and excludes accidental top-level `result.rds` analysis objects from source builds.

# SEMANTICA 0.2.12

## Saturated selection-context regression correction

- Corrects the remaining 0.2.11 saturation regression assumption: when a synthetic candidate pool and its selected subset share the same uniform within-factor (.75) and between-factor (.25) similarities, the within-minus-between gap is correctly unchanged at .50, just as stochastic-superiority discrimination is correctly unchanged at its ceiling A = 1.
- The regression now explicitly verifies `pool_gap = selected_gap = 0.50` and `gap_gain = 0` instead of incorrectly requiring a strict post-selection increase.
- This preserves the intended method contract of `semantica_selection_context()`: post-selection diagnostics are contextual/descriptive and may improve, worsen, or remain unchanged; the function does not assume optimization must improve every diagnostic.
- No production R code, ACO/PFA/ESEM mathematics, evidence thresholds, method defaults, backend behavior, or reporting logic are changed in 0.2.12.

# SEMANTICA 0.2.11

## Regression-fixture correction

- Corrects one 0.2.10 selection-context regression fixture whose candidate pool already had perfect stochastic-superiority discrimination (`A = 1`), making a strict post-selection improvement mathematically impossible.
- The intended non-saturated fixture now contains genuinely weak within-factor links below the between-factor similarity level, so selection must improve both stochastic-superiority discrimination and the within-minus-between gap.
- Adds an explicit saturation regression confirming that a perfectly discriminating candidate pool may legitimately remain at `A = 1` after selection while pool and selected evidence remain separately represented.
- No production R code, ACO/PFA/ESEM mathematics, evidence thresholds, method defaults, backend behavior, or reporting logic are changed in 0.2.11.

# SEMANTICA 0.2.10

## Evidence-context and local-diagnostic hardening

- Adds `semantica_selection_context()` so candidate-pool and optimizer-selected semantic discrimination are retained together. Selected-set stochastic-superiority and gap values are explicitly labeled post-selection descriptive evidence rather than selection-adjusted inference.
- Adds `semantica_factor_semantic_diagnostics()` to expose per-factor within-versus-between semantic separation without introducing a universal cutoff. Compact summaries surface the descriptively weakest selected factor so one collapsed dimension cannot be hidden as easily by a favorable aggregate.
- Makes ACO `best_objective` context explicit through `objective_context`: the value is labeled an optimization utility, records its evidence regime (`semantic_only`, `pfa_semantic_guided`, `esem_guided`, or explicit fallback), and is not presented as a universal cross-run scale-quality score.
- Makes multi-seed objective dispersion regime-aware through additive `objective_comparability` metadata; mixed fallback/ESEM-guided utilities remain numerically available for backward compatibility but are explicitly marked non-comparable as one quality scale.
- Corrects final-selection metadata when ESEM search guidance is deliberately disabled: semantically/PFA-guided reranking is no longer mislabeled as `esem_guided`. This is a metadata correction only; selected solutions and objective calculations are unchanged.
- Promotes chance-adjusted PFA partition agreement (ARI) in reports while retaining factor-presence recovery as a separate coverage-style diagnostic. No PFA scoring formula is changed.
- Adds factor-level summaries of existing ESEM item diagnostics and makes `admissible_but_structurally_mixed` visibly distinct from comparatively favorable structure. Technical admissibility is never equated with clean intended structure.
- Contextualizes HTMT-like semantic overlap explicitly as a sample-free proxy when mixed ESEM structure is reported; it is not labeled participant-based discriminant validity.
- Updates summary plots and reports to use `optimization utility` / `proposal utility` terminology instead of visually implying a universal quality score.
- Adds regression tests for selection-context diagnostics, local factor weakness, PFA partition-state reporting, objective evidence regimes, and mixed-ESEM anti-misinterpretation messages.
- Updates evidence-interpretation and workflow documentation with the methodological rationale and literature references for stochastic superiority, post-selection interpretation, chance-adjusted partition agreement, ESEM/local misspecification diagnosis, and HTMT-style overlap diagnostics.
- **No ACO weights, PFA mathematics, ESEM fitting/scoring formulas, method thresholds, backend routing, embedding calculations, or production defaults are changed in 0.2.10.**

# SEMANTICA 0.2.9

## Software integrity hardening

- Removes incidental R RNG consumption from persistent embedding-cache writes and ESEM path-diagram plotting; both operations are now caller-RNG-neutral.
- Adds small base-R validation/condition helpers and validates documented configuration domains before normalization, including strict logical flags and positive/integer controls. Existing resource aliases (`"serial"`, `"none"`, `"off"`) and documented `Inf` plotting/ESEM limits remain supported.
- Rejects fractional, zero, or negative `ants`, `max.iter`, and `esem_every` values instead of silently truncating/clamping them; the legacy `cfa_every` alias follows the same contract.
- Makes unknown named configuration fields observable through a compatibility warning while preserving 0.2.x merge behavior and precedence.
- Refines participant internal-structure evidence statuses using already-recorded convergence/admissibility metadata without upgrading semantic proxies to participant-based validity evidence.
- Adds sanitized, canonical resolved-configuration provenance and a stable consistency hash to the simplified full-pipeline reproducibility record; model provenance now distinguishes requested/resolved names and the strength of available revision identity.
- Adds a small R-native classed-condition hierarchy at high-value boundaries and retains the documented 0.2.x unknown-backend fallback with a specific forward-compatibility warning.
- Makes optional polarity-diagnostic failure state explicit through adjacent status metadata while preserving the existing diagnostic value fields.
- Strengthens legacy CSV reload checks for square/numeric/finite/symmetric cosine matrices, cross-file item-ID consistency, complete factor assignments, and feasible selection targets.
- Clarifies bundle MD5 semantics as accidental-corruption consistency checking, adds checksum-purpose metadata, and preserves verification of older manifests.
- Extracts a deterministic preregistration-manifest builder and standardizes side-effect/reproducibility documentation for high-level effectful functions.
- Adds focused regression tests for RNG isolation, configuration contracts, evidence states, provenance sanitization, backend fallback, reload/bundle integrity, and reviewed direct RNG use.
- Retains the packaging fix that normalizes source-tree modification times to a safe past timestamp before archive creation.

# SEMANTICA 0.2.8

## Regression-test isolation

- Fixed two resource-control regression tests that still compared 0.2.7's new
  `nonredundant_median` default against a frozen pre-0.2.7 objective baseline.
- The tests now explicitly set `within_target_method = "legacy_q40"`, because
  their purpose is to test resource ceilings and PFA scheduling, not the newer
  target estimator.
- No runtime, statistical, embedding, ACO, PFA, ESEM, or user-facing default
  behavior changed from 0.2.7. The new `nonredundant_median` default remains the
  recommended production behavior.

# SEMANTICA 0.2.7

## Methodological hardening and configuration-first help

- Replaces the default 0.25--0.55-clamped within-factor cohesion target with a model/pool-relative `nonredundant_median` target. User-supplied targets are respected; `legacy_q40` remains available explicitly for reproducibility studies.
- Updates the experimental `adaptive_pool` calibration so its cohesion targets also follow the current pool geometry rather than a universal `.70` cap.
- Makes content-definition alignment diagnostic by default. Optional `content_alignment_mode = "guard"` now excludes only pool-relative clear factor mismatches or explicit exclusion conflicts when enough alternatives remain; small rank differences and facet ambiguity stay diagnostic rather than becoming automatic deletions.
- Uses factor-specific `forbidden`/exclusion concepts contrastively during content-alignment screening.
- Separates generated counts, post-quality-guard counts, cohesion-eligible counts, and selected targets in Phase-0 reporting and returned audit metadata.
- Changes `semantic_fallback` from permanent ESEM shutdown after one failed checkpoint to checkpoint-local fallback: later distinct candidates continue to receive search-time ESEM attempts.
- Gives every archived finalist one full-ESEM opportunity before a semantic/PFA fallback can determine the final solution, and reuses those archive fits to avoid duplicate finalization work.
- Preserves full ESEM rejection/admissibility reasons when a failed archive fit is reused for transparent final diagnostics.
- Skips response-validation-N planning by default when the selected semantic-proxy ESEM is structurally inadmissible; `validation_planning_on_inadmissible = "run"` retains the legacy sensitivity calculation when deliberately requested.
- Adds finite-pool random-overlap references for raw-vs-mean-centered top-pair sensitivity. Agreement at/below that reference produces an explicit representation-sensitivity warning without automatically changing cosine preprocessing.
- Keeps pair-sampling stability and representation-preprocessing stability conceptually separate in summaries.
- Redesigns the package help around the recommended workflow: construct definition -> configuration builders -> `semantica_full_pipeline()` -> interpretation. Lower-level generation, embedding, optimizer, plotting, and research APIs remain documented and exported but are clearly labeled advanced/component interfaces rather than mandatory steps.
- Adds `_pkgdown.yml` reference groups so website users see the main workflow/configuration functions first and advanced/research helpers separately.
- Rewrites the main workflow vignette and expands configuration documentation with accepted values, precedence rules, evidence boundaries, and safe defaults.
- Adds 0.2.7 regression tests covering model-relative cohesion targets, conservative alignment decisions, repeated ESEM checkpoints after fallback, one-refit-per-archive-finalist behavior, adaptive-pool target calibration, validation-N skip policy, and rank-relative representation sensitivity.

# SEMANTICA 0.2.6

## Documentation check cleanup

- Removed the duplicate roxygen declaration of the `preflight` argument in `semantica_pipeline()`. The argument is now documented exactly once, resolving the `R CMD check` warning about duplicated `\argument` entries.
- No runtime, statistical, semantic, optimizer, provider, or analysis behavior changed from 0.2.5.

# SEMANTICA 0.2.5

## Check-clean embedding-policy contract

- Keeps `.semantica_prepare_embedding_texts()` return values as plain character vectors. Embedding-policy provenance remains explicitly recorded in session/result diagnostics and cache identities rather than being attached as a surprising vector attribute. This resolves the three 0.2.4 integrity-test failures without changing prepared text, model routing, task-prefix normalization, or cache semantics.
- Adds durable wrapped `\usage{}` documentation for the longest public APIs so CRAN-style checks do not truncate their PDF-manual signatures.
- No method defaults or public argument semantics changed relative to 0.2.4.

# SEMANTICA 0.2.4

## Representation and content-integrity hardening

- Added purpose-aware backend sessions so embedding-only preflight no longer checks irrelevant chat models.
- Added model-aware embedding task instructions. `nomic-embed-text` automatically uses its documented `clustering:` task prefix for psychometric item similarity unless the analyst overrides the policy. Cache keys now include embedding-task/instruction provenance.
- Added item-to-factor and item-to-facet definition alignment using the configured embedding model. Full-pipeline selection uses a feasibility-aware rank-consistency guard by default: misaligned items are excluded only when enough aligned alternatives remain.
- Facet coverage now distinguishes metadata-label coverage from semantic definition coverage when alignment evidence is available.
- Added an optional feasibility-aware polarity selection guard; polarity remains diagnostic by default because overt negation is not equivalent to reverse-keying.
- Facet-misaligned items no longer earn facet-coverage credit merely from their metadata label when semantic facet diagnostics are available.
- Strengthened generation prompts to require direct construct/facet instantiation and discourage paraphrases, adjacent-construct drift, causes, consequences, and outcomes.
- Removed the universal `mean(cosine) > 0.35` anisotropy flag. SEMANTICA now reports continuous common-direction concentration and none-vs-mean-center sensitivity without automatically changing the representation.
- Alignment and embedding-policy evidence are stored in pipeline results for reproducibility.

# SEMANTICA 0.2.3

* Fixes embedding batching so component/dimension names survive fresh inference and persistent-cache round trips; the cache schema is bumped to invalidate older unnamed-vector entries.
* Updates ESEM admissibility regression tests to the current full-refit contract: heuristic DFI does not perform a bootstrap ESEM, and both search and archive fits request solver diagnostics while using different fallback policies.
* Keeps item dimnames in empirical-calibration and representation-ensemble matrices and corrects tests to compare unnamed diagonals rather than discarding analytically useful matrix labels.
* Completes roxygen parameter documentation for the 0.2.x configuration, generation, plotting, sensitivity, adaptive-threshold, construct-blueprint, and polarity controls.
* Removes non-ASCII literals from executable R code by using Unicode escapes, improving source portability on CRAN-style checks.
* Uses `stats::predict()` explicitly in empirical calibration, eliminating the visible-global-function NOTE without adding an unnecessary namespace import.
* Excludes recovery notes and RStudio project files from source-package builds.
* Makes optional torch capability probes quieter and failure-tolerant when an installed torch package lacks its Lantern runtime; torch remains optional and CPU computation remains the default.
* Adds an embedding-cache regression test and documents internal ESEM admissibility helpers more completely.
* No method defaults were changed relative to 0.2.2.

# SEMANTICA 0.2.2

* Fixes vignette build failure in `evidence-interpretation.Rmd` by explicitly attaching `SEMANTICA` in the vignette setup chunk. Package vignettes are rendered in a clean R session during `R CMD build`.
* Fixes two roxygen Markdown warnings where `[0,1]` / `[0, 1]` were interpreted as documentation links.
* Hands the 49 seeded `.Rd` pages back to roxygen2 ownership so `devtools::document()` can regenerate and synchronize them with their `R/*.R` source blocks instead of skipping them.
* No method defaults were changed relative to 0.2.1.

# SEMANTICA 0.2.1

* Fixes the generated `NAMESPACE` shipped in the first revised 0.2.0 archive. The malformed namespace caused `devtools::document()` / package loading to fail at line 2 on the `:=` import.
* Restores a valid roxygen2-style namespace with all 79 exported objects, 8 S3 registrations, and package imports.
* No method defaults were changed relative to the revised 0.2.0 implementation.

# SEMANTICA 0.2.0

* Adds explicit semantic-proxy evidence labeling and richer result summaries.
* Adds backend preflight/capability checks, persistent embedding caching, HTTP retry support, and batched Ollama embeddings.
* Adds reproducibility bundles, matrix-repair diagnostics, construct-coverage and polarity diagnostics.
* Adds experimental empirical calibration, robustness/ensemble, signed-semantic, cross-language, construct-graph, pilot-updating, benchmarking, and preregistration utilities.
* Adds evidence-interpretation documentation and expanded tests.

# SEMANTICA 0.1.0

* Initial release.
