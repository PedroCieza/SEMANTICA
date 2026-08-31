list(
  manifest = list(
    captured = "2026-08-20",
    package_version = "0.1.0",
    r_version = "4.5.3",
    dependencies = c(
      lavaan = "0.7-2",
      GPArotation = "2026.8-1",
      parallelly = "1.48.0",
      Matrix = "1.7-4",
      testthat = "3.3.2"
    ),
    seeds = c(semantic = 101L, pfa = 202L),
    numeric_tolerance = 1e-8,
    note = paste(
      "Cloud-free regression summaries use manually supplied embeddings.",
      "No serialized lavaan fit object is retained."
    )
  ),
  inputs = list(
    item_ids = paste0("item_", 1:8),
    factor = rep(c("F1", "F2"), each = 4L),
    cosine = structure(
      c(
        1, 0.998022042380788, 0.996142853892887, 0.992349321251677,
        0.159618849037468, 0.201020153903043, 0.233132570746111,
        0.266469149706419, 0.998022042380788, 1,
        0.999671927227796, 0.998110339876408, 0.220846083806396,
        0.261771456564575, 0.293212584911664, 0.326189190416177,
        0.996142853892887, 0.999671927227796, 1, 0.999352738221009,
        0.243774709180197, 0.284543897186989, 0.315556660176488,
        0.348576076214144, 0.992349321251677, 0.998110339876408,
        0.999352738221009, 1, 0.277136436770766, 0.317585604222608,
        0.348033659578563, 0.380932076811889, 0.159618849037468,
        0.220846083806396, 0.243774709180197, 0.277136436770766, 1,
        0.999061682668262, 0.997139521833084, 0.993782670515257,
        0.201020153903043, 0.261771456564575, 0.284543897186989,
        0.317585604222608, 0.999061682668262, 1, 0.999256714598213,
        0.997667624317797, 0.233132570746111, 0.293212584911664,
        0.315556660176488, 0.348033659578563, 0.997139521833084,
        0.999256714598213, 1, 0.998912808207484, 0.266469149706419,
        0.326189190416177, 0.348576076214144, 0.380932076811889,
        0.993782670515257, 0.997667624317797, 0.998912808207484, 1
      ),
      dim = c(8L, 8L),
      dimnames = rep(list(paste0("item_", 1:8)), 2L)
    )
  ),
  semantic = list(
    candidate = c(1L, 0L, 1L, 1L, 1L, 1L, 0L, 1L),
    best_items = c(
      "item_1", "item_3", "item_4", "item_5", "item_6", "item_8"
    ),
    semantic_objective = 0.000569312934264357,
    duplicate_cluster_id = c(
      "dup1", "dup1", "dup1", "dup1",
      "dup2", "dup2", "dup2", "dup2"
    ),
    duplicate_clusters = list(
      dup1 = c("item_1", "item_2", "item_3", "item_4"),
      dup2 = c("item_5", "item_6", "item_7", "item_8")
    ),
    heuristic_cutoffs = list(
      cfi = 0.96, tli = 0.94, rmsea = 0.09, srmr = 0.08
    ),
    termination_reason = "max_total_iter_reached",
    iterations = 2L,
    esem_search_jobs = 0L,
    required_result_fields = c(
      "best_items", "best_objective", "duplicate_clusters",
      "elite_archive", "termination_reason", "total_iterations",
      "evaluation_telemetry", "performance", "resource_plan",
      "reproducibility", "esem_alignment", "esem_admissibility"
    )
  ),
  pfa = list(
    best_items = c(
      "item_1", "item_2", "item_4", "item_5", "item_7", "item_8"
    ),
    score = 1,
    objective_score = 1,
    best_objective = 0.300401076468202,
    search_iterations = 1L,
    search_attempts = 2L,
    search_successes = 2L,
    loadings = structure(
      c(
        -0.0628394001042142, 0.00206845709071085,
        0.0626458319489528, 1.01566205690106,
        0.995680947975378, 0.984489201802927,
        1.01553013704175, 0.999383022060938,
        0.980646343020892, -0.0631736390077361,
        0.0147379648744456, 0.0509697217499824
      ),
      dim = c(6L, 2L),
      dimnames = list(
        c("item_1", "item_2", "item_4", "item_5", "item_7", "item_8"),
        c("PFA1", "PFA2")
      )
    )
  )
)
