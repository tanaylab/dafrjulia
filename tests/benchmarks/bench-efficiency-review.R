#!/usr/bin/env Rscript
# =============================================================================
# dafr Efficiency Bottleneck Micro-Benchmarks
# =============================================================================
#
# Usage:
#   Rscript bench-efficiency-review.R
#
# Prerequisites:
#   - dafr, jlview, JuliaCall installed
#   - Julia environment with DataAxesFormats.jl available
#
# This benchmark covers 7 bottleneck areas:
#   1. Cold-start / setup_daf() sub-phases
#   2. Bridge round-trips (from_julia_array for various types & sizes)
#   3. Version counter overhead (cache hit vs miss in get_vector)
#   4. Cache infrastructure (get_daf_id objectid+string cost)
#   5. Matrix transfer sizes (dense & sparse at realistic dimensions)
#   6. Query execution (simple vs complex, cached vs uncached)
#   7. Type conversion (Bool collect vs numeric zero-copy)
# =============================================================================

# -- Utilities ----------------------------------------------------------------

#' Run a benchmark `n` times, returning elapsed times in seconds
bench_times <- function(expr_fn, n = 5, warmup = 1) {
    # Warmup runs (not recorded)
    for (i in seq_len(warmup)) {
        tryCatch(expr_fn(), error = function(e) NULL)
    }
    times <- numeric(n)
    for (i in seq_len(n)) {
        gc(verbose = FALSE)
        t <- system.time(expr_fn())
        times[i] <- t["elapsed"]
    }
    times
}

#' Summarise timing vector into a one-row data.frame
summarise_bench <- function(name, times_sec) {
    ms <- times_sec * 1000
    data.frame(
        benchmark = name,
        median_ms = round(median(ms), 2),
        min_ms    = round(min(ms), 2),
        max_ms    = round(max(ms), 2),
        n         = length(ms),
        stringsAsFactors = FALSE
    )
}

#' Safely run a benchmark block; returns NULL on error
safe_bench <- function(name, expr_fn, n = 5, warmup = 1) {
    tryCatch({
        times <- bench_times(expr_fn, n = n, warmup = warmup)
        summarise_bench(name, times)
    }, error = function(e) {
        message(sprintf("[SKIP] %s -- %s", name, conditionMessage(e)))
        data.frame(
            benchmark = name,
            median_ms = NA_real_,
            min_ms    = NA_real_,
            max_ms    = NA_real_,
            n         = 0L,
            stringsAsFactors = FALSE
        )
    })
}

results <- list()
reps <- 5

# =============================================================================
# 0. SETUP  (not timed; required for all subsequent benchmarks)
# =============================================================================
cat("=== dafr Efficiency Bottleneck Benchmarks ===\n\n")

cat("[0] Loading dafr and initialising Julia...\n")
library(dafr)
library(jlview)
library(JuliaCall)
library(Matrix)

# -- Benchmark 1: Cold-start sub-phases --------------------------------------
# We time the *remaining* setup_daf phases (JuliaCall::julia_setup already done
# when library(dafr) was loaded, so we time it once separately then load pkgs).

cat("\n[1] Cold-start / setup_daf sub-phases\n")

# Phase 1a: julia_setup (JuliaCall init)
# NOTE: julia_setup can only truly be timed on the very first call.
# Subsequent calls are nearly free. We time it once.
t_julia_setup <- system.time({
    JuliaCall::julia_setup(installJulia = FALSE)
})
results[[length(results) + 1]] <- summarise_bench(
    "cold_start:julia_setup (single run)", c(t_julia_setup["elapsed"])
)

# Phase 1b: Pkg.activate
t_pkg_activate <- system.time({
    JuliaCall::julia_library("Pkg")
    dafr:::use_default_julia_environment("default")
})
results[[length(results) + 1]] <- summarise_bench(
    "cold_start:pkg_activate (single run)", c(t_pkg_activate["elapsed"])
)

# Phase 1c: using packages (import Julia modules)
t_using <- system.time({
    pkgs_needed <- list("TanayLabUtilities", "DataAxesFormats", "Logging")
    for (pkg in pkgs_needed) JuliaCall::julia_library(pkg)
})
results[[length(results) + 1]] <- summarise_bench(
    "cold_start:using_packages (single run)", c(t_using["elapsed"])
)

# Phase 1d: import_julia_packages + define_julia_functions + type cache
t_define <- system.time({
    dafr:::import_julia_packages()
    dafr:::define_julia_functions()
    dafr:::init_julia_type_cache()
})
results[[length(results) + 1]] <- summarise_bench(
    "cold_start:define_helpers (single run)", c(t_define["elapsed"])
)

# Phase 1e: setup_logger
t_logger <- system.time({
    dafr:::setup_logger(level = "Warn", show_time = TRUE, show_module = TRUE, show_location = FALSE)
})
results[[length(results) + 1]] <- summarise_bench(
    "cold_start:setup_logger (single run)", c(t_logger["elapsed"])
)

# Total cold-start
total_cold <- t_julia_setup["elapsed"] + t_pkg_activate["elapsed"] +
    t_using["elapsed"] + t_define["elapsed"] + t_logger["elapsed"]
results[[length(results) + 1]] <- summarise_bench(
    "cold_start:total (single run)", c(total_cold)
)

# =============================================================================
# Prepare test data for remaining benchmarks
# =============================================================================
cat("\n[SETUP] Creating test Daf objects...\n")

# Small Daf for vector benchmarks
daf_small <- memory_daf(name = "bench_small")
n_cells_small <- 10000L
n_genes_small <- 1000L
cell_names_s <- sprintf("cell_%05d", seq_len(n_cells_small))
gene_names_s <- sprintf("gene_%04d", seq_len(n_genes_small))
add_axis(daf_small, "cell", cell_names_s)
add_axis(daf_small, "gene", gene_names_s)

set.seed(42)
set_vector(daf_small, "cell", "score", runif(n_cells_small))
set_vector(daf_small, "cell", "label", sample(c("typeA", "typeB", "typeC"), n_cells_small, replace = TRUE))
set_vector(daf_small, "cell", "flag", as.logical(sample(0:1, n_cells_small, replace = TRUE)))

# Dense Float64 matrix (10K x 1K)
mat_dense <- matrix(runif(n_cells_small * n_genes_small),
    nrow = n_cells_small, ncol = n_genes_small
)
set_matrix(daf_small, "cell", "gene", "UMIs", mat_dense)
rm(mat_dense)

# Sparse matrix (10K x 1K, ~10% density)
sp_density <- 0.10
nnz <- as.integer(n_cells_small * n_genes_small * sp_density)
sp_i <- sample.int(n_cells_small, nnz, replace = TRUE)
sp_j <- sample.int(n_genes_small, nnz, replace = TRUE)
sp_x <- runif(nnz)
sp_mat <- sparseMatrix(i = sp_i, j = sp_j, x = sp_x,
    dims = c(n_cells_small, n_genes_small),
    repr = "C"
)
set_matrix(daf_small, "cell", "gene", "weights", sp_mat)
rm(sp_i, sp_j, sp_x, sp_mat)

cat("  10K cells x 1K genes Daf ready.\n")

# Larger Daf for matrix-transfer size benchmarks (conditionally)
create_large <- TRUE
tryCatch({
    daf_large <- memory_daf(name = "bench_large")
    n_cells_large <- 10000L
    n_genes_large <- 2000L
    cell_names_l <- sprintf("cell_%05d", seq_len(n_cells_large))
    gene_names_l <- sprintf("gene_%04d", seq_len(n_genes_large))
    add_axis(daf_large, "cell", cell_names_l)
    add_axis(daf_large, "gene", gene_names_l)

    mat_large <- matrix(runif(n_cells_large * n_genes_large),
        nrow = n_cells_large, ncol = n_genes_large
    )
    set_matrix(daf_large, "cell", "gene", "UMIs", mat_large)
    rm(mat_large)

    # Sparse large (10K x 2K, 5% density)
    nnz2 <- as.integer(n_cells_large * n_genes_large * 0.05)
    sp_i2 <- sample.int(n_cells_large, nnz2, replace = TRUE)
    sp_j2 <- sample.int(n_genes_large, nnz2, replace = TRUE)
    sp_x2 <- runif(nnz2)
    sp_mat2 <- sparseMatrix(i = sp_i2, j = sp_j2, x = sp_x2,
        dims = c(n_cells_large, n_genes_large),
        repr = "C"
    )
    set_matrix(daf_large, "cell", "gene", "weights", sp_mat2)
    rm(sp_i2, sp_j2, sp_x2, sp_mat2)
    cat("  10K cells x 2K genes large Daf ready.\n")
}, error = function(e) {
    create_large <<- FALSE
    message("  [SKIP] Large Daf creation failed: ", conditionMessage(e))
})

gc(verbose = FALSE)

# =============================================================================
# 2. BRIDGE ROUND-TRIPS  (from_julia_array)
# =============================================================================
cat("\n[2] Bridge round-trips: from_julia_array\n")

# 2a. Dense Float64 vector (10K)
results[[length(results) + 1]] <- safe_bench(
    "bridge:dense_float64_vec_10K",
    function() {
        jl_vec <- JuliaCall::julia_call("DataAxesFormats.get_vector",
            daf_small$jl_obj, "cell", "score",
            need_return = "Julia"
        )
        r_vec <- dafr:::from_julia_array(jl_vec)
    },
    n = reps
)

# 2b. Dense Float64 matrix (10K x 1K)
results[[length(results) + 1]] <- safe_bench(
    "bridge:dense_float64_mat_10Kx1K",
    function() {
        jl_mat <- JuliaCall::julia_call("DataAxesFormats.get_matrix",
            daf_small$jl_obj, "cell", "gene", "UMIs",
            need_return = "Julia"
        )
        r_mat <- dafr:::from_julia_array(jl_mat)
    },
    n = reps
)

# 2c. Sparse Float64 matrix (10K x 1K, ~10% density)
results[[length(results) + 1]] <- safe_bench(
    "bridge:sparse_float64_mat_10Kx1K",
    function() {
        jl_sp <- JuliaCall::julia_call("DataAxesFormats.get_matrix",
            daf_small$jl_obj, "cell", "gene", "weights",
            need_return = "Julia"
        )
        r_sp <- dafr:::from_julia_array(jl_sp)
    },
    n = reps
)

# 2d. Bool vector (10K) -- requires collect, no zero-copy
results[[length(results) + 1]] <- safe_bench(
    "bridge:bool_vec_10K",
    function() {
        jl_bool <- JuliaCall::julia_call("DataAxesFormats.get_vector",
            daf_small$jl_obj, "cell", "flag",
            need_return = "Julia"
        )
        r_bool <- dafr:::from_julia_array(jl_bool)
    },
    n = reps
)

# 2e. String vector (10K) -- requires collect, no zero-copy
results[[length(results) + 1]] <- safe_bench(
    "bridge:string_vec_10K",
    function() {
        jl_str <- JuliaCall::julia_call("DataAxesFormats.get_vector",
            daf_small$jl_obj, "cell", "label",
            need_return = "Julia"
        )
        r_str <- dafr:::from_julia_array(jl_str)
    },
    n = reps
)

# =============================================================================
# 3. VERSION COUNTER OVERHEAD
# =============================================================================
cat("\n[3] Version counter overhead: cache hit vs miss\n")

# 3a. get_vector cache miss (first call, R-cache cold)
# Clear R-side cache first
empty_cache(daf_small)

results[[length(results) + 1]] <- safe_bench(
    "version_counter:get_vector_cache_miss",
    function() {
        # Clear R cache before each iteration to force miss
        obj_id <- dafr:::get_daf_id(daf_small)
        if (exists(obj_id, envir = dafr:::.daf_cache_registry)) {
            cache <- get(obj_id, envir = dafr:::.daf_cache_registry)
            rm(list = ls(cache), envir = cache)
        }
        v <- get_vector(daf_small, "cell", "score")
    },
    n = reps,
    warmup = 0
)

# 3b. get_vector cache hit (second+ call, R-cache warm)
# Ensure cache is warm
invisible(get_vector(daf_small, "cell", "score"))

results[[length(results) + 1]] <- safe_bench(
    "version_counter:get_vector_cache_hit",
    function() {
        v <- get_vector(daf_small, "cell", "score")
    },
    n = reps
)

# 3c. Bare version-counter fetch cost (isolate the JuliaCall overhead)
results[[length(results) + 1]] <- safe_bench(
    "version_counter:fetch_only",
    function() {
        vc <- JuliaCall::julia_call("string",
            JuliaCall::julia_call("DataAxesFormats.vector_version_counter",
                daf_small$jl_obj, "cell", "score",
                need_return = "Julia"
            ),
            need_return = "R"
        )
    },
    n = reps
)

# =============================================================================
# 4. CACHE INFRASTRUCTURE
# =============================================================================
cat("\n[4] Cache infrastructure: get_daf_id overhead\n")

# 4a. get_daf_id (objectid + string conversion)
results[[length(results) + 1]] <- safe_bench(
    "cache:get_daf_id",
    function() {
        id <- dafr:::get_daf_id(daf_small)
    },
    n = reps
)

# 4b. cache_lookup (after get_daf_id)
vc_for_bench <- JuliaCall::julia_call("string",
    JuliaCall::julia_call("DataAxesFormats.vector_version_counter",
        daf_small$jl_obj, "cell", "score",
        need_return = "Julia"
    ),
    need_return = "R"
)
results[[length(results) + 1]] <- safe_bench(
    "cache:cache_lookup",
    function() {
        val <- dafr:::cache_lookup(daf_small, "vec:cell:score", vc_for_bench)
    },
    n = reps
)

# 4c. Full cache round-trip: get_daf_cache + lookup
results[[length(results) + 1]] <- safe_bench(
    "cache:full_lookup_roundtrip",
    function() {
        cache <- dafr:::get_daf_cache(daf_small)
        full_key <- paste0(vc_for_bench, ":vec:cell:score")
        val <- if (exists(full_key, envir = cache)) get(full_key, envir = cache) else NULL
    },
    n = reps
)

# =============================================================================
# 5. MATRIX TRANSFER SIZES
# =============================================================================
cat("\n[5] Matrix transfer sizes\n")

# 5a. Dense 10K x 1K
results[[length(results) + 1]] <- safe_bench(
    "matrix_xfer:dense_10Kx1K",
    function() {
        m <- get_matrix(daf_small, "cell", "gene", "UMIs")
    },
    n = reps
)

# 5b. Sparse 10K x 1K
results[[length(results) + 1]] <- safe_bench(
    "matrix_xfer:sparse_10Kx1K",
    function() {
        m <- get_matrix(daf_small, "cell", "gene", "weights")
    },
    n = reps
)

if (create_large) {
    # 5c. Dense 10K x 2K
    results[[length(results) + 1]] <- safe_bench(
        "matrix_xfer:dense_10Kx2K",
        function() {
            m <- get_matrix(daf_large, "cell", "gene", "UMIs")
        },
        n = reps
    )

    # 5d. Sparse 10K x 2K
    results[[length(results) + 1]] <- safe_bench(
        "matrix_xfer:sparse_10Kx2K",
        function() {
            m <- get_matrix(daf_large, "cell", "gene", "weights")
        },
        n = reps
    )
}

# =============================================================================
# 6. QUERY EXECUTION
# =============================================================================
cat("\n[6] Query execution: simple vs complex, cached vs uncached\n")

# 6a. Simple query: Axis("cell") |> LookupVector("score")
simple_q <- Axis("cell") |> LookupVector("score")

results[[length(results) + 1]] <- safe_bench(
    "query:simple_vector_cached",
    function() {
        v <- get_query(daf_small, simple_q, cache = TRUE)
    },
    n = reps
)

results[[length(results) + 1]] <- safe_bench(
    "query:simple_vector_uncached",
    function() {
        v <- get_query(daf_small, simple_q, cache = FALSE)
    },
    n = reps
)

# 6b. Matrix lookup query: Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs")
mat_q <- Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs")

results[[length(results) + 1]] <- safe_bench(
    "query:matrix_lookup_cached",
    function() {
        m <- get_query(daf_small, mat_q, cache = TRUE)
    },
    n = reps
)

# 6c. Complex query with mask: filtered vector
# Axis("cell") |> IsGreater on score > 0.5 |> LookupVector("score")
complex_q <- Axis("cell") |> LookupVector("score") |> IsGreater(0.5)

results[[length(results) + 1]] <- safe_bench(
    "query:complex_masked_cached",
    function() {
        v <- get_query(daf_small, complex_q, cache = TRUE)
    },
    n = reps
)

results[[length(results) + 1]] <- safe_bench(
    "query:complex_masked_uncached",
    function() {
        v <- get_query(daf_small, complex_q, cache = FALSE)
    },
    n = reps
)

# 6d. parse_query cost
results[[length(results) + 1]] <- safe_bench(
    "query:parse_query_string",
    function() {
        q <- parse_query("/ cell : score")
    },
    n = reps
)

# =============================================================================
# 7. TYPE CONVERSION  (Bool collect vs numeric zero-copy)
# =============================================================================
cat("\n[7] Type conversion: Bool collect vs Float64 zero-copy\n")

# Pre-fetch Julia objects for isolated comparison
jl_float_vec <- JuliaCall::julia_call("DataAxesFormats.get_vector",
    daf_small$jl_obj, "cell", "score",
    need_return = "Julia"
)
jl_bool_vec <- JuliaCall::julia_call("DataAxesFormats.get_vector",
    daf_small$jl_obj, "cell", "flag",
    need_return = "Julia"
)

# 7a. Float64 zero-copy path (jlview)
results[[length(results) + 1]] <- safe_bench(
    "typeconv:float64_zerocopy_10K",
    function() {
        r <- dafr:::from_julia_array(jl_float_vec)
    },
    n = reps
)

# 7b. Bool collect path (must copy)
results[[length(results) + 1]] <- safe_bench(
    "typeconv:bool_collect_10K",
    function() {
        r <- dafr:::from_julia_array(jl_bool_vec)
    },
    n = reps
)

# 7c. String collect path (must copy)
jl_str_vec <- JuliaCall::julia_call("DataAxesFormats.get_vector",
    daf_small$jl_obj, "cell", "label",
    need_return = "Julia"
)
results[[length(results) + 1]] <- safe_bench(
    "typeconv:string_collect_10K",
    function() {
        r <- dafr:::from_julia_array(jl_str_vec)
    },
    n = reps
)

# 7d. Ratio: Bool-to-Float64 overhead (derived)
# We compute this from already-captured results; no timing needed.

# =============================================================================
# RESULTS TABLE
# =============================================================================
cat("\n")
cat("=============================================================================\n")
cat("                         BENCHMARK RESULTS\n")
cat("=============================================================================\n\n")

results_df <- do.call(rbind, results)
rownames(results_df) <- NULL

# Pretty-print with fixed-width columns
fmt <- "%-48s %10s %10s %10s %5s\n"
cat(sprintf(fmt, "benchmark", "median_ms", "min_ms", "max_ms", "n"))
cat(paste(rep("-", 88), collapse = ""), "\n")
for (i in seq_len(nrow(results_df))) {
    r <- results_df[i, ]
    cat(sprintf(fmt,
        r$benchmark,
        if (is.na(r$median_ms)) "SKIP" else sprintf("%.2f", r$median_ms),
        if (is.na(r$min_ms))    "SKIP" else sprintf("%.2f", r$min_ms),
        if (is.na(r$max_ms))    "SKIP" else sprintf("%.2f", r$max_ms),
        as.character(r$n)
    ))
}

cat("\n")

# Derived metrics
float_row <- results_df[results_df$benchmark == "typeconv:float64_zerocopy_10K", ]
bool_row  <- results_df[results_df$benchmark == "typeconv:bool_collect_10K", ]
if (nrow(float_row) == 1 && nrow(bool_row) == 1 &&
    !is.na(float_row$median_ms) && !is.na(bool_row$median_ms) &&
    float_row$median_ms > 0) {
    cat(sprintf("Bool/Float64 overhead ratio: %.1fx\n",
        bool_row$median_ms / float_row$median_ms))
}

hit_row  <- results_df[results_df$benchmark == "version_counter:get_vector_cache_hit", ]
miss_row <- results_df[results_df$benchmark == "version_counter:get_vector_cache_miss", ]
if (nrow(hit_row) == 1 && nrow(miss_row) == 1 &&
    !is.na(hit_row$median_ms) && !is.na(miss_row$median_ms) &&
    hit_row$median_ms > 0) {
    cat(sprintf("Cache hit/miss speedup: %.1fx\n",
        miss_row$median_ms / hit_row$median_ms))
}

vc_row <- results_df[results_df$benchmark == "version_counter:fetch_only", ]
if (nrow(vc_row) == 1 && !is.na(vc_row$median_ms)) {
    cat(sprintf("Version counter bare fetch cost: %.2f ms\n", vc_row$median_ms))
}

id_row <- results_df[results_df$benchmark == "cache:get_daf_id", ]
if (nrow(id_row) == 1 && !is.na(id_row$median_ms)) {
    cat(sprintf("get_daf_id overhead: %.2f ms\n", id_row$median_ms))
}

cat("\n=== Done ===\n")
