library(dafJuliaWrapper)
library(jlview)

setup_daf(confirm_install = TRUE)

cat("=== Latency Benchmark: jlview vs copy ===\n\n")

JuliaCall::julia_command("using DataAxesFormats, NamedArrays")
JuliaCall::julia_command('
daf = MemoryDaf(name = "bench")
add_axis!(daf, "cell", ["cell_$(lpad(i, 5, \'0\'))" for i in 1:10000])
add_axis!(daf, "gene", ["gene_$(lpad(i, 4, \'0\'))" for i in 1:1000])
set_matrix!(daf, "cell", "gene", "UMIs", rand(10000, 1000))
set_vector!(daf, "cell", "score", rand(10000))
')
daf_obj <- Daf(JuliaCall::julia_eval("daf", need_return = "Julia"))

# Warm up
mat <- get_matrix(daf_obj, "cell", "gene", "UMIs")
vec <- get_vector(daf_obj, "cell", "score")
rm(mat, vec)
gc()

# Benchmark get_matrix (10K x 1K)
n <- 5
times_mat <- numeric(n)
for (i in 1:n) {
    t <- system.time({
        mat <- get_matrix(daf_obj, "cell", "gene", "UMIs")
    })
    times_mat[i] <- t["elapsed"]
    rm(mat)
    gc()
}
cat("get_matrix (10Kx1K named, jlview):\n")
cat("  mean:", round(mean(times_mat) * 1000), "ms\n")
cat("  median:", round(median(times_mat) * 1000), "ms\n")
cat("  is_jlview:", jlview::is_jlview(get_matrix(daf_obj, "cell", "gene", "UMIs")), "\n\n")

# Benchmark get_vector (10K named)
times_vec <- numeric(n)
for (i in 1:n) {
    t <- system.time({
        vec <- get_vector(daf_obj, "cell", "score")
    })
    times_vec[i] <- t["elapsed"]
    rm(vec)
    gc()
}
cat("get_vector (10K named, jlview):\n")
cat("  mean:", round(mean(times_vec) * 1000), "ms\n")
cat("  median:", round(median(times_vec) * 1000), "ms\n\n")

# Copy baseline for comparison (manual collect, no jlview)
times_copy <- numeric(n)
for (i in 1:n) {
    t <- system.time({
        jl_mat <- JuliaCall::julia_call("DataAxesFormats.get_matrix",
            daf_obj$jl_obj, "cell", "gene", "UMIs",
            need_return = "Julia"
        )
        jl_stripped <- JuliaCall::julia_call("_strip_wrappers", jl_mat, need_return = "Julia")
        mat_copy <- JuliaCall::julia_call("collect", jl_stripped, need_return = "R")
    })
    times_copy[i] <- t["elapsed"]
    rm(jl_mat, jl_stripped, mat_copy)
    gc()
}
cat("copy baseline (10Kx1K, collect):\n")
cat("  mean:", round(mean(times_copy) * 1000), "ms\n")
cat("  median:", round(median(times_copy) * 1000), "ms\n\n")

speedup <- mean(times_copy) / mean(times_mat)
cat("=== Matrix speedup:", round(speedup, 1), "x ===\n")
