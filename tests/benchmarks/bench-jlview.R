library(dafr)
library(jlview)
library(JuliaCall)

setup_daf(pkg_check = FALSE)

cat("=== Phase 5a Benchmark: Zero-Copy with Named Matrices ===\n\n")

# Create test data: 10K x 1K named Float64 matrix using dafr's R API
daf <- memory_daf(name = "bench")
cell_names <- sprintf("cell_%05d", 1:10000)
gene_names <- sprintf("gene_%04d", 1:1000)
add_axis(daf, "cell", cell_names)
add_axis(daf, "gene", gene_names)

# Create and set a random matrix
set.seed(42)
mat_data <- matrix(runif(10000 * 1000), nrow = 10000, ncol = 1000)
set_matrix(daf, "cell", "gene", "UMIs", mat_data)
rm(mat_data)
gc()

cat("Created 10000 x 1000 Float64 matrix (~76.3 MB raw)\n\n")

# Test 1: Zero-copy path (jlview with atomic names)
gc()
gc()
mem_before <- gc(reset = TRUE)
mat <- get_matrix(daf, "cell", "gene", "UMIs")
mem_after <- gc()

is_altrep <- jlview::is_jlview(mat)
heap_mb <- (mem_after[2, 2] - mem_before[2, 2])

cat("Zero-copy path:\n")
cat("  is_jlview (ALTREP):", is_altrep, "\n")
cat("  R heap delta:", round(heap_mb, 1), "MB\n")
cat("  dim:", dim(mat), "\n")
cat("  has dimnames:", !is.null(dimnames(mat)), "\n")
if (!is.null(dimnames(mat))) {
    cat("  rownames[1:3]:", head(rownames(mat), 3), "\n")
    cat("  colnames[1:3]:", head(colnames(mat), 3), "\n")
}
cat("\n")

# Test 2: Copy path for comparison
rm(mat)
gc()
gc()
mem_before2 <- gc(reset = TRUE)
jl_mat <- julia_call("DataAxesFormats.get_matrix", daf$jl_obj, "cell", "gene", "UMIs", need_return = "Julia")
jl_unwrapped <- julia_call("_strip_wrappers", jl_mat, need_return = "Julia")
mat_copy <- julia_call("collect", jl_unwrapped, need_return = "R")
mem_after2 <- gc()
heap_mb2 <- (mem_after2[2, 2] - mem_before2[2, 2])

cat("Copy path:\n")
cat("  R heap delta:", round(heap_mb2, 1), "MB\n")
cat("  dim:", dim(mat_copy), "\n")
cat("\n")

cat("=== SUMMARY ===\n")
cat("Zero-copy heap:", round(heap_mb, 1), "MB\n")
cat("Copy heap:", round(heap_mb2, 1), "MB\n")
savings <- if (heap_mb2 > 0) round((1 - heap_mb / heap_mb2) * 100, 1) else 0
cat("Memory savings:", savings, "%\n")
cat("ALTREP preserved:", is_altrep, "\n")
