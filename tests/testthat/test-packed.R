test_that("packed files daf round-trips a chunk-sized vector", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    dir <- file.path(tempdir(), "packed_test.files")
    unlink(dir, recursive = TRUE, force = TRUE)

    daf <- files_daf(dir, "w", name = "p!", packed = TRUE)
    add_axis(daf, "cell", paste0("c", seq_len(10000)))
    vals <- as.numeric(seq_len(10000))
    set_vector(daf, "cell", "score", vals)

    daf2 <- files_daf(dir, "r")
    expect_equal(as.numeric(get_vector(daf2, "cell", "score")), vals)
})
