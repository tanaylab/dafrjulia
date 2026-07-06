test_that("zarr_daf round-trips scalar, axis, vector", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    path <- file.path(tempdir(), "z.daf.zarr")
    unlink(path, recursive = TRUE, force = TRUE)

    daf <- zarr_daf(path, "w", name = "z!")
    set_scalar(daf, "answer", 42L)
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "score", c(1.0, 2.0, 3.0))

    daf2 <- zarr_daf(path, "r")
    expect_equal(get_scalar(daf2, "answer"), 42)
    expect_setequal(as.character(axis_vector(daf2, "cell")), c("A", "B", "C"))
    expect_equal(as.numeric(get_vector(daf2, "cell", "score")), c(1, 2, 3))
})

test_that("zip_daf round-trips scalar, axis, vector", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    path <- file.path(tempdir(), "z.daf.zip")
    unlink(path, force = TRUE)

    daf <- zip_daf(path, "w", name = "z!")
    set_scalar(daf, "answer", 7L)
    add_axis(daf, "cell", c("A", "B"))
    set_vector(daf, "cell", "score", c(5.0, 6.0))

    daf2 <- zip_daf(path, "r")
    expect_equal(get_scalar(daf2, "answer"), 7)
    expect_equal(as.numeric(get_vector(daf2, "cell", "score")), c(5, 6))
})
