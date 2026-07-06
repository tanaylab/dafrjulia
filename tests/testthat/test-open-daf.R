test_that("open_daf dispatches by suffix and round-trips", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")

    # description() prints "type: <Backend>", so it verifies the path actually
    # dispatched to the intended backend, not merely that a round-trip is
    # self-consistent through whatever backend was picked.
    zp <- file.path(tempdir(), "d.daf.zarr")
    unlink(zp, recursive = TRUE, force = TRUE)
    zw <- open_daf(zp, "w")
    expect_match(description(zw), "type: ZarrDaf")
    set_scalar(zw, "x", 1L)
    expect_equal(get_scalar(open_daf(zp, "r"), "x"), 1)

    zip <- file.path(tempdir(), "d.daf.zip")
    unlink(zip, force = TRUE)
    zipw <- open_daf(zip, "w")
    expect_match(description(zipw), "type: ZipDaf")
    set_scalar(zipw, "x", 2L)
    expect_equal(get_scalar(open_daf(zip, "r"), "x"), 2)

    h5 <- file.path(tempdir(), "d.h5df")
    unlink(h5, force = TRUE)
    h5w <- open_daf(h5, "w")
    expect_match(description(h5w), "type: H5df")
    set_scalar(h5w, "x", 3L)
    expect_equal(get_scalar(open_daf(h5, "r"), "x"), 3)

    fdir <- file.path(tempdir(), "d.files")
    unlink(fdir, recursive = TRUE, force = TRUE)
    fw <- open_daf(fdir, "w")
    expect_match(description(fw), "type: FilesDaf")
    set_scalar(fw, "x", 4L)
    expect_equal(get_scalar(open_daf(fdir, "r"), "x"), 4)
})

test_that("open_daf 'w' truncates an existing files daf", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    fdir <- file.path(tempdir(), "trunc.files")
    unlink(fdir, recursive = TRUE, force = TRUE)
    set_scalar(open_daf(fdir, "w"), "x", 1L)
    d2 <- open_daf(fdir, "w")
    expect_false(has_scalar(d2, "x"))
})

test_that("open_daf refuses write mode over http", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_error(open_daf("http://example.com/x", "r+"))
})
