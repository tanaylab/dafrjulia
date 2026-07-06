test_that("reorder_axes permutes an axis and its vector together", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "r!")
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "score", c(10, 20, 30))

    before <- setNames(
        as.numeric(get_vector(daf, "cell", "score")),
        as.character(axis_vector(daf, "cell"))
    )
    reorder_axes(daf, list(cell = c(3L, 1L, 2L)))
    after <- setNames(
        as.numeric(get_vector(daf, "cell", "score")),
        as.character(axis_vector(daf, "cell"))
    )

    # Entry->value pairing preserved (direction-agnostic), order changed.
    expect_equal(after[names(before)], before)
    expect_false(identical(names(after), names(before)))
})

test_that("reorder_axes rejects a non-named permutation list", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "r2!")
    add_axis(daf, "cell", c("A", "B"))
    expect_error(reorder_axes(daf, list(c(2L, 1L))), "named list")
})

test_that("reset_reorder_axes returns FALSE on a clean daf", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "rr!")
    add_axis(daf, "cell", c("A", "B"))
    expect_false(reset_reorder_axes(daf))
})
