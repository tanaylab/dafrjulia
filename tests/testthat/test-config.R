test_that("enforce_contracts gets and sets", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    original <- enforce_contracts()
    on.exit(enforce_contracts(original), add = TRUE)

    previous <- enforce_contracts(TRUE)
    expect_equal(previous, original)
    expect_true(enforce_contracts())

    enforce_contracts(FALSE)
    expect_false(enforce_contracts())
})

test_that("daf_packed_options gets and sets", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    original <- daf_packed_options()
    on.exit(daf_packed_options(target_chunk_kb = original$target_chunk_kb), add = TRUE)

    expect_true(is.list(original))
    expect_true(is.character(original$compression))

    daf_packed_options(target_chunk_kb = 16L)
    expect_equal(daf_packed_options()$target_chunk_kb, 16)
})
