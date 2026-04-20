# Test empty_cache function
test_that("empty_cache works", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    test_matrix <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3)
    set_matrix(daf, "cell", "gene", "UMIs", test_matrix)

    # Get matrix to populate cache
    get_matrix(daf, "cell", "gene", "UMIs")

    # Clear cache
    empty_cache(daf)

    # Should still work after cache is cleared
    result_matrix <- get_matrix(daf, "cell", "gene", "UMIs")
    expect_equal(result_matrix, test_matrix, ignore_attr = TRUE)

    # Test selective cache clearing
    empty_cache(daf, clear = "MappedData")
    empty_cache(daf, keep = "MemoryData")

    # Should still work after cache is cleared
    result_matrix <- get_matrix(daf, "cell", "gene", "UMIs")
    expect_equal(result_matrix, test_matrix, ignore_attr = TRUE)
})

# Test R-side caching with version-counter invalidation
test_that("get_vector returns cached result on second call", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "score", c(1.0, 2.0, 3.0))

    result1 <- get_vector(daf, "cell", "score")
    result2 <- get_vector(daf, "cell", "score")

    # Both results should be identical
    expect_identical(result1, result2)
})

test_that("get_vector cache is invalidated after set_vector with overwrite", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "score", c(1.0, 2.0, 3.0))

    result1 <- get_vector(daf, "cell", "score")
    expect_equal(as.numeric(result1), c(1.0, 2.0, 3.0))

    # Overwrite with new data
    set_vector(daf, "cell", "score", c(10.0, 20.0, 30.0), overwrite = TRUE)

    result2 <- get_vector(daf, "cell", "score")
    expect_equal(as.numeric(result2), c(10.0, 20.0, 30.0))
})

test_that("empty_cache clears the R-side cache", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "score", c(1.0, 2.0, 3.0))

    # Populate cache
    get_vector(daf, "cell", "score")
    expect_gt(length(ls(daf$cache_env)), 0)

    # Clear cache
    empty_cache(daf)
    expect_equal(length(ls(daf$cache_env)), 0)

    # Data should still be accessible
    result <- get_vector(daf, "cell", "score")
    expect_equal(as.numeric(result), c(1.0, 2.0, 3.0))
})

test_that("empty_cache clears R-side cache even with selective clear/keep", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "score", c(1.0, 2.0, 3.0))

    get_vector(daf, "cell", "score")
    expect_gt(length(ls(daf$cache_env)), 0)
    empty_cache(daf, clear = "QueryData")
    expect_equal(length(ls(daf$cache_env)), 0)

    get_vector(daf, "cell", "score")
    expect_gt(length(ls(daf$cache_env)), 0)
    empty_cache(daf, keep = "MemoryData")
    expect_equal(length(ls(daf$cache_env)), 0)
})

test_that("empty_cache rejects invalid clear/keep values", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    expect_error(empty_cache(daf, clear = "Nonsense"))
    expect_error(empty_cache(daf, keep = "AlsoWrong"))
})

test_that("two Daf wrappers of distinct Julia objects get isolated caches", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf1 <- memory_daf()
    daf2 <- memory_daf()
    add_axis(daf1, "cell", c("A", "B"))
    add_axis(daf2, "cell", c("X", "Y", "Z"))

    axis_vector(daf1, "cell")
    expect_gt(length(ls(daf1$cache_env)), 0)
    expect_equal(length(ls(daf2$cache_env)), 0)

    empty_cache(daf1)
    expect_equal(length(ls(daf1$cache_env)), 0)
})

test_that("S3 copy of a Daf shares the cache env", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "score", c(1.0, 2.0, 3.0))

    daf_copy <- daf
    get_vector(daf_copy, "cell", "score")
    # both see the same entries
    expect_equal(ls(daf$cache_env), ls(daf_copy$cache_env))
})

test_that("get_matrix caching works", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    test_matrix <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3)
    set_matrix(daf, "cell", "gene", "UMIs", test_matrix)

    result1 <- get_matrix(daf, "cell", "gene", "UMIs")
    result2 <- get_matrix(daf, "cell", "gene", "UMIs")

    # Both results should be identical (cache hit)
    expect_identical(result1, result2)
    expect_equal(result1, test_matrix, ignore_attr = TRUE)
})

test_that("get_matrix cache is invalidated after set_matrix with overwrite", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    test_matrix1 <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3)
    set_matrix(daf, "cell", "gene", "UMIs", test_matrix1)

    result1 <- get_matrix(daf, "cell", "gene", "UMIs")
    expect_equal(result1, test_matrix1, ignore_attr = TRUE)

    # Overwrite with new data
    test_matrix2 <- matrix(c(10, 40, 20, 50, 30, 60), nrow = 2, ncol = 3)
    set_matrix(daf, "cell", "gene", "UMIs", test_matrix2, overwrite = TRUE)

    result2 <- get_matrix(daf, "cell", "gene", "UMIs")
    expect_equal(result2, test_matrix2, ignore_attr = TRUE)
})

test_that("axis_vector caching works", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B", "C"))

    result1 <- axis_vector(daf, "cell")
    result2 <- axis_vector(daf, "cell")

    # Both results should be identical (cache hit)
    expect_identical(result1, result2)
    expect_equal(result1, c("A", "B", "C"))
})

test_that("axis_vector cache is invalidated after axis overwrite", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B", "C"))

    result1 <- axis_vector(daf, "cell")
    expect_equal(result1, c("A", "B", "C"))

    # Overwrite axis with new entries
    add_axis(daf, "cell", c("D", "E"), overwrite = TRUE)

    result2 <- axis_vector(daf, "cell")
    expect_equal(result2, c("D", "E"))
})

test_that("get_vector with default skips cache", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "cell", c("A", "B", "C"))

    # default = NA path should not use cache
    result <- get_vector(daf, "cell", "nonexistent", default = NA)
    expect_true(all(is.na(result)))
    expect_equal(length(result), 3)
})
