test_that("handler constants are exported correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_equal(IGNORE_HANDLER, "IgnoreHandler")
    expect_equal(WARN_HANDLER, "WarnHandler")
    expect_equal(ERROR_HANDLER, "ErrorHandler")
})

test_that("get_julia_handler validates input", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Valid handlers should not error
    expect_no_error(dafJuliaWrapper:::get_julia_handler(IGNORE_HANDLER))
    expect_no_error(dafJuliaWrapper:::get_julia_handler(WARN_HANDLER))
    expect_no_error(dafJuliaWrapper:::get_julia_handler(ERROR_HANDLER))

    # Invalid handler should error with clear message
    expect_error(
        dafJuliaWrapper:::get_julia_handler("InvalidHandler"),
        "Handler must be one of:"
    )
})

test_that("inefficient_action_handler sets and returns handlers", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Save the current handler to restore later
    original_handler <- inefficient_action_handler(IGNORE_HANDLER)

    # Test setting and getting each handler type
    expect_equal(inefficient_action_handler(WARN_HANDLER), IGNORE_HANDLER)
    expect_equal(inefficient_action_handler(ERROR_HANDLER), WARN_HANDLER)
    expect_equal(inefficient_action_handler(IGNORE_HANDLER), ERROR_HANDLER)

    # Restore the original handler
    inefficient_action_handler(original_handler)
})

test_that("inefficient_action_handler validates input", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_error(
        inefficient_action_handler("InvalidHandler"),
        "Handler must be one of:"
    )
})
