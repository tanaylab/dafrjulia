test_that("http_daf rejects a non-http path without touching the network", {
    expect_error(http_daf("/tmp/not-a-url"), "must be an http")
})

test_that("http_daf live read", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    skip_on_cran()
    skip_if_offline()
    skip("No served Daf HTTP fixture available in CI")
    # Manual smoke test: point at a served .daf directory root.
    # daf <- http_daf("http://localhost:8000/example")
    # expect_true(has_axis(daf, "cell"))
})
