## Test environments

- Local Linux (AlmaLinux 8.10), R 4.4.1

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Julia dependency notes

`dafr` provides an R interface to the Julia package 'DataAxesFormats.jl' via
`JuliaCall` (already on CRAN). Julia (>= 1.10) and several Julia packages are
declared in `SystemRequirements` with a download URL, following CRAN policy.

**No Julia at install or load time.** Julia is only initialized when users
explicitly call `setup_daf()`. The package installs, loads, and passes all
checks without a Julia runtime present.

**Examples.** All examples use `\dontrun{}` since they require a Julia runtime.

**Tests.** The test suite uses a `tryCatch`/`JULIA_AVAILABLE` guard in
`tests/testthat/setup.R`:

```r
tryCatch(
    { setup_daf(pkg_check = FALSE); JULIA_AVAILABLE <- TRUE },
    error = function(e) { JULIA_AVAILABLE <<- FALSE }
)
```

Every Julia-dependent test is wrapped with
`skip_if(!JULIA_AVAILABLE, "Julia not available")` so that all 283 Julia tests
are gracefully skipped when Julia is absent. Pure-R tests (contract object
creation, documentation generation) run unconditionally. On CRAN, the check
therefore covers package load, documentation, and R-only logic without requiring
an external runtime or network access.

**Vignette.** The vignette uses `eval = FALSE` for all code chunks.

**Julia package installation.** To avoid unintended writes in non-interactive
environments, Julia package installation requires explicit user confirmation
via the `confirm_install` parameter or is skipped entirely with
`pkg_check = FALSE`.

**CRAN precedent.** The `jlview` package on CRAN uses the same architecture:
it interfaces with Julia via `JuliaCall` and uses the identical
`tryCatch`/`JULIA_AVAILABLE` guard pattern in its test setup.
