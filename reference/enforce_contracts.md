# Get or set whether Daf contracts are enforced

Controls the `DAF_ENFORCE_CONTRACTS` global in DataAxesFormats. When
enforced, `@computation` contracts are verified (useful in development,
expensive in production pipelines).

## Usage

``` r
enforce_contracts(enforce = NULL)
```

## Arguments

- enforce:

  If NULL (default), return the current value. Otherwise a single
  logical to set.

## Value

With `enforce = NULL`, the current logical value (visibly). When
setting, the previous value (invisibly).
