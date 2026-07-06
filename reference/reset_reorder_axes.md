# Roll back an interrupted axis reorder

If a previous `reorder_axes` call was interrupted, restore the Daf(s) to
their pre-reorder state. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/reorder.html)
for details.

## Usage

``` r
reset_reorder_axes(daf)
```

## Arguments

- daf:

  A writable Daf object, or a list of writable Daf objects.

## Value

TRUE if a stale reorder was rolled back, FALSE if there was nothing to
do.
