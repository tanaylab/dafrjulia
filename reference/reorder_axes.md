# Reorder the entries of one or more axes

Permute the entries of the named axes in a writable leaf Daf (or a set
of leaf Dafs sharing those axes), rewriting all data that depends on
them. Not valid for chain or view Dafs. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/reorder.html)
for details.

## Usage

``` r
reorder_axes(daf, permutations)
```

## Arguments

- daf:

  A writable Daf object, or a list of writable Daf objects.

- permutations:

  A named list mapping each axis name to a 1-based integer permutation
  vector of that axis's entries.

## Value

NULL, invisibly. Called for its side effect.
