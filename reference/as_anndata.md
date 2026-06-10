# Create an AnnData-like facade for a Daf object

Creates a live, read-only facade over a Daf object that provides
AnnData-compatible accessors (`$X`, `$obs`, `$var`, `$layers`, `$uns`).
No data is copied at construction; reads go through to the underlying
Daf object on demand, served by dafrjulia's R-side cache on subsequent
calls.

## Usage

``` r
as_anndata(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs")
```

## Arguments

- daf:

  A Daf object

- obs_axis:

  Name of the observations axis. Auto-detected if NULL (tries "cell",
  then "metacell").

- var_axis:

  Name of the variables axis. Auto-detected if NULL (tries "gene").

- x_name:

  Name of the primary matrix property. If NULL, the first matrix on
  (obs_axis, var_axis) is used, preferring `"UMIs"`. Defaults to
  `"UMIs"`.

## Value

A
[`DafAnnData`](https://tanaylab.github.io/dafrjulia/reference/DafAnnData.md)
R6 object

## See also

[`DafAnnData`](https://tanaylab.github.io/dafrjulia/reference/DafAnnData.md)

## Examples

``` r
if (FALSE) { # \dontrun{
daf <- example_cells_daf()
adata <- as_anndata(daf)
adata$X # primary matrix
adata$obs # observation metadata
adata$var # variable metadata
adata$obs_names # observation names
adata$n_obs # number of observations
} # }
```
