# AnnData-like live facade for a Daf object

AnnData-like live facade for a Daf object

AnnData-like live facade for a Daf object

## Value

An R6 object of class `DafAnnData`.

## Details

Wraps a Daf object and provides AnnData-compatible accessors (`$X`,
`$obs`, `$var`, `$layers`, `$uns`). This is a read-only facade: reads
are lazy and go through to the underlying Daf object on demand,
benefiting from dafrjulia's R-side caching.

The facade maps AnnData concepts to Daf data:

- `$X` - the primary matrix (`obs_axis` x `var_axis`, named `x_name`)

- `$obs` - data frame of observation (`obs_axis`) vectors

- `$var` - data frame of variable (`var_axis`) vectors

- `$layers` - named list of additional matrices (excluding `$X`)

- `$uns` - named list of scalars

- `$obs_names` - character vector of observation names

- `$var_names` - character vector of variable names

- `$n_obs` - number of observations

- `$n_vars` - number of variables

- `$shape` - `c(n_obs, n_vars)`

## Public fields

- `daf`:

  The underlying Daf object

- `obs_axis`:

  Name of the observations axis

- `var_axis`:

  Name of the variables axis

- `x_name`:

  Name of the primary matrix

## Active bindings

- `X`:

  The primary matrix (obs x var)

- `obs`:

  Data frame of observation vectors

- `var`:

  Data frame of variable vectors

- `layers`:

  Named list of additional matrices (excluding X)

- `uns`:

  Named list of scalars

- `obs_names`:

  Character vector of observation names

- `var_names`:

  Character vector of variable names

- `n_obs`:

  Number of observations

- `n_vars`:

  Number of variables

- `shape`:

  Dimensions c(n_obs, n_vars)

## Methods

### Public methods

- [`DafAnnData$new()`](#method-DafAnnData-new)

- [`DafAnnData$print()`](#method-DafAnnData-print)

- [`DafAnnData$summary()`](#method-DafAnnData-summary)

- [`DafAnnData$clone()`](#method-DafAnnData-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new DafAnnData facade.

#### Usage

    DafAnnData$new(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs")

#### Arguments

- `daf`:

  A Daf object

- `obs_axis`:

  Observations axis name. If `NULL`, auto-detects `"cell"` or
  `"metacell"`.

- `var_axis`:

  Variables axis name. If `NULL`, auto-detects `"gene"`.

- `x_name`:

  Primary matrix name. If `NULL`, the first matrix available on
  `(obs_axis, var_axis)` is used; defaults to `"UMIs"` when present.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print a concise summary. Prints dimensions and axis names only; call
[`summary()`](https://rdrr.io/r/base/summary.html) for the full listing
of obs/var/layer names.

#### Usage

    DafAnnData$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Full listing of obs vectors, var vectors, and layer names. Each access
is a bridge call; kept out of
[`print()`](https://rdrr.io/r/base/print.html) to keep auto-print cheap.

#### Usage

    DafAnnData$summary(object, ...)

#### Arguments

- `object`:

  Present for S3 generic compatibility.

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DafAnnData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
