# Create a Daf object with HDF5-based storage

This function creates a Daf object that stores data in an HDF5 disk
file. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/h5df_format.html)
for details.

## Usage

``` r
h5df(root, mode = "r", name = NULL, packed = FALSE)
```

## Arguments

- root:

  Path to the HDF5 file, or a Julia HDF5 File or Group object

- mode:

  Mode to open the storage ("r" for read-only, "r+" for read-write)

- name:

  Optional name for the Daf object

- packed:

  If TRUE, store arrays chunked and compressed (DataAxesFormats
  v0.3.0+).

## Value

A Daf object with HDF5-based storage

## Examples

``` r
if (FALSE) { # \dontrun{
setup_daf()
h5_path <- file.path(tempdir(), "example.h5")
daf <- h5df(h5_path, "w", name = "example")
} # }
```
