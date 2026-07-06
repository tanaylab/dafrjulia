# Create a Daf object with file-based storage

This function creates a Daf object that stores data in disk files. See
the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/files_format.html)
for details.

## Usage

``` r
files_daf(path, mode = "r", name = NULL, packed = FALSE)
```

## Arguments

- path:

  Path to the files storage location

- mode:

  Mode to open the storage ("r" for read-only, "r+" for read-write)

- name:

  Optional name for the Daf object

- packed:

  If TRUE, store arrays chunked and compressed (DataAxesFormats
  v0.3.0+).

## Value

A Daf object with file-based storage

## Examples

``` r
if (FALSE) { # \dontrun{
setup_daf()
daf <- files_daf(tempdir(), "w", name = "example")
add_axis(daf, "gene", c("X", "Y", "Z"))
} # }
```
