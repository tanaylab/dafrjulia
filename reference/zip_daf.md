# Create a Daf object with ZIP-archive storage

Stores data in a single append-only `.daf.zip` archive. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/zip_files.html)
for details.

## Usage

``` r
zip_daf(path, mode = "r", name = NULL, packed = FALSE)
```

## Arguments

- path:

  Path to the ZIP archive (`.daf.zip` or `.dafs.zip#/<group>`)

- mode:

  Mode to open the storage ("r", "r+", "w", or "w+")

- name:

  Optional name for the Daf object

- packed:

  If TRUE, store arrays chunked and compressed

## Value

A Daf object with ZIP-archive storage
