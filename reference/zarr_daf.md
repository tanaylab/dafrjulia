# Create a Daf object with Zarr-based storage

Stores data using the Zarr format (a directory, a `.daf.zarr.zip`
archive, or a remote `http(s)://` Zarr). See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/zarr_format.html)
for details.

## Usage

``` r
zarr_daf(path, mode = "r", name = NULL, packed = FALSE)
```

## Arguments

- path:

  Path to the Zarr storage (`.daf.zarr`, `.daf.zarr.zip`,
  `.dafs.zarr.zip#/<group>`, or an `http(s)://` URL)

- mode:

  Mode to open the storage ("r", "r+", "w", or "w+")

- name:

  Optional name for the Daf object

- packed:

  If TRUE, store arrays chunked and compressed

## Value

A Daf object with Zarr-based storage
