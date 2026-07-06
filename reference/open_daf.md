# Open a Daf repository based on path

Dispatches to the appropriate backend based on `path`, mirroring Julia's
`open_daf`: `.daf.zarr`/`.daf.zarr.zip` open a Zarr Daf; `.daf.zip`
opens a ZIP Daf; `http(s)://` opens a read-only HTTP Daf; `.h5df` opens
an HDF5 Daf; otherwise a native files Daf.

## Usage

``` r
open_daf(path, mode = "r", name = NULL, packed = FALSE)
```

## Arguments

- path:

  Path (or URL) to the Daf repository

- mode:

  Mode to open the storage ("r", "r+", "w", or "w+"; HTTP is "r" only)

- name:

  Optional name for the Daf object

- packed:

  If TRUE, store arrays chunked and compressed

## Value

A Daf object using the backend selected by `path`

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/complete.html)
for details.
