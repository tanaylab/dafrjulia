# Create a read-only Daf object served over HTTP(S)

Opens a Daf data set served over `http://` or `https://`. Read-only;
there is no `mode` argument. See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/http_format.html)
for details.

## Usage

``` r
http_daf(url, name = NULL, packed = FALSE)
```

## Arguments

- url:

  The `http(s)://` URL of the served Daf data set

- name:

  Optional name for the Daf object

- packed:

  Ignored for HTTP (kept for signature symmetry with other backends)

## Value

A read-only Daf object
