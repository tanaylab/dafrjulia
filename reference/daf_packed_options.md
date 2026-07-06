# Get or set the packing (chunk/compression) options

Reads and, for any non-NULL argument, sets the `DAF_PACKED_*` /
`DAF_HTTP_MAX_COALESCE_GAP_KB` globals in DataAxesFormats. These control
how `packed = TRUE` storage is chunked, compressed, and cached.

## Usage

``` r
daf_packed_options(
  compression = NULL,
  compression_level = NULL,
  target_chunk_kb = NULL,
  local_cache_kb = NULL,
  http_cache_kb = NULL,
  http_max_coalesce_gap_kb = NULL
)
```

## Arguments

- compression:

  Compression codec name, e.g. "blosc_zstd_bitshuffle" or "gzip_shuffle"
  (a Julia Symbol).

- compression_level:

  Integer compression level.

- target_chunk_kb:

  Target chunk size in binary KB.

- local_cache_kb:

  Local decoded-chunk cache size in binary KB.

- http_cache_kb:

  HTTP decoded-chunk cache size in binary KB.

- http_max_coalesce_gap_kb:

  Max gap (binary KB) to coalesce HTTP range reads.

## Value

The values in effect *before* this call, as a named list (visibly when
all arguments are NULL, invisibly when setting).
