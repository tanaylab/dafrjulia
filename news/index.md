# Changelog

## dafrjulia (development version)

### New Features (DataAxesFormats 0.3.0 catch-up)

- New storage backends:
  [`zarr_daf()`](https://tanaylab.github.io/dafrjulia/reference/zarr_daf.md),
  [`zip_daf()`](https://tanaylab.github.io/dafrjulia/reference/zip_daf.md),
  and read-only
  [`http_daf()`](https://tanaylab.github.io/dafrjulia/reference/http_daf.md).
- `packed = TRUE` for chunked+compressed storage on
  [`files_daf()`](https://tanaylab.github.io/dafrjulia/reference/files_daf.md),
  [`h5df()`](https://tanaylab.github.io/dafrjulia/reference/h5df.md),
  [`zarr_daf()`](https://tanaylab.github.io/dafrjulia/reference/zarr_daf.md),
  [`zip_daf()`](https://tanaylab.github.io/dafrjulia/reference/zip_daf.md),
  [`open_daf()`](https://tanaylab.github.io/dafrjulia/reference/open_daf.md),
  and
  [`complete_daf()`](https://tanaylab.github.io/dafrjulia/reference/complete_daf.md).
- [`open_daf()`](https://tanaylab.github.io/dafrjulia/reference/open_daf.md)
  now delegates to the Julia dispatch, routing `.daf.zarr`, `.daf.zip`,
  `http(s)://`, and `.h5df` paths to the correct backend (it previously
  opened `.daf.zarr`/`.daf.zip` as plain files repositories).
- Format conversion:
  [`files_to_zarr()`](https://tanaylab.github.io/dafrjulia/reference/files_to_zarr.md)
  and
  [`zarr_to_files()`](https://tanaylab.github.io/dafrjulia/reference/zarr_to_files.md).
- Axis reordering:
  [`reorder_axes()`](https://tanaylab.github.io/dafrjulia/reference/reorder_axes.md)
  and
  [`reset_reorder_axes()`](https://tanaylab.github.io/dafrjulia/reference/reset_reorder_axes.md).
- Configuration:
  [`daf_packed_options()`](https://tanaylab.github.io/dafrjulia/reference/daf_packed_options.md)
  and
  [`enforce_contracts()`](https://tanaylab.github.io/dafrjulia/reference/enforce_contracts.md).

### Bug Fixes

- Reading a vector or matrix from packed/chunked/disk-backed storage
  (Zarr/Zip/Http or `packed = TRUE`) no longer segfaults the R process:
  the zero-copy `jlview` path is now restricted to genuine in-memory
  arrays, and lazy arrays are materialized via a copying fallback.

## dafrjulia 0.1.1

**Tracks DataAxesFormats.jl 0.3.0.** DataAxesFormats 0.3.0 requires
Julia 1.12 and changed the empty-buffer builder protocol in
`writers.jl`, which broke the wrapper’s `filled_empty_sparse_vector` /
`filled_empty_sparse_matrix` (they hit `MethodError`s on the new
signatures). Fixed:

- `get_empty_sparse_vector` / `get_empty_sparse_matrix` capture the
  `cache_group` the builder now returns and hold it until the paired
  `filled_*` call. `filled_empty_sparse_vector` /
  `filled_empty_sparse_matrix` thread that `cache_group` to the Julia
  finalizer and release the data write lock the builder opened (the old
  `_b` binding helpers a non-Julia wrapper used to rely on were removed
  in 0.3.0).
- `get_empty_dense_vector` / `get_empty_dense_matrix` unwrap the buffer
  from the new `(buffer, cache_group)` return tuple and release the
  write lock (dense buffers have no separate finalizer step).

The public R API (function names, arguments, return shapes) is
unchanged. Full test suite passes against DataAxesFormats 0.3.0 on Julia
1.12.

## dafrjulia 0.1.0

**First CRAN release. Renamed from `dafr` — the `dafr` name has been
freed for a native-R reimplementation in a separate repository. This
package is the JuliaCall-based wrapper around DataAxesFormats.jl and has
been renamed accordingly.**

### Migration from `dafr`

- [`library(dafr)`](https://rdrr.io/r/base/library.html) becomes
  [`library(dafrjulia)`](https://tanaylab.github.io/dafrjulia/).
- `options(dafr.JULIA_HOME = ...)` becomes
  `options(dafrjulia.JULIA_HOME = ...)`; same for
  `dafr.julia_environment` and other `dafr.*` options.
- Public API (function names, arguments, return shapes) is unchanged.

### New Features

#### AnnData-like facade

- `as_anndata(daf)` returns a `DafAnnData` R6 object exposing `$X`,
  `$obs`, `$var`, `$layers`, `$uns`, `$obs_names`, `$var_names`,
  `$n_obs`, `$n_vars`, and `$shape`. Read-only and live: accesses go
  through to the underlying Daf on demand, served by the R-side cache on
  hits. `obs_axis`/`var_axis` auto-detect `cell`/`metacell` and `gene`;
  `x_name` is validated at construction.

#### R-side caching

- `get_vector`, `get_matrix`, and `axis_vector` now cache results on the
  R side, keyed by Daf-level version counters so that modifications via
  `set_vector`/`set_matrix`/`add_axis` invalidate entries automatically.
- The cache env is stored directly on each `Daf` object, so there is no
  global registry and no leak. Two R-level copies of the same `Daf` list
  share the cache (reference semantics); two wrappers of the same Julia
  object get separate caches.
- `empty_cache(daf, clear = ..., keep = ...)` now always purges the
  R-side cache (regardless of selectors) and validates its arguments
  with `match.arg`.

#### Contracts

- New `CreatedOutput` expectation type, matching DataAxesFormats.jl
  v0.2.0. Like `GuaranteedOutput`, a `CreatedOutput` item is expected to
  be produced by the computation and warns if a value is already
  present; unlike `OptionalOutput`, which never warns.

### Performance

- Consolidated `from_julia_array` from 9 sequential Julia↔︎R bridge calls
  to 4 (5–6 for named arrays). The helper is re-entrant and uses
  structured Tuple return (no Main-level globals, no string delimiters).
- `_prepare_for_r` eagerly materialises dense non-zero-copy types
  (`Bool`, `String`, etc.) in Julia, saving a `collect()` round-trip on
  the R side.
- Version counters are now fetched as strings, preventing `UInt32`
  overflow when converting to R integers. `axis_version_counter`,
  `vector_version_counter`, and `matrix_version_counter` return
  character strings; compare with
  [`identical()`](https://rdrr.io/r/base/identical.html) or `==`.
- `_prepare_for_r` flattens type dispatch to reduce cold-start JIT cost.

### CRAN compliance

- DESCRIPTION: updated `Title`, `Description`, `URL`, `BugReports`.
- `cran-comments.md` documents Julia dependency handling, `\dontrun{}`
  examples, vignette eval-off, and the `JULIA_AVAILABLE` test guard.
- Vignette chunks are explicitly `eval = FALSE, purl = FALSE` so R CMD
  check’s tangle step does not try to initialise Julia.
- `inst/WORDLIST` updated for spelling checks.
- All Julia-dependent tests guarded with `skip_if(!JULIA_AVAILABLE)`.

### Safety

- `jl_R_to_julia_type` (internal) now validates string inputs against a
  whitelist regex and rejects anything that isn’t a plausible Julia type
  name. Prevents arbitrary Julia code execution via the type-conversion
  path.
- `Daf(jl_obj)` rejects non-JuliaObject inputs with a clear error
  instead of passing them through silently.

### Breaking changes vs. `dafr`

- Package name. All the options and
  [`library()`](https://rdrr.io/r/base/library.html) calls above.
- `get_vector` / `get_matrix` return values for zero-copy types are now
  live views over Julia memory (via `jlview` / `jlview_sparse`). Copy
  explicitly (`as.numeric(v)`, `as.matrix(m)`) if stability across
  subsequent Daf modifications is needed.

------------------------------------------------------------------------
