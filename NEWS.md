# dafJuliaWrapper 0.1.0

**First CRAN release. Renamed from `dafr` — the `dafr` name has been freed
for a native-R reimplementation in a separate repository. This package is
the JuliaCall-based wrapper around DataAxesFormats.jl and has been renamed
accordingly.**

## Migration from `dafr`

* `library(dafr)` becomes `library(dafJuliaWrapper)`.
* `options(dafr.JULIA_HOME = ...)` becomes `options(dafJuliaWrapper.JULIA_HOME = ...)`;
  same for `dafr.julia_environment` and other `dafr.*` options.
* Public API (function names, arguments, return shapes) is unchanged.

## New Features

### AnnData-like facade

* `as_anndata(daf)` returns a `DafAnnData` R6 object exposing
  `$X`, `$obs`, `$var`, `$layers`, `$uns`, `$obs_names`, `$var_names`,
  `$n_obs`, `$n_vars`, and `$shape`. Read-only and live: accesses go through
  to the underlying Daf on demand, served by the R-side cache on hits.
  `obs_axis`/`var_axis` auto-detect `cell`/`metacell` and `gene`; `x_name`
  is validated at construction.

### R-side caching

* `get_vector`, `get_matrix`, and `axis_vector` now cache results on the
  R side, keyed by Daf-level version counters so that modifications via
  `set_vector`/`set_matrix`/`add_axis` invalidate entries automatically.
* The cache env is stored directly on each `Daf` object, so there is no
  global registry and no leak. Two R-level copies of the same `Daf` list
  share the cache (reference semantics); two wrappers of the same Julia
  object get separate caches.
* `empty_cache(daf, clear = ..., keep = ...)` now always purges the
  R-side cache (regardless of selectors) and validates its arguments with
  `match.arg`.

### Contracts

* New `CreatedOutput` expectation type, matching DataAxesFormats.jl v0.2.0.
  Like `GuaranteedOutput`, a `CreatedOutput` item is expected to be
  produced by the computation and warns if a value is already present;
  unlike `OptionalOutput`, which never warns.

## Performance

* Consolidated `from_julia_array` from 9 sequential Julia↔R bridge calls
  to 4 (5–6 for named arrays). The helper is re-entrant and uses
  structured Tuple return (no Main-level globals, no string delimiters).
* `_prepare_for_r` eagerly materialises dense non-zero-copy types
  (`Bool`, `String`, etc.) in Julia, saving a `collect()` round-trip on
  the R side.
* Version counters are now fetched as strings, preventing `UInt32`
  overflow when converting to R integers. `axis_version_counter`,
  `vector_version_counter`, and `matrix_version_counter` return
  character strings; compare with `identical()` or `==`.
* `_prepare_for_r` flattens type dispatch to reduce cold-start JIT cost.

## CRAN compliance

* DESCRIPTION: updated `Title`, `Description`, `URL`, `BugReports`.
* `cran-comments.md` documents Julia dependency handling, `\dontrun{}`
  examples, vignette eval-off, and the `JULIA_AVAILABLE` test guard.
* Vignette chunks are explicitly `eval = FALSE, purl = FALSE` so
  R CMD check's tangle step does not try to initialise Julia.
* `inst/WORDLIST` updated for spelling checks.
* All Julia-dependent tests guarded with `skip_if(!JULIA_AVAILABLE)`.

## Safety

* `jl_R_to_julia_type` (internal) now validates string inputs against a
  whitelist regex and rejects anything that isn't a plausible Julia type
  name. Prevents arbitrary Julia code execution via the type-conversion
  path.
* `Daf(jl_obj)` rejects non-JuliaObject inputs with a clear error instead
  of passing them through silently.

## Breaking changes vs. `dafr`

* Package name. All the options and `library()` calls above.
* `get_vector` / `get_matrix` return values for zero-copy types are now
  live views over Julia memory (via `jlview` / `jlview_sparse`). Copy
  explicitly (`as.numeric(v)`, `as.matrix(m)`) if stability across
  subsequent Daf modifications is needed.

---

# dafr 0.1.0 (renamed to dafJuliaWrapper)

## Breaking Changes

### Updated to DataAxesFormats.jl v0.2.0 API

* Renamed query operations to match v0.2.0 naming:
    - `Lookup` -> `LookupVector`
    - `And` -> `AndMask`
    - `AndNot` -> `AndNegatedMask`
    - `Or` -> `OrMask`
    - `OrNot` -> `OrNegatedMask`
    - `Xor` -> `XorMask`
    - `XorNot` -> `XorNegatedMask`
    - `SquareMaskColumn` -> `SquareColumnIs`
    - `SquareMaskRow` -> `SquareRowIs`
* Removed `Fetch` and `MaskSlice` (deprecated wrappers still provided for backwards compatibility)
* Deprecated wrappers provided for all renamed functions with deprecation warnings

### Parameter Changes

* `Names`: removed `kind` parameter
* `IfMissing`: `missing_value` renamed to `default_value`, `type` parameter removed
* `Axis`: now optional in queries

## New Features

### New Query Operations

* Added `LookupScalar` for looking up scalar data
* Added `LookupMatrix` for looking up matrix data
* Added `BeginMask` for starting a mask combination
* Added `BeginNegatedMask` for starting a negated mask combination
* Added `EndMask` for ending a mask combination
* Added `GroupColumnsBy` for grouping columns
* Added `GroupRowsBy` for grouping rows
* Added `ReduceToColumn` for reducing matrix to a column vector
* Added `ReduceToRow` for reducing matrix to a row vector

### New Functions

* Added `complete_path` for constructing complete paths to Daf data
* Added `complete_chain` for opening complete chain Daf repositories

### Documentation

* All documentation URLs updated to DataAxesFormats.jl v0.2.0

# dafr 0.0.3

## New Functions

### New Operations
* Added `Count()` for counting non-zero elements, with optional `type` parameter
* Added `GeoMean()` for geometric mean reduction, with optional `type` and `eps` parameters
* Added `Mode()` for most common value reduction

### Query Utilities
* Added `escape_value()` for escaping special characters in query strings
* Added `unescape_value()` for reversing `escape_value()`
* Added `query_requires_relayout()` to check if a query needs data relayout

### Empty Data Functions
* Added `get_empty_dense_vector()` for getting empty dense vectors for in-place filling
* Added `get_empty_sparse_vector()` for getting empty sparse vectors for in-place filling
* Added `get_empty_dense_matrix()` for getting empty dense matrices for in-place filling
* Added `get_empty_sparse_matrix()` for getting empty sparse matrices for in-place filling
* Added `filled_empty_sparse_vector()` for committing filled sparse vectors back to Daf
* Added `filled_empty_sparse_matrix()` for committing filled sparse matrices back to Daf
* Exported `get_frame()` for direct use

## Enhanced Parameters

* Added `type` parameter to all reduction operations (`Abs`, `Sum`, `Mean`, `Median`, `Quantile`, `Var`, `VarN`, `Std`, `StdN`, `Min`, `Max`, `Count`, `Fraction`, `Round`, `Clamp`, `Log`)
* Added `eps` parameter to `VarN`, `StdN`, `GeoMean`
* Added `type` and `insist` parameters to `copy_scalar()`
* Added `eltype`, `bestify`, `min_sparse_saving_fraction`, and `insist` parameters to `copy_vector()`
* Added `eltype`, `bestify`, `min_sparse_saving_fraction`, and `insist` parameters to `copy_matrix()`
* Added `relayout`, `bestify`, `min_sparse_saving_fraction` parameters to `copy_tensor()`
* Added `X_eltype` parameter to `daf_as_h5ad()`

## CI/CD

* Added GitHub Actions workflows for R CMD check, conda build, and pkgdown site deployment
* Added conda recipe for building and distributing conda packages
* Fixed CI test execution to properly install package before running testthat tests

## Tests

* Added comprehensive tests for all new functions and parameters
* New test files: `test-operations.R`, `test-copies.R`, `test-data-writers.R`, `test-queries.R`, `test-anndata_format.R`

# dafr 0.0.2

## New Features

### Contract System
* Added `create_contract()` for defining data contracts with axes and data specifications
* Added `axis_contract()`, `scalar_contract()`, `vector_contract()`, `matrix_contract()` for specifying contract requirements
* Added `tensor_contract()` for 3D tensor specifications
* Added `verify_contract()`, `verify_input()`, `verify_output()` for validating Daf objects against contracts
* Added `contractor()` for creating contract-aware Daf wrappers
* Added `contract_docs()` for generating markdown/text documentation from contracts
* Added expectation types: `RequiredInput`, `OptionalInput`, `GuaranteedOutput`, `OptionalOutput`

### Group Functions
* Added `group_names()` for generating unique deterministic names for groups based on members
* Added `collect_group_members()` for converting group indices to member lists
* Added `compact_groups()` for compacting non-consecutive group indices to 1..N

### View Constants
* Added `VIEW_ALL_AXES`, `VIEW_ALL_SCALARS`, `VIEW_ALL_VECTORS`, `VIEW_ALL_MATRICES`, `VIEW_ALL_DATA` constants for creating views of complete data

### Query Utilities
* Added `is_axis_query()` to check if a query targets only axes
* Added `query_axis_name()` to extract axis name from axis-only queries

### Version Counters
* Added `axis_version_counter()`, `vector_version_counter()`, `matrix_version_counter()` for tracking data modifications

### Complete/Open Functions
* Added `complete_daf()` for opening complete chains of Daf repositories
* Added `open_daf()` for smart opening of files-based or HDF5-based Daf

### Example Data
* Added `example_chain_daf()` for creating example chain data

## Improvements

* Added `tensors` parameter to `description()` function
* Fixed issue with error when returning only names (#6)
* Improved test coverage with 1201 tests (up from baseline)

# dafr 0.0.1

* Initial WIP
