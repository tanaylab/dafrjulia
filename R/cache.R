#' Create an empty R-side cache environment for a Daf object
#' @noRd
new_daf_cache <- function() {
    new.env(parent = emptyenv())
}

#' Look up a cached value by key, returning it only when the stored
#' version counter matches.
#' @noRd
cache_lookup <- function(daf, cache_key, version_counter) {
    cache <- daf$cache_env
    if (is.null(cache) || !exists(cache_key, envir = cache, inherits = FALSE)) {
        return(NULL)
    }
    entry <- get(cache_key, envir = cache, inherits = FALSE)
    if (identical(entry$vc, version_counter)) entry$val else NULL
}

#' Store a (version_counter, value) entry under `cache_key`, overwriting
#' any previous entry. O(1) per call; no regex, no full-env scan.
#' @noRd
cache_store <- function(daf, cache_key, version_counter, value) {
    cache <- daf$cache_env
    if (is.null(cache)) return(invisible(value))
    assign(cache_key, list(vc = version_counter, val = value), envir = cache)
    invisible(value)
}

#' Empty cache of a Daf object
#'
#' Clears the Julia-side cache and the R-side cache for a Daf object. The
#' R-side cache is always purged (regardless of `clear`/`keep`), because
#' version counters only invalidate entries opportunistically on next
#' access and may miss stale data when backing storage is unmapped or
#' when Julia cache is flushed without bumping data versions.
#'
#' @param daf A Daf object
#' @param clear Cache group to clear. Can be one of "MappedData", "MemoryData", or "QueryData".
#' @param keep Cache group to keep. Can be one of "MappedData", "MemoryData", or "QueryData".
#' @return The Daf object (invisibly, for chaining operations)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Data.empty_cache!) for details.
#' @examples
#' \dontrun{
#' setup_daf()
#' daf <- example_cells_daf()
#' empty_cache(daf) # clear all caches
#' empty_cache(daf, clear = "QueryData") # clear only Julia query cache
#' empty_cache(daf, keep = "MappedData") # keep mapped data, clear rest
#' }
#' @export
empty_cache <- function(daf, clear = NULL, keep = NULL) {
    validate_daf_object(daf)
    valid_groups <- c("MappedData", "MemoryData", "QueryData")
    if (!is.null(clear)) clear <- match.arg(clear, valid_groups)
    if (!is.null(keep)) keep <- match.arg(keep, valid_groups)

    jl_cache_type <- c(
        "MappedData" = "DataAxesFormats.MappedData",
        "MemoryData" = "DataAxesFormats.MemoryData",
        "QueryData" = "DataAxesFormats.QueryData"
    )
    jl_clear <- if (!is.null(clear)) julia_eval(jl_cache_type[[clear]]) else NULL
    jl_keep  <- if (!is.null(keep))  julia_eval(jl_cache_type[[keep]])  else NULL

    julia_call(
        "DataAxesFormats.empty_cache!",
        daf$jl_obj,
        clear = jl_clear,
        keep = jl_keep
    )

    # Always purge R-side cache. The R-side mirror is, by construction,
    # a subset of the Julia cache; if Julia was asked to evict anything,
    # the R-side may be stale. Rebuilding on next access is cheap.
    if (!is.null(daf$cache_env)) {
        rm(list = ls(daf$cache_env, all.names = TRUE), envir = daf$cache_env)
    }

    invisible(daf)
}
