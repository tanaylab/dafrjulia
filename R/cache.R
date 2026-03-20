# Global cache registry: maps Daf object id -> cache environment
.daf_cache_registry <- new.env(parent = emptyenv())

#' Get or create cache environment for a Daf object
#' @noRd
get_daf_cache <- function(daf) {
    obj_id <- get_daf_id(daf)
    if (!exists(obj_id, envir = .daf_cache_registry)) {
        cache_env <- new.env(parent = emptyenv())
        assign(obj_id, cache_env, envir = .daf_cache_registry)
    }
    get(obj_id, envir = .daf_cache_registry)
}

#' Get a stable identifier for a Daf object
#' @noRd
get_daf_id <- function(daf) {
    JuliaCall::julia_call("string", JuliaCall::julia_call("objectid", daf$jl_obj), need_return = "R")
}

#' Look up a cached value, checking version counter
#' @noRd
cache_lookup <- function(daf, cache_key, version_counter) {
    cache <- get_daf_cache(daf)
    full_key <- paste0(version_counter, ":", cache_key)
    if (exists(full_key, envir = cache)) {
        return(get(full_key, envir = cache))
    }
    NULL
}

#' Store a value in the cache
#' @noRd
cache_store <- function(daf, cache_key, version_counter, value) {
    cache <- get_daf_cache(daf)
    # Remove old versions of this key (different version counter prefix, same suffix)
    old_keys <- ls(cache)
    # Escape the cache_key for regex use
    escaped_key <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", cache_key)
    pattern <- paste0("^[0-9]+:", escaped_key, "$")
    stale <- old_keys[grepl(pattern, old_keys)]
    for (k in stale) {
        rm(list = k, envir = cache)
    }
    full_key <- paste0(version_counter, ":", cache_key)
    assign(full_key, value, envir = cache)
    invisible(value)
}

#' Empty cache of a Daf object
#'
#' Clears both the Julia-side cache and the R-side cache for a Daf object.
#'
#' @param daf A Daf object
#' @param clear Cache group to clear. Can be one of "MappedData", "MemoryData", or "QueryData".
#' @param keep Cache group to keep. Can be one of "MappedData", "MemoryData", or "QueryData".
#' @return The Daf object (invisibly, for chaining operations)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Data.empty_cache!) for details.
#' @export
empty_cache <- function(daf, clear = NULL, keep = NULL) {
    validate_daf_object(daf)
    jl_cache_type <- c(
        "MappedData" = "DataAxesFormats.MappedData",
        "MemoryData" = "DataAxesFormats.MemoryData",
        "QueryData" = "DataAxesFormats.QueryData"
    )

    if (!is.null(clear)) {
        clear <- julia_eval(jl_cache_type[clear])
    }
    if (!is.null(keep)) {
        keep <- julia_eval(jl_cache_type[keep])
    }

    # Clear Julia cache
    julia_call(
        "DataAxesFormats.empty_cache!",
        daf$jl_obj,
        clear = clear,
        keep = keep
    )

    # Clear R-side cache
    obj_id <- get_daf_id(daf)
    if (exists(obj_id, envir = .daf_cache_registry)) {
        cache <- get(obj_id, envir = .daf_cache_registry)
        rm(list = ls(cache), envir = cache)
    }

    invisible(daf)
}
