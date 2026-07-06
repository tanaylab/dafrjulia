#' Get or set whether Daf contracts are enforced
#'
#' Controls the `DAF_ENFORCE_CONTRACTS` global in DataAxesFormats. When enforced,
#' `@computation` contracts are verified (useful in development, expensive in
#' production pipelines).
#'
#' @param enforce If NULL (default), return the current value. Otherwise a single
#'   logical to set.
#' @return With `enforce = NULL`, the current logical value (visibly). When setting,
#'   the previous value (invisibly).
#' @export
enforce_contracts <- function(enforce = NULL) {
    previous <- julia_eval("DataAxesFormats.Contracts.DAF_ENFORCE_CONTRACTS")
    if (is.null(enforce)) {
        return(previous)
    }
    if (!is.logical(enforce) || length(enforce) != 1 || is.na(enforce)) {
        cli::cli_abort("{.arg enforce} must be a single TRUE or FALSE")
    }
    julia_eval(paste0(
        "DataAxesFormats.Contracts.DAF_ENFORCE_CONTRACTS = ",
        if (enforce) "true" else "false"
    ))
    invisible(previous)
}

#' Get or set the packing (chunk/compression) options
#'
#' Reads and, for any non-NULL argument, sets the `DAF_PACKED_*` /
#' `DAF_HTTP_MAX_COALESCE_GAP_KB` globals in DataAxesFormats. These control how
#' `packed = TRUE` storage is chunked, compressed, and cached.
#'
#' @param compression Compression codec name, e.g. "blosc_zstd_bitshuffle" or
#'   "gzip_shuffle" (a Julia Symbol).
#' @param compression_level Integer compression level.
#' @param target_chunk_kb Target chunk size in binary KB.
#' @param local_cache_kb Local decoded-chunk cache size in binary KB.
#' @param http_cache_kb HTTP decoded-chunk cache size in binary KB.
#' @param http_max_coalesce_gap_kb Max gap (binary KB) to coalesce HTTP range reads.
#' @return The values in effect *before* this call, as a named list (visibly when all
#'   arguments are NULL, invisibly when setting).
#' @export
daf_packed_options <- function(compression = NULL,
                               compression_level = NULL,
                               target_chunk_kb = NULL,
                               local_cache_kb = NULL,
                               http_cache_kb = NULL,
                               http_max_coalesce_gap_kb = NULL) {
    pf <- "DataAxesFormats.PackedFormat."
    previous <- list(
        compression = julia_eval(paste0("String(", pf, "DAF_PACKED_COMPRESSION)")),
        compression_level = julia_eval(paste0(pf, "DAF_PACKED_COMPRESSION_LEVEL")),
        target_chunk_kb = julia_eval(paste0(pf, "DAF_PACKED_TARGET_CHUNK_KB")),
        local_cache_kb = julia_eval(paste0(pf, "DAF_PACKED_LOCAL_CACHE_KB")),
        http_cache_kb = julia_eval(paste0(pf, "DAF_PACKED_HTTP_CACHE_KB")),
        http_max_coalesce_gap_kb = julia_eval(paste0(pf, "DAF_HTTP_MAX_COALESCE_GAP_KB"))
    )

    if (!is.null(compression)) {
        julia_eval(paste0(pf, "DAF_PACKED_COMPRESSION = Symbol(\"", compression, "\")"))
    }
    if (!is.null(compression_level)) {
        julia_eval(paste0(pf, "DAF_PACKED_COMPRESSION_LEVEL = ", as.integer(compression_level)))
    }
    if (!is.null(target_chunk_kb)) {
        julia_eval(paste0(pf, "DAF_PACKED_TARGET_CHUNK_KB = ", as.integer(target_chunk_kb)))
    }
    if (!is.null(local_cache_kb)) {
        julia_eval(paste0(pf, "DAF_PACKED_LOCAL_CACHE_KB = ", as.integer(local_cache_kb)))
    }
    if (!is.null(http_cache_kb)) {
        julia_eval(paste0(pf, "DAF_PACKED_HTTP_CACHE_KB = ", as.integer(http_cache_kb)))
    }
    if (!is.null(http_max_coalesce_gap_kb)) {
        julia_eval(paste0(pf, "DAF_HTTP_MAX_COALESCE_GAP_KB = ", as.integer(http_max_coalesce_gap_kb)))
    }

    all_null <- is.null(compression) && is.null(compression_level) &&
        is.null(target_chunk_kb) && is.null(local_cache_kb) &&
        is.null(http_cache_kb) && is.null(http_max_coalesce_gap_kb)
    if (all_null) {
        return(previous)
    }
    invisible(previous)
}
