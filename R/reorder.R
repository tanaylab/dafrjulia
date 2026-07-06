#' Reorder the entries of one or more axes
#'
#' Permute the entries of the named axes in a writable leaf Daf (or a set of leaf
#' Dafs sharing those axes), rewriting all data that depends on them. Not valid for
#' chain or view Dafs. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/reorder.html) for details.
#'
#' @param daf A writable Daf object, or a list of writable Daf objects.
#' @param permutations A named list mapping each axis name to a 1-based integer
#'   permutation vector of that axis's entries.
#' @return NULL, invisibly. Called for its side effect.
#' @export
reorder_axes <- function(daf, permutations) {
    dafs <- if (is_daf(daf)) list(daf) else daf
    lapply(dafs, validate_daf_object)
    if (!is.list(permutations) || is.null(names(permutations)) || any(names(permutations) == "")) {
        cli::cli_abort("{.arg permutations} must be a named list mapping axis names to integer permutation vectors")
    }
    jl_dafs <- lapply(dafs, function(d) d$jl_obj)
    # A single-axis call means names(permutations) is a length-1 character
    # vector, which JuliaCall would otherwise collapse to a Julia scalar
    # String instead of an AbstractVector{String}; to_julia_vector() forces
    # it through the Julia-side _to_julia_vec() helper to keep it a vector
    # (same pattern used elsewhere in R/data.R for length-1 index vectors).
    axis_names <- to_julia_vector(names(permutations))
    julia_call("_reorder_axes!", jl_dafs, axis_names, unname(permutations))
    invisible(NULL)
}

#' Roll back an interrupted axis reorder
#'
#' If a previous `reorder_axes` call was interrupted, restore the Daf(s) to their
#' pre-reorder state. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/reorder.html) for details.
#'
#' @param daf A writable Daf object, or a list of writable Daf objects.
#' @return TRUE if a stale reorder was rolled back, FALSE if there was nothing to do.
#' @export
reset_reorder_axes <- function(daf) {
    dafs <- if (is_daf(daf)) list(daf) else daf
    lapply(dafs, validate_daf_object)
    jl_dafs <- lapply(dafs, function(d) d$jl_obj)
    julia_call("_reset_reorder_axes!", jl_dafs, need_return = "R")
}
