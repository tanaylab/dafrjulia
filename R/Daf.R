#' Daf (Data Axes Format) S3 object
#'
#' An S3 object that wraps a Julia DataAxesFormats object and provides access to its methods.
#'
#' @param jl_obj The Julia DafReader object to wrap
#' @return A Daf S3 object
#' @details See the Julia documentation [here](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/index.html),
#' [here](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/formats.html#Read-API)
#' and [here](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/formats.html#Write-API) for details.
#' @examples
#' \dontrun{
#' setup_daf()
#' daf <- memory_daf("example") # memory_daf() returns a Daf object
#' is_daf(daf) # TRUE
#' }
#' @export
Daf <- function(jl_obj) {
    if (!inherits(jl_obj, "JuliaObject")) {
        cli::cli_abort("Expected a Julia object (JuliaObject) but got {.cls {class(jl_obj)[1]}}")
    }
    is_daf_type <- julia_call("isa", jl_obj, julia_eval("DafReader"), need_return = "R")
    if (!isTRUE(is_daf_type)) {
        obj_type <- julia_call("string", julia_call("typeof", jl_obj), need_return = "R")
        cli::cli_abort("Expected a Julia DafReader object but got {.val {obj_type}}")
    }
    # cache_env is an environment (reference semantics): two R-level copies
    # of the Daf list share the same cache; two wrappers of the same Julia
    # object get separate caches — that's intentional isolation.
    obj <- structure(
        list(jl_obj = jl_obj, cache_env = new_daf_cache()),
        class = "Daf"
    )
    return(obj)
}

#' Print method for Daf objects
#'
#' Prints a description of the Daf object using Julia's description function
#'
#' @param x The Daf object to print
#' @param ... Additional arguments passed to print
#' @return The Daf object (invisibly)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/readers.html#DataAxesFormats.Readers.description) for details.
#' @export
print.Daf <- function(x, ...) {
    description <- julia_call("DataAxesFormats.description", x$jl_obj)
    cat(description)
    invisible(x)
}

#' Get description of a Daf object
#'
#' @param daf A Daf object
#' @param cache Whether to include cache information
#' @param deep Whether to include detailed information about nested data
#' @param tensors Whether to include tensor information (condensed list of tensor matrices)
#' @return A string description of the Daf object
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/readers.html#DataAxesFormats.Readers.description) for details.
#' @export
description <- function(daf, cache = FALSE, deep = FALSE, tensors = TRUE) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.description", daf$jl_obj, cache = cache, deep = deep, tensors = tensors)
}

#' Check if object is a Daf
#'
#' @param x Object to check
#' @return TRUE if x is a Daf object, FALSE otherwise
#' @export
is_daf <- function(x) {
    inherits(x, "Daf")
}

#' Validate that an object is a Daf object
#'
#' @param daf Object to check
#' @param call The calling environment
#' @return NULL, invisibly. Throws an error if object is not a Daf object.
#' @noRd
validate_daf_object <- function(daf, call = parent.frame()) {
    if (!is_daf(daf)) {
        cli::cli_abort("Expected a Daf object but got {class(daf)[1]}", call = call)
    }
    invisible(NULL)
}

#' Extract results from a Daf object using a query
#'
#' @param x A Daf object
#' @param i A query string or object
#' @param ... Ignored. Present for compatibility with the `[` generic.
#' @return The result of the query
#' @details The expression `daf[query]` is equivalent to `get_query(daf, query, cache = TRUE)`.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Operations.get_query) for details.
#' @examples
#' \dontrun{
#' setup_daf()
#' daf <- memory_daf("example")
#' add_axis(daf, "cell", c("A", "B", "C"))
#' set_vector(daf, "cell", "score", c(1.0, 2.0, 3.0))
#' daf[Axis("cell") |> LookupVector("score")]
#' }
#' @export
`[.Daf` <- function(x, i, ...) {
    get_query(x, i, cache = TRUE)
}
