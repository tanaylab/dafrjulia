# Factory functions for generating query operation wrappers
# These eliminate the massive code duplication in query.R and operations.R

#' Create a query operation function with a required string parameter
#' @noRd
make_string_query_op <- function(julia_fn, param_name = "property") {
    force(julia_fn)
    force(param_name)
    function(value, ...) {
        res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
        if (!res$provided) {
            cli::cli_abort(paste0("{.field ", param_name, "} is missing with no default"))
        }
        if (!is.character(res$value)) {
            cli::cli_abort(paste0("{.field ", param_name, "} must be a character string"))
        }
        ans <- JuliaCall::julia_call(julia_fn, res$value)
        if (!is.null(res$query)) {
            ans <- JuliaCall::julia_call("|>", res$query, ans)
        }
        ans
    }
}

#' Create a query operation with a required value parameter (accepts any type)
#' @noRd
make_value_query_op <- function(julia_fn) {
    force(julia_fn)
    function(value, ...) {
        res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
        if (!res$provided) {
            cli::cli_abort("{.field value} is missing with no default")
        }
        ans <- JuliaCall::julia_call(julia_fn, res$value)
        if (!is.null(res$query)) {
            ans <- JuliaCall::julia_call("|>", res$query, ans)
        }
        ans
    }
}

#' Create a parameterless query operation function
#' @noRd
make_nullary_query_op <- function(julia_fn) {
    force(julia_fn)
    function(...) {
        dots <- list(...)
        non_query <- Filter(function(x) !inherits(x, "JuliaObject"), dots)
        if (length(non_query) > 0) {
            cli::cli_abort("{.code {julia_fn}} expects zero arguments or one query object")
        }
        res <- extract_query_and_value(NULL, TRUE, dots, required = FALSE)
        ans <- JuliaCall::julia_call(julia_fn)
        if (!is.null(res$query)) {
            ans <- JuliaCall::julia_call("|>", res$query, ans)
        }
        ans
    }
}

#' Create an optional-string query operation function
#' @noRd
make_optional_string_query_op <- function(julia_fn, param_name = "value") {
    force(julia_fn)
    force(param_name)
    function(value = NULL, ...) {
        res <- extract_query_and_value(value, missing(value), list(...), required = FALSE, default = NULL)
        if (!is.null(res$value) && !is.character(res$value)) {
            cli::cli_abort(paste0("{.field ", param_name, "} must be a character string or NULL"))
        }
        ans <- JuliaCall::julia_call(julia_fn, res$value)
        if (!is.null(res$query)) {
            ans <- JuliaCall::julia_call("|>", res$query, ans)
        }
        ans
    }
}

#' Create a reduction operation with optional type parameter
#' @noRd
make_typed_reduction_op <- function(julia_fn) {
    force(julia_fn)
    function(type = NULL, ...) {
        validate_dots(list(...), julia_fn)
        res <- extract_query_and_value(type, missing(type), list(...), required = FALSE)
        query <- res$query
        type_val <- res$value
        if (!is.null(type_val)) {
            julia_type <- jl_R_to_julia_type(type_val)
            ans <- JuliaCall::julia_call(julia_fn, type = julia_type)
        } else {
            ans <- JuliaCall::julia_call(julia_fn)
        }
        if (!is.null(query)) {
            ans <- JuliaCall::julia_call("|>", query, ans)
        }
        ans
    }
}
