#' Deprecated query operations
#'
#' These functions have been renamed or removed in DataAxesFormats v0.2.0.
#' They are provided for backward compatibility but will be removed in a future version.
#'
#' @name deprecated-queries
NULL

#' @export
#' @rdname LookupVector
Lookup <- function(property, ...) {
    .Deprecated("LookupVector")
    LookupVector(property, ...)
}

#' @export
#' @rdname AndMask
And <- function(property, ...) {
    .Deprecated("AndMask")
    AndMask(property, ...)
}

#' @export
#' @rdname AndNegatedMask
AndNot <- function(property, ...) {
    .Deprecated("AndNegatedMask")
    AndNegatedMask(property, ...)
}

#' @export
#' @rdname OrMask
Or <- function(property, ...) {
    .Deprecated("OrMask")
    OrMask(property, ...)
}

#' @export
#' @rdname OrNegatedMask
OrNot <- function(property, ...) {
    .Deprecated("OrNegatedMask")
    OrNegatedMask(property, ...)
}

#' @export
#' @rdname XorMask
Xor <- function(property, ...) {
    .Deprecated("XorMask")
    XorMask(property, ...)
}

#' @export
#' @rdname XorNegatedMask
XorNot <- function(property, ...) {
    .Deprecated("XorNegatedMask")
    XorNegatedMask(property, ...)
}

#' @export
#' @rdname SquareColumnIs
SquareMaskColumn <- function(value, ...) {
    .Deprecated("SquareColumnIs")
    SquareColumnIs(value, ...)
}

#' @export
#' @rdname SquareRowIs
SquareMaskRow <- function(value, ...) {
    .Deprecated("SquareRowIs")
    SquareRowIs(value, ...)
}

#' Fetch (Deprecated)
#'
#' This function has been removed in DataAxesFormats v0.2.0.
#' Use \code{\link{LookupVector}} with \code{AsAxis()} instead.
#'
#' @param property A property to fetch.
#' @param ... Additional arguments (ignored).
#'
#' @export
Fetch <- function(property, ...) {
    .Deprecated(msg = "Fetch has been removed in DataAxesFormats v0.2.0. Use LookupVector() with AsAxis() instead.")
    cli::cli_abort("Fetch is no longer available. Use LookupVector() with AsAxis() instead.")
}

#' MaskSlice (Deprecated)
#'
#' This function has been removed in DataAxesFormats v0.2.0.
#' Use \code{\link{BeginMask}}/\code{\link{EndMask}} instead.
#'
#' @param axis An axis to slice.
#' @param ... Additional arguments (ignored).
#'
#' @export
MaskSlice <- function(axis, ...) {
    .Deprecated(msg = "MaskSlice has been removed in DataAxesFormats v0.2.0. Use BeginMask()/EndMask() instead.")
    cli::cli_abort("MaskSlice is no longer available. Use BeginMask()/EndMask() instead.")
}
