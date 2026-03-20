#' Deprecated query operations
#'
#' These functions have been renamed or removed in DataAxesFormats v0.2.0.
#' They are provided for backward compatibility but will be removed in a future version.
#'
#' @name deprecated-queries
NULL

#' @describeIn LookupVector Deprecated: use [LookupVector()] instead.
#' @param property String specifying the property
#' @param ... Additional arguments
#' @export
Lookup <- function(property, ...) {
    .Deprecated("LookupVector")
    LookupVector(property, ...)
}

#' @describeIn AndMask Deprecated: use [AndMask()] instead.
#' @param property String specifying the property
#' @param ... Additional arguments
#' @export
And <- function(property, ...) {
    .Deprecated("AndMask")
    AndMask(property, ...)
}

#' @describeIn AndNegatedMask Deprecated: use [AndNegatedMask()] instead.
#' @param property String specifying the property
#' @param ... Additional arguments
#' @export
AndNot <- function(property, ...) {
    .Deprecated("AndNegatedMask")
    AndNegatedMask(property, ...)
}

#' @describeIn OrMask Deprecated: use [OrMask()] instead.
#' @param property String specifying the property
#' @param ... Additional arguments
#' @export
Or <- function(property, ...) {
    .Deprecated("OrMask")
    OrMask(property, ...)
}

#' @describeIn OrNegatedMask Deprecated: use [OrNegatedMask()] instead.
#' @param property String specifying the property
#' @param ... Additional arguments
#' @export
OrNot <- function(property, ...) {
    .Deprecated("OrNegatedMask")
    OrNegatedMask(property, ...)
}

#' @describeIn XorMask Deprecated: use [XorMask()] instead.
#' @param property String specifying the property
#' @param ... Additional arguments
#' @export
Xor <- function(property, ...) {
    .Deprecated("XorMask")
    XorMask(property, ...)
}

#' @describeIn XorNegatedMask Deprecated: use [XorNegatedMask()] instead.
#' @param property String specifying the property
#' @param ... Additional arguments
#' @export
XorNot <- function(property, ...) {
    .Deprecated("XorNegatedMask")
    XorNegatedMask(property, ...)
}

#' @describeIn SquareColumnIs Deprecated: use [SquareColumnIs()] instead.
#' @param value String specifying the value
#' @param ... Additional arguments
#' @export
SquareMaskColumn <- function(value, ...) {
    .Deprecated("SquareColumnIs")
    SquareColumnIs(value, ...)
}

#' @describeIn SquareRowIs Deprecated: use [SquareRowIs()] instead.
#' @param value String specifying the value
#' @param ... Additional arguments
#' @export
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
