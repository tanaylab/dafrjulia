#' Check if a query can be applied to a Daf object
#'
#' Determines whether a query can be validly applied to a Daf object. This is useful for
#' checking if the properties referenced in a query exist in the Daf object before
#' attempting to execute the query.
#'
#' @param daf A Daf object
#' @param query Query string or object. Can be created using query operations such as
#'   Axis(), LookupVector(), IsGreater(), etc.
#' @return TRUE if query can be applied, FALSE otherwise
#' @include query_factories.R
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Operations.has_query) for details.
#' @seealso [get_query()], [parse_query()], and the query operations documentation.
#' @export
has_query <- function(daf, query) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.Queries.has_query", daf$jl_obj, query)
}

#' Apply a query to a Daf object
#'
#' Executes a query on a Daf object and returns the result. Queries provide a way to extract,
#' filter, and manipulate data from a Daf object using a composable syntax. Queries can retrieve
#' scalars, vectors, matrices, or sets of names depending on the operations used.
#'
#' @param daf A Daf object
#' @param query Query string or object. Can be created using query operations such as
#'   Axis(), LookupVector(), IsGreater(), etc.
#' In order to support the use of pipe operators, the query can also be a Daf object and vice versa, see examples below.
#' @param cache Whether to cache the query result
#' @return The result of the query, which could be a scalar, vector, matrix, or set of names depending on the query
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Operations.get_query) for details.
#' @seealso [has_query()], [parse_query()], [get_dataframe_query()], and the query operations documentation.
#' @export
get_query <- function(daf = NULL, query = NULL, cache = TRUE) {
    if (is.null(daf) || is.null(query)) {
        cli::cli_abort("Please provide both a Daf object and a query.")
    }

    daf_arg <- daf
    query_arg <- query
    if (inherits(query, "Daf")) {
        daf <- query_arg
        query <- daf_arg
    }

    validate_daf_object(daf)
    result <- julia_call("DataAxesFormats.Queries.get_query", daf$jl_obj, query, cache = cache, need_return = "Julia")

    return(from_julia_object(result))
}

#' Apply a query to a Daf object and return result as a data.frame
#'
#' Executes a query on a Daf object and returns the result formatted as a data.frame for
#' easier use in R analysis workflows. This is a convenience wrapper around get_query
#' that provides properly structured dataframes for different result types.
#'
#' @param daf A Daf object
#' @param query Query string or object. Can be created using query operations such as
#'   Axis(), LookupVector(), IsGreater(), etc.
#' In order to support the use of pipe operators, the query can also be a Daf object and vice versa.
#' @param cache Whether to cache the query result
#' @return A data.frame representation of the query result. If the query result is a matrix, row names and column names are the axis entries. If the query result is a scalar, a data.frame with one row and one column is returned. If the query result is a vector, a data.frame with a column "value" is returned.
#' @seealso get_query, get_frame
#' @export
get_dataframe_query <- function(daf = NULL, query = NULL, cache = TRUE) {
    if (is.null(daf) || is.null(query)) {
        cli::cli_abort("Please provide both a Daf object and a query.")
    }

    # Handle pipe operator usage
    daf_arg <- daf
    query_arg <- query
    if (inherits(query, "Daf")) {
        daf <- query_arg
        query <- daf_arg
    }

    validate_daf_object(daf)

    # Get dimensions to determine how to process the result
    dims <- query_result_dimensions(query)

    result <- julia_call("DataAxesFormats.Queries.get_query",
        daf$jl_obj,
        query,
        cache = cache,
        need_return = if (dims == 0) "R" else "Julia"
    )

    if (dims == 0) {
        return(data.frame(value = result, stringsAsFactors = FALSE))
    }

    if (dims == 1) {
        # For vectors, extract the values and names directly from the NamedVector
        values <- from_julia_array(result)

        # Extract the names using NamedArrays.names
        names <- julia_call("NamedArrays.names", result, as.integer(1), need_return = "R")

        # Create an R data frame with proper names
        df <- data.frame(
            value = values,
            stringsAsFactors = FALSE,
            row.names = names
        )

        return(df)
    } else if (dims == 2) {
        # For matrices, extract the values and both row and column names
        values <- from_julia_array(result)

        # Handle sparse matrices differently
        if (inherits(values, "sparseMatrix")) {
            # Convert sparse matrix to a dense matrix for data frame conversion
            dense_matrix <- as.matrix(values)
            df <- as.data.frame(dense_matrix)

            # Preserve row and column names from the sparse matrix
            rownames(df) <- rownames(values)
            colnames(df) <- colnames(values)
        } else {
            # Regular dense matrix
            df <- as.data.frame(values)
        }

        return(df)
    } else {
        # For name set results (dims == -1), convert directly
        return(from_julia_object(result))
    }
}

#' Parse a query string into a query object
#'
#' Converts a string representation of a query into a query object that can be applied to a Daf object.
#' This allows for a more concise syntax for creating complex queries. If you want something similar
#' to the `q` prefix used in Julia, you can write `q <- parse_query` in your R code.
#'
#' @param query_string String containing the query
#' @return A query object that can be used with get_query and has_query
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html) for details.
#' @export
parse_query <- function(query_string) {
    julia_call("DataAxesFormats.Queries.parse_query", query_string)
}

#' Get the number of dimensions of a query result
#'
#' Determines the dimensionality of the result that would be produced by applying a query.
#' This is useful for understanding what kind of data structure to expect from a query before
#' executing it, which can help with writing code that handles different result types.
#'
#' @param query Query string or object
#' @return Number of dimensions (-1 - names, 0 - scalar, 1 - vector, 2 - matrix)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html) for details.
#' @export
query_result_dimensions <- function(query) {
    julia_call("DataAxesFormats.Queries.query_result_dimensions", query)
}

#' @title Axis query operation
#' @description A query operation for specifying a result axis in a query sequence.
#' This is typically the first operation in a query sequence and determines which axis
#' the query will operate on. It sets the context for subsequent operations in the query.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.Axis) for details.
#' @param value Optional string specifying the axis name (NULL for unspecified)
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Axis <- make_optional_string_query_op("DataAxesFormats.Queries.Axis", param_name = "axis")

#' @title LookupVector query operation
#' @description A query operation for looking up the value of a vector property with a specific name.
#' This operation retrieves the data associated with the specified property for the current axis context.
#' It is typically used after an `Axis` operation to select a vector property from that axis.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.LookupVector)
#' for details.
#' @param value String specifying the property name to look up
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
LookupVector <- make_optional_string_query_op("DataAxesFormats.Queries.LookupVector", param_name = "property")

#' @title LookupScalar query operation
#' @description A query operation for looking up the value of a scalar property.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.LookupScalar)
#' for details.
#' @param value String specifying the scalar property name to look up
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
LookupScalar <- make_optional_string_query_op("DataAxesFormats.Queries.LookupScalar", param_name = "property")

#' @title LookupMatrix query operation
#' @description A query operation for looking up the value of a matrix property.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.LookupMatrix)
#' for details.
#' @param value String specifying the matrix property name to look up
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
LookupMatrix <- make_string_query_op("DataAxesFormats.Queries.LookupMatrix", param_name = "property")

#' @title Names query operation
#' @description A query operation for looking up a set of names in a Daf object.
#' This operation retrieves the entry names of the current axis.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.Names)
#' for details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Names <- make_nullary_query_op("DataAxesFormats.Queries.Names")

#' @title IfMissing query operation
#' @description A query operation providing a default value to use if the data is missing some property.
#' This is useful when querying for properties that might not exist for all entries, allowing you to
#' provide a fallback value instead of getting an error. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.IfMissing)
#' for details.
#' @param default_value Value to use when data is missing the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
IfMissing <- function(default_value, ...) {
    res <- extract_query_and_value(default_value, missing(default_value), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("argument {.field default_value} is missing with no default")
    }

    if (identical(res$value, NA)) {
        cli::cli_abort("{.field default_value} cannot be NA. See the Julia documentation for details.")
    }

    ans <- julia_call("DataAxesFormats.Queries.IfMissing", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title IfNot query operation
#' @description A query operation providing a value to use for "false-ish" values in a vector.
#' This replaces empty strings, zero numeric values, or false Boolean values with the specified value.
#' This is useful for handling default or missing values in data without completely replacing the property.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.IfNot) for
#' details.
#' @param value Optional value to use for replacement. If NULL, uses the default replacement value.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
IfNot <- function(value = NULL, ...) {
    res <- extract_query_and_value(value, missing(value), list(...), required = FALSE, default = NULL)

    # No specific validation needed for value (can be NULL or various types)

    result <- julia_call("DataAxesFormats.Queries.IfNot", res$value)

    if (!is.null(res$query)) {
        result <- julia_call("|>", res$query, result)
    }
    return(result)
}

#' @title AsAxis query operation
#' @description A query operation that treats values in a vector property as names of entries in another axis.
#' This operation is commonly used with `CountBy` and `GroupBy`,
#' where vector values need to be interpreted as axis entry names. If no axis is specified, values
#' are treated as entries of a default axis based on context. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.AsAxis) for
#' details.
#' @param value Optional string specifying the axis name to use for interpreting the values
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
AsAxis <- make_optional_string_query_op("DataAxesFormats.Queries.AsAxis", param_name = "axis")

#' @title BeginMask query operation
#' @description Start specifying a mask to apply to an axis of the result.
#' Must be accompanied by an EndMask. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.BeginMask) for details.
#' @param value String specifying the property name for the initial mask
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
BeginMask <- make_string_query_op("DataAxesFormats.Queries.BeginMask")

#' @title BeginNegatedMask query operation
#' @description Start specifying a negated mask to apply to an axis of the result.
#' Must be accompanied by an EndMask. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.BeginNegatedMask) for details.
#' @param value String specifying the property name for the initial negated mask
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
BeginNegatedMask <- make_string_query_op("DataAxesFormats.Queries.BeginNegatedMask")

#' @title EndMask query operation
#' @description Finish specifying a mask to apply to an axis, following BeginMask or BeginNegatedMask.
#' See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.EndMask) for details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
EndMask <- make_nullary_query_op("DataAxesFormats.Queries.EndMask")

#' @title SquareColumnIs query operation
#' @description Used when the mask matrix is square and we'd like to use a column as a mask. See the
#' Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.SquareColumnIs)
#' for details.
#' @param value String specifying the value
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
SquareColumnIs <- make_string_query_op("DataAxesFormats.Queries.SquareColumnIs", param_name = "value")

#' @title SquareRowIs query operation
#' @description Used when the mask matrix is square and we'd like to use a row as a mask. See the
#' Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.SquareRowIs) for
#' details.
#' @param value String specifying the value
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
SquareRowIs <- make_string_query_op("DataAxesFormats.Queries.SquareRowIs", param_name = "value")

#' @title AndMask query operation
#' @description A query operation for filtering axis entries using a Boolean mask.
#' This operation restricts the set of entries of an axis to only those where the specified
#' property contains true values (or non-zero/non-empty values). It essentially performs
#' a logical AND between the current selection and the specified property, treating the
#' property as a Boolean mask.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.AndMask) for details.
#' @param value String specifying the property to use as a filter mask
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
AndMask <- make_string_query_op("DataAxesFormats.Queries.AndMask")

#' @title AndNegatedMask query operation
#' @description Same as `AndMask` but use the inverse of the mask. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.AndNegatedMask) for
#' details.
#' @param value String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
AndNegatedMask <- make_string_query_op("DataAxesFormats.Queries.AndNegatedMask")

#' @title OrMask query operation
#' @description A query operation for expanding the set of entries of an axis.
#' This operation adds entries to the current selection where the specified property
#' contains true values (or non-zero/non-empty values). It performs a logical OR
#' between the current selection and the specified property, treating the property
#' as a Boolean mask.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.OrMask) for details.
#' @param value String specifying the property to use for expanding the selection
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
OrMask <- make_string_query_op("DataAxesFormats.Queries.OrMask")

#' @title OrNegatedMask query operation
#' @description Same as `OrMask` but use the inverse of the mask. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.OrNegatedMask) for details.
#' @param value String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
OrNegatedMask <- make_string_query_op("DataAxesFormats.Queries.OrNegatedMask")

#' @title XorMask query operation
#' @description A query operation for flipping the set of entries of an `Axis`. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.XorMask) for details.
#' @param value String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
XorMask <- make_string_query_op("DataAxesFormats.Queries.XorMask")

#' @title XorNegatedMask query operation
#' @description Same as `XorMask` but use the inverse of the mask. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.XorNegatedMask) for details.
#' @param value String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
XorNegatedMask <- make_string_query_op("DataAxesFormats.Queries.XorNegatedMask")

#' @title IsLess query operation
#' @description A query operation for filtering based on numeric comparison.
#' This operation converts a vector property to a Boolean mask by comparing each value
#' to the specified threshold using the less-than (`<`) operator. Only entries where the
#' comparison returns true are included in the result. Typically used after a LookupVector operation
#' to filter entries based on numeric values.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.IsLess) for details.
#' @param value Threshold value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
IsLess <- make_value_query_op("DataAxesFormats.Queries.IsLess")

#' @title IsLessEqual query operation
#' @description Similar to `IsLess` except that uses `<=` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.IsLessEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsLessEqual <- make_value_query_op("DataAxesFormats.Queries.IsLessEqual")

#' @title IsEqual query operation
#' @description Equality is used for two purposes: As a comparison operator, similar to `IsLess` except that uses `=` instead of
#' `<` for the comparison; and To select a single entry from a vector. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.IsEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsEqual <- make_value_query_op("DataAxesFormats.Queries.IsEqual")

#' @title IsNotEqual query operation
#' @description Similar to `IsLess` except that uses `!=` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.IsNotEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsNotEqual <- make_value_query_op("DataAxesFormats.Queries.IsNotEqual")

#' @title IsGreater query operation
#' @description Similar to `IsLess` except that uses `>` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.IsGreater) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsGreater <- make_value_query_op("DataAxesFormats.Queries.IsGreater")

#' @title IsGreaterEqual query operation
#' @description Similar to `IsLess` except that uses `>=` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.IsGreaterEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsGreaterEqual <- make_value_query_op("DataAxesFormats.Queries.IsGreaterEqual")

#' @title IsMatch query operation
#' @description Similar to `IsLess` except that the compared values must be strings, and the mask
#' is of the values that match the given regular expression. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.IsMatch) for
#' details.
#' @param value Regular expression pattern to match against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsMatch <- make_string_query_op("DataAxesFormats.Queries.IsMatch", param_name = "value")

#' @title IsNotMatch query operation
#' @description Similar to `IsMatch` except that looks for entries that do not match the pattern. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.IsNotMatch) for
#' details.
#' @param value Regular expression pattern to not match against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsNotMatch <- make_string_query_op("DataAxesFormats.Queries.IsNotMatch", param_name = "value")

#' @title CountBy query operation
#' @description A query operation that generates a matrix of counts of combinations of pairs of values.
#' This operation creates a contingency table counting the occurrences of each combination of values
#' between the current property and the specified property, for the same entries of an axis.
#' The result is a matrix whose rows are the values of the first property, columns are the values
#' of the second property, and entries are the counts of occurrences of each combination.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.CountBy) for
#' details.
#' @param value String specifying the property to count combinations with
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
CountBy <- make_string_query_op("DataAxesFormats.Queries.CountBy")

#' @title GroupBy query operation
#' @description A query operation that aggregates values by groups.
#' This operation takes a property whose values define groups, and applies a subsequent
#' reduction operation (e.g., Mean, Sum, Max) to aggregate the values within each group.
#' If applied to a vector, the result is a vector with one entry per group. If applied to a matrix,
#' the result is a matrix with one row per group. This is typically followed by a reduction
#' operation that specifies how to aggregate the grouped values.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.GroupBy) for
#' details.
#' @param value String specifying the property to group by
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
GroupBy <- make_string_query_op("DataAxesFormats.Queries.GroupBy")

#' @title GroupColumnsBy query operation
#' @description Specify value per matrix column to group the columns by. Must be followed
#' by a ReduceToColumn to reduce each group of columns to a single column.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.GroupColumnsBy) for details.
#' @param value String specifying the property to group columns by
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
GroupColumnsBy <- make_string_query_op("DataAxesFormats.Queries.GroupColumnsBy")

#' @title GroupRowsBy query operation
#' @description Specify value per matrix row to group the rows by. Must be followed
#' by a ReduceToRow to reduce each group of rows to a single row.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.GroupRowsBy) for details.
#' @param value String specifying the property to group rows by
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
GroupRowsBy <- make_string_query_op("DataAxesFormats.Queries.GroupRowsBy")

#' @title ReduceToColumn query operation
#' @description Specify a reduction operation to convert each row of a matrix to a single value,
#' reducing the matrix to a single column.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.ReduceToColumn) for details.
#' @param reduction A reduction operation (e.g., from Mean(), Sum(), etc.) or a string describing the reduction
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
ReduceToColumn <- function(reduction, ...) {
    dots <- list(...)

    if (missing(reduction)) {
        cli::cli_abort("{.field reduction} is missing with no default")
    }

    # When piped, reduction gets the query and actual reduction is in ...
    if (inherits(reduction, "JuliaObject") && length(dots) > 0 && inherits(dots[[1]], "JuliaObject")) {
        query <- reduction
        actual_reduction <- dots[[1]]
    } else if (inherits(reduction, "JuliaObject")) {
        query <- NULL
        actual_reduction <- reduction
    } else {
        cli::cli_abort("{.field reduction} must be a query operation object")
    }

    ans <- julia_call("DataAxesFormats.Queries.ReduceToColumn", actual_reduction)
    if (!is.null(query)) {
        ans <- julia_call("|>", query, ans)
    }
    ans
}

#' @title ReduceToRow query operation
#' @description Specify a reduction operation to convert each column of a matrix to a single value,
#' reducing the matrix to a single row.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.ReduceToRow) for details.
#' @param reduction A reduction operation (e.g., from Mean(), Sum(), etc.) or a string describing the reduction
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
ReduceToRow <- function(reduction, ...) {
    dots <- list(...)

    if (missing(reduction)) {
        cli::cli_abort("{.field reduction} is missing with no default")
    }

    # When piped, reduction gets the query and actual reduction is in ...
    if (inherits(reduction, "JuliaObject") && length(dots) > 0 && inherits(dots[[1]], "JuliaObject")) {
        query <- reduction
        actual_reduction <- dots[[1]]
    } else if (inherits(reduction, "JuliaObject")) {
        query <- NULL
        actual_reduction <- reduction
    } else {
        cli::cli_abort("{.field reduction} must be a query operation object")
    }

    ans <- julia_call("DataAxesFormats.Queries.ReduceToRow", actual_reduction)
    if (!is.null(query)) {
        ans <- julia_call("|>", query, ans)
    }
    ans
}

#' Check if a query returns axis entries
#'
#' Determines whether a query will return axis entries (names) as opposed to
#' scalar values, vectors, or matrices.
#'
#' @param query Query string or query object
#' @return TRUE if the query returns axis entries, FALSE otherwise
#' @details This is useful for determining the expected result type of a query
#'   before executing it. A query that returns axis entries can be used as a filter
#'   for other queries.
#' @export
is_axis_query <- function(query) {
    julia_call("DataAxesFormats.Queries.is_axis_query", query)
}

#' Get the axis name from a query
#'
#' Returns the name of the axis that a query operates on.
#'
#' @param query Query string or query object
#' @return The name of the axis as a character string
#' @details This is useful for understanding which axis a query will affect
#'   or for building compound queries programmatically.
#' @export
query_axis_name <- function(query) {
    julia_call("DataAxesFormats.Queries.query_axis_name", query)
}

#' Escape a value for use in a query string
#'
#' Escapes special characters in a value so it can be safely embedded in a query string.
#' This is needed when values contain characters that have special meaning in the query
#' syntax (e.g., spaces, quotes, backslashes).
#'
#' @param value A character string value to escape
#' @return The escaped value as a character string
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.escape_value) for details.
#' @export
escape_value <- function(value) {
    julia_call("DataAxesFormats.Queries.escape_value", value)
}

#' Unescape a value from a query string
#'
#' Reverses the escaping done by `escape_value`, restoring the original value.
#'
#' @param value A character string value to unescape
#' @return The unescaped value as a character string
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.unescape_value) for details.
#' @export
unescape_value <- function(value) {
    julia_call("DataAxesFormats.Queries.unescape_value", value)
}

#' Check if a query requires relayout
#'
#' Determines whether executing a query on a Daf object would require a matrix relayout
#' operation. This is useful for performance optimization, as relayout can be expensive.
#'
#' @param daf A Daf object
#' @param query Query string or query object
#' @return TRUE if the query requires relayout, FALSE otherwise
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Queries.query_requires_relayout) for details.
#' @export
query_requires_relayout <- function(daf, query) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.Queries.query_requires_relayout", daf$jl_obj, query)
}
