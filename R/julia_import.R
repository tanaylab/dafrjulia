# Package-level environment for caching Julia type maps and other state
dafr_env <- new.env(parent = emptyenv())

#' Check if Julia is initialized
#'
#' @return TRUE if Julia has been initialized via setup_daf(), FALSE otherwise
#' @noRd
is_julia_initialized <- function() {
    tryCatch(
        {
            JuliaCall::julia_eval("true")
            TRUE
        },
        error = function(e) {
            FALSE
        }
    )
}

#' Use default or custom Julia environment
#'
#' Force JuliaCall to use your specified Julia environment instead of creating a new one.
#' By default JuliaCall creates its own separate independent environment, which means
#' that it re-downloads and re-installs all the dependency packages there.
#' This function allows using the default Julia environment or a custom one.
#'
#' @param env_path Either "default" to use the default Julia environment, or a path to a custom environment
#' @return No return value, called for side effects.
#' @export
use_default_julia_environment <- function(env_path = "default") {
    if (env_path == "default") {
        default_env <- JuliaCall::julia_eval('joinpath(DEPOT_PATH[1], "environments", "v$(VERSION.major).$(VERSION.minor)")')
        JuliaCall::julia_call("Pkg.activate", default_env)
    } else {
        JuliaCall::julia_call("Pkg.activate", env_path)
    }
}

import_julia_packages <- function() {
    julia_eval("using Pkg")

    julia_eval("using DataAxesFormats")
    julia_eval("using TanayLabUtilities")

    julia_eval("import DataFrames")
    julia_eval("import HDF5")
    julia_eval("import LinearAlgebra")
    julia_eval("import Logging")
    julia_eval("import Muon")
    julia_eval("import NamedArrays")
    julia_eval("import SparseArrays")
}


define_julia_functions <- function() {
    julia_eval("
    function _to_daf_readers(readers::AbstractVector)::Vector{DafReader}
        return Vector{DafReader}(readers)
    end
    ")

    julia_eval("
    function _pairify_columns(items::Maybe{AbstractVector})::Maybe{DataAxesFormats.Queries.FrameColumns}
        if items == nothing
            return nothing
        else
            return [name => query for (name, query) in items]
        end
    end")

    julia_eval("
    function _pairify_axes(items::Maybe{AbstractVector})::Maybe{DataAxesFormats.ViewAxes}
        if items == nothing
            return nothing
        else
            return [key => query for (key, query) in items]
        end
    end")

    julia_eval("
    function _pairify_data(items::Maybe{AbstractVector})::Maybe{DataAxesFormats.ViewData}
    if items == nothing
        return nothing
    else
        return [typeof(key) <: AbstractVector ? Tuple(key) => query : key => query for (key, query) in items]
    end
    end")

    julia_eval("
    function _pairify_merge(items::Maybe{AbstractVector})::Maybe{DataAxesFormats.MergeData}
        if items == nothing
            return nothing
        else
            return [key => query for (key, query) in items]
        end
    end")

    julia_eval("_DafReadersVector = Vector{DafReader}")

    # Add function to convert R strings to Julia AbstractString
    julia_eval("
    function _to_abstract_string(str::String)::AbstractString
        return str
    end
    ")

    # Add functions to ensure a value is always a Vector (handles JuliaCall scalar conversion)
    julia_eval("_to_julia_vec(x::AbstractVector) = x")
    julia_eval("_to_julia_vec(x) = [x]")

    # Add function to get field from Julia object
    julia_eval("
    function get_object_field(obj, field_name)
        return getfield(obj, Symbol(field_name))
    end
    ")

    julia_eval("
    function _construct_pairs(keys, values)
        result = []
        for i in 1:length(keys)
            key = keys[i]
            value = values[i]
            if isa(key, Vector) && length(key) > 1
                # Convert vector to tuple for matrix keys
                tuple_key = Tuple(key)
                push!(result, tuple_key => value)
            else
                push!(result, key => value)
            end
        end
        return result
    end")

    julia_eval("
    function _inefficient_action_handler(new_handler::AbnormalHandler)::AbnormalHandler
        old_handler = TanayLabUtilities.MatrixLayouts.GLOBAL_INEFFICIENT_ACTION_HANDLER
        TanayLabUtilities.MatrixLayouts.GLOBAL_INEFFICIENT_ACTION_HANDLER = new_handler
        return old_handler
    end
    ")

    # Add function to strip ReadOnly and NamedArray wrappers
    julia_eval("
    function _strip_wrappers(array::Union{ReadOnlyArray, NamedArrays.NamedArray})::AbstractArray
        array = parent(array)
        return _strip_wrappers(array)
    end
    ")

    julia_eval("
    function _strip_wrappers(array::AbstractArray)::AbstractArray
        return array
    end
    ")

    julia_eval("
    function _classify_object(obj)
        obj isa AbstractArray && return \"array\"
        obj isa Base.KeySet && return \"keyset\"
        obj isa AbstractSet && return \"set\"
        obj isa DataFrames.AbstractDataFrame && return \"dataframe\"
        obj isa AbstractString && return \"string\"
        obj isa Bool && return \"bool\"
        obj isa Number && return \"number\"
        return \"unknown\"
    end
    ")

    # Single Julia-side call that extracts all metadata needed by from_julia_array.
    # This replaces 6+ separate julia_call round-trips with one call, eliminating
    # per-call JIT overhead on cold start (~0.5s savings).
    # Returns: (stripped_array, eltype_str, is_sparse_csc, is_sparse_abstract,
    #           is_named_vec, is_named_mat, names1, names2)
    julia_eval("
    function _prepare_for_r(array)
        stripped = _strip_wrappers(array)
        eltype_str = string(eltype(stripped))
        is_sparse_csc = stripped isa SparseArrays.SparseMatrixCSC
        is_sparse_abstract = stripped isa SparseArrays.AbstractSparseArray
        is_named_vec = array isa NamedArrays.NamedVector
        is_named_mat = array isa NamedArrays.NamedMatrix
        names1 = is_named_vec || is_named_mat ? collect(string.(NamedArrays.names(array, 1))) : String[]
        names2 = is_named_mat ? collect(string.(NamedArrays.names(array, 2))) : String[]
        return (stripped, eltype_str, is_sparse_csc, is_sparse_abstract,
                is_named_vec, is_named_mat, names1, names2)
    end
    ")
}

get_julia_field <- function(julia_object, field_name, need_return = "Julia") {
    return(julia_call("get_object_field", julia_object, field_name, need_return = need_return))
}

is_julia_type <- function(julia_object, type_name, need_return = "R") {
    return(julia_call("isa", julia_object, julia_eval(type_name), need_return = need_return))
}

jl_pair <- function(x) {
    pair <- list()
    for (i in seq_along(x)) {
        pair[[i]] <- list(names(x)[i], x[[i]])
    }
    return(pair)
}

jl_pairify_axes <- function(axes) {
    axes <- jl_pair(axes)
    return(julia_call("_pairify_axes", axes, need_return = "Julia"))
}

jl_pairify_data <- function(data) {
    data_list <- list()
    for (i in seq_along(data)) {
        key_name <- names(data)[i]

        # Handle matrix tuple keys that use comma notation (e.g., "var,obs,X")
        if (grepl(",", key_name)) {
            parts <- strsplit(key_name, ",")[[1]]
            parts <- trimws(parts)
            data_list <- c(data_list, list(list(parts, data[[i]])))
        } else {
            data_list <- c(data_list, list(list(key_name, data[[i]])))
        }
    }
    return(julia_call("_pairify_data", data_list, need_return = "Julia"))
}


#' Initialize the Julia type cache
#'
#' Builds the R-to-Julia type mapping and stores it in dafr_env$JULIA_TYPE_OF_R_TYPE.
#' Should be called once during setup, after Julia is initialized.
#'
#' @return No return value, called for side effects.
#' @noRd
init_julia_type_cache <- function() {
    dafr_env$JULIA_TYPE_OF_R_TYPE <- list(
        "logical" = julia_eval("Bool"),
        "integer" = julia_eval("Int64"),
        "double" = julia_eval("Float64"),
        "int8" = julia_eval("Int8"),
        "int16" = julia_eval("Int16"),
        "int32" = julia_eval("Int32"),
        "int64" = julia_eval("Int64"),
        "uint8" = julia_eval("UInt8"),
        "uint16" = julia_eval("UInt16"),
        "uint32" = julia_eval("UInt32"),
        "uint64" = julia_eval("UInt64"),
        "float32" = julia_eval("Float32"),
        "float64" = julia_eval("Float64")
    )
    dafr_env$JULIA_NOTHING_TYPE <- julia_eval("Nothing")
}

#' Convert R types to Julia types
#'
#' This function converts R types or values to their appropriate Julia type equivalents.
#' It maps R data types to Julia data types using a cached lookup table.
#'
#' @param value An R value or type to convert to a Julia type
#' @return The equivalent Julia type or the value unchanged if no conversion is needed
#' @noRd
jl_R_to_julia_type <- function(value) {
    type_map <- dafr_env$JULIA_TYPE_OF_R_TYPE

    # If value is a string that represents a type name
    if (is.character(value) && length(value) == 1) {
        if (value %in% names(type_map)) {
            return(type_map[[value]])
        }
    }

    # Determine the type of the actual value
    if (!is.null(value) && !is.function(value)) {
        r_type <- typeof(value)
        if (r_type %in% names(type_map)) {
            return(type_map[[r_type]])
        }
    }

    if (is.null(value)) {
        return(dafr_env$JULIA_NOTHING_TYPE)
    }

    # Return the value unchanged if no conversion is needed or possible
    return(julia_eval(value, need_return = "Julia"))
}

#' Convert a Julia object to an appropriate R object
#'
#' @param julia_object A Julia object
#' @return An R object of appropriate type
#' @noRd
from_julia_object <- function(julia_object) {
    if (is.null(julia_object)) {
        return(NULL)
    }

    if (!inherits(julia_object, "JuliaObject")) {
        return(julia_object)
    }

    obj_type <- julia_call("_classify_object", julia_object, need_return = "R")

    switch(obj_type,
        "array" = from_julia_array(julia_object),
        "keyset" = ,
        "set" = as.character(julia_call("collect", julia_object)),
        "dataframe" = as.data.frame(julia_object),
        "string" = as.character(julia_object),
        "bool" = as.logical(julia_object),
        "number" = as.numeric(julia_object),
        "unknown" = julia_eval(julia_object, need_return = "R")
    )
}

create_julia_sparse_matrix <- function(sparse_matrix) {
    if (!inherits(sparse_matrix, "dgCMatrix")) {
        sparse_matrix <- as(sparse_matrix, "CsparseMatrix")
    }

    # Extract and convert to 1-indexed integers for Julia
    colptr <- as.integer(sparse_matrix@p + 1L)
    rowval <- as.integer(sparse_matrix@i + 1L)
    nzval <- sparse_matrix@x

    nrows <- sparse_matrix@Dim[1]
    ncols <- sparse_matrix@Dim[2]

    # Create Julia sparse matrix directly from R vectors
    return(julia_call(
        "SparseArrays.SparseMatrixCSC",
        as.integer(nrows), as.integer(ncols),
        colptr, rowval, nzval,
        need_return = "Julia"
    ))
}


#' Convert an R vector to a Julia Vector, safely handling single-element vectors
#'
#' JuliaCall converts single-element R vectors to Julia scalars. This function
#' ensures the result is always a Julia Vector by using a Julia helper function.
#'
#' @param value An R vector (integer or numeric)
#' @return A Julia Vector object
#' @noRd
to_julia_vector <- function(value) {
    julia_call("_to_julia_vec", value, need_return = "Julia")
}

#' Convert R arrays, vectors and sparse matrices to Julia
#'
#' @param value An R object to convert to a Julia array
#' @return A Julia array or the value unchanged if no conversion is needed
#' @noRd
to_julia_array <- function(value) {
    # Handle strings
    if (is.character(value) && length(value) == 1) {
        return(julia_call("_to_abstract_string", value))
    }

    # Handle sparse matrices
    if (inherits(value, "sparseMatrix")) {
        return(create_julia_sparse_matrix(value))
    }

    # Handle vectors and convert to appropriate type
    if (is.vector(value) && !is.list(value)) {
        return(julia_call("Vector", value))
    }

    # Handle matrices and arrays
    if (is.matrix(value) || is.array(value)) {
        return(julia_call("Array", value))
    }

    # Return value unchanged if no conversion needed
    return(value)
}

#' Convert Julia arrays to R arrays, vectors or sparse matrices
#'
#' @param julia_array A Julia array object
#' @return An R array, vector or sparse matrix
#' @noRd
from_julia_array <- function(julia_array) {
    # Use julia_assign + julia_command + julia_eval to avoid the docall overhead.
    # julia_call goes through docall → rcopy(Vector{Any}, args) → sexp which has
    # significant first-call JIT. julia_command/julia_eval bypass docall entirely.
    JuliaCall::julia_assign("_fja_in", julia_array)
    JuliaCall::julia_command("_fja_p = _prepare_for_r(_fja_in)")

    # Extract metadata via julia_eval (returns plain R types, no docall)
    eltype_str <- JuliaCall::julia_eval("_fja_p[2]")
    is_sparse_csc <- JuliaCall::julia_eval("_fja_p[3]")
    is_sparse_abstract <- JuliaCall::julia_eval("_fja_p[4]")
    is_named_vec <- JuliaCall::julia_eval("_fja_p[5]")
    is_named_mat <- JuliaCall::julia_eval("_fja_p[6]")
    names1 <- JuliaCall::julia_eval("_fja_p[7]")
    names2 <- JuliaCall::julia_eval("_fja_p[8]")
    # Stripped array stays in Julia for jlview zero-copy
    stripped <- JuliaCall::julia_eval("JuliaCall.JuliaObject(_fja_p[1])")

    # Sparse CSC matrix path
    if (isTRUE(is_sparse_csc)) {
        sparse_zero_copy_types <- c("Float64", "Float32", "Int64", "Int32", "Int16", "UInt8", "UInt16", "UInt32", "UInt64")

        if (eltype_str %in% sparse_zero_copy_types) {
            sp <- jlview::jlview_sparse(stripped)
        } else {
            # Fallback: copy-based sparse conversion for unsupported types (e.g. Bool)
            colptr <- get_julia_field(stripped, "colptr", need_return = "R")
            rowval <- get_julia_field(stripped, "rowval", need_return = "R")
            nzval <- get_julia_field(stripped, "nzval", need_return = "R")
            colptr <- colptr - 1
            dims <- julia_call("size", stripped)
            dims <- do.call(c, dims)

            if (eltype_str == "Bool") {
                sp <- Matrix::sparseMatrix(i = rowval, p = colptr, x = as.logical(nzval), dims = dims, repr = "C")
            } else {
                sp <- Matrix::sparseMatrix(i = rowval, p = colptr, x = as.numeric(nzval), dims = dims, repr = "C")
            }
        }

        if (isTRUE(is_named_mat) && length(names1) > 0 && length(names2) > 0) {
            dimnames(sp) <- list(names1, names2)
        }
        return(sp)
    }

    # Dense zero-copy path via jlview
    zero_copy_types <- c("Float64", "Float32", "Int64", "Int32", "Int16", "UInt8", "UInt16", "UInt32", "UInt64")

    if (eltype_str %in% zero_copy_types && !isTRUE(is_sparse_abstract)) {
        if (isTRUE(is_named_vec) && length(names1) > 0) {
            r_array <- jlview::jlview(julia_array, names = names1)
            if (!is.null(dim(r_array))) r_array <- as.vector(r_array)
            return(r_array)
        }

        if (isTRUE(is_named_mat) && length(names1) > 0 && length(names2) > 0) {
            r_array <- jlview::jlview(julia_array, dimnames = list(names1, names2))
            return(r_array)
        }

        # Non-named arrays
        r_array <- jlview::jlview(julia_array)
        if (!is.null(dim(r_array)) && length(dim(r_array)) == 1) {
            r_array <- as.vector(r_array)
        }
        return(r_array)
    }

    # Fallback: Bool, String, unsupported types - copy via collect
    if (isTRUE(is_named_vec)) {
        r_array <- as.vector(julia_call("collect", stripped, need_return = "R"))
        if (length(names1) > 0) names(r_array) <- names1
        return(r_array)
    }

    if (isTRUE(is_named_mat)) {
        r_array <- as.matrix(julia_call("collect", stripped, need_return = "R"))
        if (length(names1) > 0 && length(names2) > 0) {
            rownames(r_array) <- names1
            colnames(r_array) <- names2
        }
        return(r_array)
    }

    # Handle regular arrays
    r_array <- julia_call("collect", stripped, need_return = "R")
    if (is.vector(r_array) || length(dim(r_array)) == 1 || (length(dim(r_array)) == 2 && any(dim(r_array) == 1))) {
        r_array <- as.vector(r_array)
    }
    return(r_array)
}

# Add function to process frame columns for get_frame
#' Process column specifications for get_frame
#'
#' This function transforms R column specifications (lists, named vectors, etc.)
#' into a format suitable for Julia's DataAxesFormats.Queries.get_frame function.
#'
#' @param columns List or vector of column specifications
#' @return A Julia array of pairs suitable for DataAxesFormats.Queries.get_frame
#' @noRd
process_frame_columns <- function(columns) {
    # Return NULL for NULL input
    if (is.null(columns)) {
        return(NULL)
    }

    # If columns is a single string, convert it to a list with one element
    if (is.character(columns) && length(columns) == 1 && is.null(names(columns))) {
        columns <- list(columns)
    }

    processed_columns <- list()

    for (i in seq_along(columns)) {
        item <- columns[[i]]

        # If the item is a named list/vector, convert it to a pair
        if (is.list(item) && !is.null(names(item))) {
            for (j in seq_along(item)) {
                processed_columns <- c(processed_columns, list(list(names(item)[j], item[[j]])))
            }
        } else if (!is.null(names(columns)) && names(columns)[i] != "") {
            # If the columns list itself has names, use those
            processed_columns <- c(processed_columns, list(list(names(columns)[i], item)))
        } else {
            # Pass through simple items
            processed_columns <- c(processed_columns, list(item))
        }
    }

    # Convert to Julia format
    return(julia_call("_pairify_columns", processed_columns, need_return = "Julia"))
}
