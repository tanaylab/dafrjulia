#' Complete chain of Daf repositories
#'
#' Open a complete chain of Daf repositories by tracing back through the `base_daf_repository` property.
#' Each repository in a chain contains a scalar property called `base_daf_repository` which identifies
#' its parent repository (if any).
#'
#' @param leaf Path to the leaf repository, which will be traced back through its ancestors
#' @param mode Mode to open the repositories ("r" for read-only, "r+" for read-write)
#' @param name Optional name for the complete Daf object
#' @return A Daf object combining the leaf repository with all its ancestors
#' @details If mode is "r+", only the first (leaf) repository is opened in write mode.
#'   The `base_daf_repository` path is relative to the directory containing the child repository.
#'
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/complete.html) for details.
#' @export
complete_daf <- function(leaf, mode = "r", name = NULL) {
    # Validate mode parameter
    if (!mode %in% c("r", "r+")) {
        cli::cli_abort("Mode must be one of 'r' or 'r+'")
    }

    # Normalize the path to resolve symlinks (e.g. /var -> /private/var on macOS).
    # This matches how files_daf() normalizes paths, ensuring the ispath cache
    # uses consistent keys across different Daf handles on the same directory.
    leaf <- normalizePath(leaf, mustWork = FALSE)

    # Call the Julia implementation directly
    jl_obj <- julia_call("DataAxesFormats.complete_daf", leaf, mode, name = name)

    return(Daf(jl_obj))
}

#' Create a persistent chain from a base Daf and a new Daf
#'
#' Immediately after creating an empty disk-based `new_daf`, chain it with a disk-based
#' `base_daf` and return the new chain. If `axes` and/or `data` are specified, the `new_daf`
#' will be chained on top of a view of the `base_daf`.
#'
#' This will set the `base_daf_repository` scalar property of the `new_daf` to point at the
#' `base_daf`, and if view `axes` or `data` were specified, the `base_daf_view` as well.
#' It should therefore be possible to recreate the chain by calling `complete_daf` in the future.
#'
#' @param base_daf A Daf object to use as the base (read-only) data
#' @param new_daf A Daf object to use as the new (writable) data on top of the base
#' @param name Optional name for the chained Daf object
#' @param axes Optional named list specifying axes to expose from the base (same format as `viewer`)
#' @param data Optional named list specifying data to expose from the base (same format as `viewer`)
#' @param absolute If TRUE, store the absolute path to the base_daf. If FALSE (default),
#'   store a relative path for portability.
#' @return A writable Daf object chaining the base and new data
#' @details By default, the stored base path in the `new_daf` will be the relative path to the
#'   `base_daf`, for the common case where a group of repositories is stored under a common root.
#'   This allows the root to be renamed or moved and still allow `complete_daf` to work.
#'
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/chains.html#DataAxesFormats.Chains.complete_chain!) for details.
#' @export
complete_chain <- function(base_daf, new_daf, name = NULL, axes = NULL, data = NULL, absolute = FALSE) {
    validate_daf_object(base_daf)
    validate_daf_object(new_daf)

    # Process axes
    if (!is.null(axes)) {
        axes <- jl_pairify_axes(axes)
    }

    # Process data
    if (!is.null(data)) {
        data <- jl_pairify_data(data)
    }

    # Call the Julia implementation
    jl_obj <- julia_call("DataAxesFormats.complete_chain!",
        base_daf = base_daf$jl_obj,
        new_daf = new_daf$jl_obj,
        name = name,
        axes = axes,
        data = data,
        absolute = absolute,
        need_return = "Julia"
    )

    return(Daf(jl_obj))
}

#' Open a Daf repository based on path
#'
#' This function determines whether to open a files-based Daf or an HDF5-based Daf
#' based on the file path.
#'
#' @param path Path to the Daf repository
#' @param mode Mode to open the storage ("r" for read-only, "r+" for read-write)
#' @param name Optional name for the Daf object
#' @return A Daf object (either files_daf or h5df)
#' @details If the path ends with `.h5df` or contains `.h5dfs#` (followed by a group path),
#'   then it opens an HDF5 file (or a group in one). Otherwise, it opens a files-based Daf.
#'
#'   As a shorthand, you can specify a path to a group within an HDF5 file by using a path
#'   with a `.h5dfs` suffix, followed by `#` and the path of the group in the file.
#'
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/complete.html) for details.
#' @export
open_daf <- function(path, mode = "r", name = NULL) {
    # Determine the type of Daf repository based on the path
    if (grepl("\\.h5df$", path) || grepl("\\.h5dfs#", path)) {
        # HDF5-based Daf
        return(h5df(path, mode, name = name))
    } else {
        # Files-based Daf
        return(files_daf(path, mode, name = name))
    }
}
