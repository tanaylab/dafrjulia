#' Create a Daf object with in-memory storage
#'
#' This function creates a Daf object that stores data in memory. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/memory_format.html) for details.
#'
#' @param name The name of the Daf object (default: "memory")
#' @return A Daf object with in-memory storage
#' @examples
#' \dontrun{
#' setup_daf()
#' daf <- memory_daf("example")
#' add_axis(daf, "cell", c("A", "B", "C"))
#' set_vector(daf, "cell", "score", c(1.0, 2.0, 3.0))
#' }
#' @export
memory_daf <- function(name = "memory") {
    jl_obj <- julia_call("DataAxesFormats.MemoryDaf", name = name)
    return(Daf(jl_obj))
}

#' Create a Daf object with file-based storage
#'
#' This function creates a Daf object that stores data in disk files. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/files_format.html) for details.
#'
#' @param path Path to the files storage location
#' @param mode Mode to open the storage ("r" for read-only, "r+" for read-write)
#' @param name Optional name for the Daf object
#' @param packed If TRUE, store arrays chunked and compressed (DataAxesFormats v0.3.0+).
#' @return A Daf object with file-based storage
#' @examples
#' \dontrun{
#' setup_daf()
#' daf <- files_daf(tempdir(), "w", name = "example")
#' add_axis(daf, "gene", c("X", "Y", "Z"))
#' }
#' @export
files_daf <- function(path, mode = "r", name = NULL, packed = FALSE) {
    path <- normalizePath(path, mustWork = FALSE)
    # Work around a DataAxesFormats v0.2.0 cache invalidation bug when opening
    # existing directories in truncate mode ("w").
    if (identical(mode, "w") && dir.exists(path)) {
        unlink(path, recursive = TRUE, force = TRUE)
    }
    jl_obj <- julia_call("DataAxesFormats.FilesDaf", path, mode, name = name, packed = packed)
    return(Daf(jl_obj))
}

#' Create a Daf object with HDF5-based storage
#'
#' This function creates a Daf object that stores data in an HDF5 disk file. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/h5df_format.html) for details.
#'
#' @param root Path to the HDF5 file, or a Julia HDF5 File or Group object
#' @param mode Mode to open the storage ("r" for read-only, "r+" for read-write)
#' @param name Optional name for the Daf object
#' @param packed If TRUE, store arrays chunked and compressed (DataAxesFormats v0.3.0+).
#' @return A Daf object with HDF5-based storage
#' @examples
#' \dontrun{
#' setup_daf()
#' h5_path <- file.path(tempdir(), "example.h5")
#' daf <- h5df(h5_path, "w", name = "example")
#' }
#' @export
h5df <- function(root, mode = "r", name = NULL, packed = FALSE) {
    root <- normalizePath(root, mustWork = FALSE)
    jl_obj <- julia_call("DataAxesFormats.H5df", root, mode, name = name, packed = packed)
    return(Daf(jl_obj))
}

#' Create a Daf object with Zarr-based storage
#'
#' Stores data using the Zarr format (a directory, a `.daf.zarr.zip` archive, or a
#' remote `http(s)://` Zarr). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/zarr_format.html) for details.
#'
#' @param path Path to the Zarr storage (`.daf.zarr`, `.daf.zarr.zip`, `.dafs.zarr.zip#/<group>`, or an `http(s)://` URL)
#' @param mode Mode to open the storage ("r", "r+", "w", or "w+")
#' @param name Optional name for the Daf object
#' @param packed If TRUE, store arrays chunked and compressed
#' @return A Daf object with Zarr-based storage
#' @export
zarr_daf <- function(path, mode = "r", name = NULL, packed = FALSE) {
    if (!grepl("^https?://", path)) {
        path <- normalizePath(path, mustWork = FALSE)
    }
    jl_obj <- julia_call("DataAxesFormats.ZarrDaf", path, mode, name = name, packed = packed)
    return(Daf(jl_obj))
}

#' Create a Daf object with ZIP-archive storage
#'
#' Stores data in a single append-only `.daf.zip` archive. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/zip_files.html) for details.
#'
#' @param path Path to the ZIP archive (`.daf.zip` or `.dafs.zip#/<group>`)
#' @param mode Mode to open the storage ("r", "r+", "w", or "w+")
#' @param name Optional name for the Daf object
#' @param packed If TRUE, store arrays chunked and compressed
#' @return A Daf object with ZIP-archive storage
#' @export
zip_daf <- function(path, mode = "r", name = NULL, packed = FALSE) {
    path <- normalizePath(path, mustWork = FALSE)
    jl_obj <- julia_call("DataAxesFormats.ZipDaf", path, mode, name = name, packed = packed)
    return(Daf(jl_obj))
}

#' Create a read-only Daf object served over HTTP(S)
#'
#' Opens a Daf data set served over `http://` or `https://`. Read-only; there is no
#' `mode` argument. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.3.0/http_format.html) for details.
#'
#' @param url The `http(s)://` URL of the served Daf data set
#' @param name Optional name for the Daf object
#' @param packed Ignored for HTTP (kept for signature symmetry with other backends)
#' @return A read-only Daf object
#' @export
http_daf <- function(url, name = NULL, packed = FALSE) {
    if (!grepl("^https?://", url)) {
        cli::cli_abort("{.arg url} must be an http:// or https:// URL, got {.val {url}}")
    }
    jl_obj <- julia_call("DataAxesFormats.HttpDaf", url, name = name, packed = packed)
    return(Daf(jl_obj))
}

#' Create a read-only chain wrapper of DafReader objects
#'
#' This function creates a read-only chain wrapper of DafReader objects, presenting them as a single DafReader.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/chains.html#DataAxesFormats.Chains.chain_reader) for details.
#'
#' @param dsets List of Daf objects to chain
#' @param name Optional name for the chained Daf object
#' @return A read-only Daf object chaining the input Daf objects
#' @export
chain_reader <- function(dsets, name = NULL) {
    jl_dsets <- lapply(dsets, function(dset) dset$jl_obj)
    jl_obj <- julia_call("DataAxesFormats.chain_reader",
        julia_call("_to_daf_readers", jl_dsets),
        name = name
    )
    return(Daf(jl_obj))
}

#' Create a writable chain wrapper of DafReader objects
#'
#' This function creates a chain wrapper for a chain of DafReader data, presenting them as a single DafWriter.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/chains.html#DataAxesFormats.Chains.chain_writer) for details.
#'
#' @param dsets List of Daf objects to chain
#' @param name Optional name for the chained Daf object
#' @return A writable Daf object chaining the input Daf objects
#' @export
chain_writer <- function(dsets, name = NULL) {
    jl_dsets <- lapply(dsets, function(dset) dset$jl_obj)
    jl_obj <- julia_call("DataAxesFormats.chain_writer",
        julia_call("_to_daf_readers", jl_dsets),
        name = name
    )
    return(Daf(jl_obj))
}
