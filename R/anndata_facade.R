#' AnnData-like live facade for a Daf object
#'
#' Wraps a Daf object and provides AnnData-compatible accessors
#' (\code{$X}, \code{$obs}, \code{$var}, \code{$layers}, \code{$uns}).
#' This is a read-only facade: reads are lazy and go through to the
#' underlying Daf object on demand, benefiting from dafJuliaWrapper's
#' R-side caching.
#'
#' The facade maps AnnData concepts to Daf data:
#' \itemize{
#'   \item \code{$X} - the primary matrix (\code{obs_axis} x \code{var_axis}, named \code{x_name})
#'   \item \code{$obs} - data frame of observation (\code{obs_axis}) vectors
#'   \item \code{$var} - data frame of variable (\code{var_axis}) vectors
#'   \item \code{$layers} - named list of additional matrices (excluding \code{$X})
#'   \item \code{$uns} - named list of scalars
#'   \item \code{$obs_names} - character vector of observation names
#'   \item \code{$var_names} - character vector of variable names
#'   \item \code{$n_obs} - number of observations
#'   \item \code{$n_vars} - number of variables
#'   \item \code{$shape} - \code{c(n_obs, n_vars)}
#' }
#'
#' @return An R6 object of class \code{DafAnnData}.
#' @export
DafAnnData <- R6::R6Class("DafAnnData",
    public = list(
        #' @field daf The underlying Daf object
        daf = NULL,
        #' @field obs_axis Name of the observations axis
        obs_axis = NULL,
        #' @field var_axis Name of the variables axis
        var_axis = NULL,
        #' @field x_name Name of the primary matrix
        x_name = NULL,

        #' @description Create a new DafAnnData facade.
        #' @param daf A Daf object
        #' @param obs_axis Observations axis name. If \code{NULL}, auto-detects
        #'   \code{"cell"} or \code{"metacell"}.
        #' @param var_axis Variables axis name. If \code{NULL}, auto-detects \code{"gene"}.
        #' @param x_name Primary matrix name. If \code{NULL}, the first matrix available on
        #'   \code{(obs_axis, var_axis)} is used; defaults to \code{"UMIs"} when present.
        initialize = function(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs") {
            if (!is_daf(daf)) {
                cli::cli_abort("Expected a Daf object")
            }
            self$daf <- daf
            axes <- axes_set(daf)
            if (is.null(obs_axis)) {
                if ("cell" %in% axes) {
                    obs_axis <- "cell"
                } else if ("metacell" %in% axes) {
                    obs_axis <- "metacell"
                } else {
                    cli::cli_abort("Cannot auto-detect obs_axis. Available axes: {paste(axes, collapse = ', ')}. Specify obs_axis explicitly.")
                }
            } else if (!(obs_axis %in% axes)) {
                cli::cli_abort("obs_axis {.val {obs_axis}} is not one of the Daf axes: {paste(axes, collapse = ', ')}")
            }
            if (is.null(var_axis)) {
                if ("gene" %in% axes) {
                    var_axis <- "gene"
                } else {
                    cli::cli_abort("Cannot auto-detect var_axis. Available axes: {paste(axes, collapse = ', ')}. Specify var_axis explicitly.")
                }
            } else if (!(var_axis %in% axes)) {
                cli::cli_abort("var_axis {.val {var_axis}} is not one of the Daf axes: {paste(axes, collapse = ', ')}")
            }
            self$obs_axis <- obs_axis
            self$var_axis <- var_axis

            # Resolve x_name: prefer explicit, else "UMIs" if present, else
            # first available matrix, else error listing what's available.
            available <- matrices_set(daf, obs_axis, var_axis)
            if (is.null(x_name)) {
                if (length(available) == 0) {
                    cli::cli_abort("No matrices available on ({obs_axis}, {var_axis}); cannot choose an X matrix.")
                }
                x_name <- if ("UMIs" %in% available) "UMIs" else available[1]
            } else if (!(x_name %in% available)) {
                cli::cli_abort(
                    "x_name {.val {x_name}} is not available on ({obs_axis}, {var_axis}). Available: {paste(available, collapse = ', ')}"
                )
            }
            self$x_name <- x_name
        },

        #' @description Print a concise summary. Prints dimensions and axis
        #'   names only; call \code{summary()} for the full listing of
        #'   obs/var/layer names.
        #' @param ... Ignored.
        print = function(...) {
            cat(sprintf("DafAnnData: %d obs x %d vars\n", self$n_obs, self$n_vars))
            cat(sprintf("  obs_axis='%s'  var_axis='%s'  X='%s'\n",
                        self$obs_axis, self$var_axis, self$x_name))
            invisible(self)
        },

        #' @description Full listing of obs vectors, var vectors, and layer
        #'   names. Each access is a bridge call; kept out of
        #'   \code{print()} to keep auto-print cheap.
        #' @param object Present for S3 generic compatibility.
        #' @param ... Ignored.
        summary = function(object, ...) {
            cat(sprintf("DafAnnData: %d obs x %d vars\n", self$n_obs, self$n_vars))
            cat(sprintf("  obs_axis='%s'  var_axis='%s'  X='%s'\n",
                        self$obs_axis, self$var_axis, self$x_name))
            obs_vecs <- vectors_set(self$daf, self$obs_axis)
            var_vecs <- vectors_set(self$daf, self$var_axis)
            layer_names <- private$get_layer_names()
            if (length(obs_vecs) > 0) cat(sprintf("  obs: %s\n", paste(obs_vecs, collapse = ", ")))
            if (length(var_vecs) > 0) cat(sprintf("  var: %s\n", paste(var_vecs, collapse = ", ")))
            if (length(layer_names) > 0) cat(sprintf("  layers: %s\n", paste(layer_names, collapse = ", ")))
            invisible(self)
        }
    ),
    active = list(
        #' @field X The primary matrix (obs x var)
        X = function(value) {
            if (!missing(value)) {
                cli::cli_abort("DafAnnData facade is read-only. Use the underlying Daf object to modify data.")
            }
            get_matrix(self$daf, self$obs_axis, self$var_axis, self$x_name)
        },

        #' @field obs Data frame of observation vectors
        obs = function(value) {
            if (!missing(value)) {
                cli::cli_abort("DafAnnData facade is read-only. Use the underlying Daf object to modify data.")
            }
            get_dataframe(self$daf, self$obs_axis)
        },

        #' @field var Data frame of variable vectors
        var = function(value) {
            if (!missing(value)) {
                cli::cli_abort("DafAnnData facade is read-only. Use the underlying Daf object to modify data.")
            }
            get_dataframe(self$daf, self$var_axis)
        },

        #' @field layers Named list of additional matrices (excluding X)
        layers = function(value) {
            if (!missing(value)) {
                cli::cli_abort("DafAnnData facade is read-only. Use the underlying Daf object to modify data.")
            }
            layer_names <- private$get_layer_names()
            result <- list()
            for (nm in layer_names) {
                result[[nm]] <- get_matrix(self$daf, self$obs_axis, self$var_axis, nm)
            }
            result
        },

        #' @field uns Named list of scalars
        uns = function(value) {
            if (!missing(value)) {
                cli::cli_abort("DafAnnData facade is read-only. Use the underlying Daf object to modify data.")
            }
            scalar_names <- scalars_set(self$daf)
            result <- list()
            for (nm in scalar_names) {
                result[[nm]] <- get_scalar(self$daf, nm)
            }
            result
        },

        #' @field obs_names Character vector of observation names
        obs_names = function(value) {
            if (!missing(value)) {
                cli::cli_abort("DafAnnData facade is read-only.")
            }
            axis_vector(self$daf, self$obs_axis)
        },

        #' @field var_names Character vector of variable names
        var_names = function(value) {
            if (!missing(value)) {
                cli::cli_abort("DafAnnData facade is read-only.")
            }
            axis_vector(self$daf, self$var_axis)
        },

        #' @field n_obs Number of observations
        n_obs = function() {
            axis_length(self$daf, self$obs_axis)
        },

        #' @field n_vars Number of variables
        n_vars = function() {
            axis_length(self$daf, self$var_axis)
        },

        #' @field shape Dimensions c(n_obs, n_vars)
        shape = function() {
            c(self$n_obs, self$n_vars)
        }
    ),
    private = list(
        get_layer_names = function() {
            all_mats <- matrices_set(self$daf, self$obs_axis, self$var_axis)
            setdiff(all_mats, self$x_name)
        }
    )
)

#' Create an AnnData-like facade for a Daf object
#'
#' Creates a live, read-only facade over a Daf object that provides AnnData-compatible
#' accessors (\code{$X}, \code{$obs}, \code{$var}, \code{$layers}, \code{$uns}).
#' No data is copied at construction; reads go through to the underlying Daf
#' object on demand, served by dafJuliaWrapper's R-side cache on subsequent calls.
#'
#' @param daf A Daf object
#' @param obs_axis Name of the observations axis. Auto-detected if NULL (tries "cell", then "metacell").
#' @param var_axis Name of the variables axis. Auto-detected if NULL (tries "gene").
#' @param x_name Name of the primary matrix property. If NULL, the first
#'   matrix on (obs_axis, var_axis) is used, preferring \code{"UMIs"}.
#'   Defaults to \code{"UMIs"}.
#' @return A \code{\link{DafAnnData}} R6 object
#' @seealso \code{\link{DafAnnData}}
#' @examples
#' \dontrun{
#' daf <- example_cells_daf()
#' adata <- as_anndata(daf)
#' adata$X # primary matrix
#' adata$obs # observation metadata
#' adata$var # variable metadata
#' adata$obs_names # observation names
#' adata$n_obs # number of observations
#' }
#' @importFrom R6 R6Class
#' @export
as_anndata <- function(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs") {
    DafAnnData$new(daf, obs_axis = obs_axis, var_axis = var_axis, x_name = x_name)
}
