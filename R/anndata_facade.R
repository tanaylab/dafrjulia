#' AnnData-like live facade for a Daf object
#'
#' Wraps a Daf object and provides AnnData-compatible accessors (`$X`, `$obs`,
#' `$var`, `$layers`, `$uns`). This is a zero-copy live facade: reads are
#' lazy and go through to the underlying Daf data on demand.
#'
#' @details
#' The facade maps AnnData concepts to Daf data:
#' \itemize{
#'   \item \code{$X} - the primary matrix (obs_axis x var_axis, named x_name)
#'   \item \code{$obs} - data frame of observation (obs_axis) vectors
#'   \item \code{$var} - data frame of variable (var_axis) vectors
#'   \item \code{$layers} - named list of additional matrices (obs_axis x var_axis)
#'   \item \code{$uns} - named list of scalars
#'   \item \code{$obs_names} - character vector of observation names
#'   \item \code{$var_names} - character vector of variable names
#'   \item \code{$n_obs} - number of observations
#'   \item \code{$n_vars} - number of variables
#'   \item \code{$shape} - c(n_obs, n_vars)
#' }
#'
#' All reads are lazy and cached via dafJuliaWrapper's caching system.
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

        #' @description Create a new DafAnnData facade
        #' @param daf A Daf object
        #' @param obs_axis Observations axis name (auto-detected if NULL)
        #' @param var_axis Variables axis name (auto-detected if NULL)
        #' @param x_name Primary matrix name (default "UMIs")
        initialize = function(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs") {
            if (!is_daf(daf)) {
                cli::cli_abort("Expected a Daf object")
            }
            self$daf <- daf
            # Auto-detect obs_axis
            if (is.null(obs_axis)) {
                axes <- axes_set(daf)
                if ("cell" %in% axes) {
                    obs_axis <- "cell"
                } else if ("metacell" %in% axes) {
                    obs_axis <- "metacell"
                } else {
                    cli::cli_abort("Cannot auto-detect obs_axis. Available axes: {paste(axes, collapse = ', ')}. Specify obs_axis explicitly.")
                }
            }
            # Auto-detect var_axis
            if (is.null(var_axis)) {
                axes <- axes_set(daf)
                if ("gene" %in% axes) {
                    var_axis <- "gene"
                } else {
                    cli::cli_abort("Cannot auto-detect var_axis. Available axes: {paste(axes, collapse = ', ')}. Specify var_axis explicitly.")
                }
            }
            self$obs_axis <- obs_axis
            self$var_axis <- var_axis
            self$x_name <- x_name
        },

        #' @description Print the DafAnnData object
        #' @param ... Ignored
        print = function(...) {
            cat(sprintf("DafAnnData object: %d obs x %d vars\n", self$n_obs, self$n_vars))
            cat(sprintf("  obs_axis: '%s', var_axis: '%s', X: '%s'\n", self$obs_axis, self$var_axis, self$x_name))
            obs_vecs <- vectors_set(self$daf, self$obs_axis)
            var_vecs <- vectors_set(self$daf, self$var_axis)
            if (length(obs_vecs) > 0) {
                cat(sprintf("  obs: %s\n", paste(obs_vecs, collapse = ", ")))
            }
            if (length(var_vecs) > 0) {
                cat(sprintf("  var: %s\n", paste(var_vecs, collapse = ", ")))
            }
            layer_names <- private$get_layer_names()
            if (length(layer_names) > 0) {
                cat(sprintf("  layers: %s\n", paste(layer_names, collapse = ", ")))
            }
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
#' No data is copied; reads go through to the underlying Daf object on demand.
#'
#' @param daf A Daf object
#' @param obs_axis Name of the observations axis. Auto-detected if NULL (tries "cell", then "metacell").
#' @param var_axis Name of the variables axis. Auto-detected if NULL (tries "gene").
#' @param x_name Name of the primary matrix property (default "UMIs")
#' @return A \code{\link{DafAnnData}} R6 object
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
