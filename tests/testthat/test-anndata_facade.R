test_that("as_anndata creates a DafAnnData from example data", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    expect_s3_class(adata, "DafAnnData")
})

test_that("DafAnnData $X returns correct matrix", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    x <- adata$X
    expect_true(is.matrix(x) || inherits(x, "sparseMatrix"))
    expect_equal(nrow(x), axis_length(daf, "cell"))
    expect_equal(ncol(x), axis_length(daf, "gene"))
})

test_that("DafAnnData $obs returns data frame", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    obs <- adata$obs
    expect_true(is.data.frame(obs))
    expect_equal(nrow(obs), axis_length(daf, "cell"))
})

test_that("DafAnnData $var returns data frame", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    var_df <- adata$var
    expect_true(is.data.frame(var_df))
    expect_equal(nrow(var_df), axis_length(daf, "gene"))
})

test_that("DafAnnData $obs_names and $var_names work", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    expect_equal(length(adata$obs_names), axis_length(daf, "cell"))
    expect_equal(length(adata$var_names), axis_length(daf, "gene"))
})

test_that("DafAnnData $n_obs and $n_vars work", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    expect_equal(adata$n_obs, axis_length(daf, "cell"))
    expect_equal(adata$n_vars, axis_length(daf, "gene"))
})

test_that("DafAnnData $shape returns c(n_obs, n_vars)", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    expect_equal(adata$shape, c(adata$n_obs, adata$n_vars))
})

test_that("DafAnnData $uns returns scalars", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    uns <- adata$uns
    expect_true(is.list(uns))
})

test_that("DafAnnData is read-only", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    expect_error(adata$X <- matrix(0, 1, 1))
})

test_that("DafAnnData auto-detects axes", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf, x_name = "UMIs")
    expect_equal(adata$obs_axis, "cell")
    expect_equal(adata$var_axis, "gene")
})

test_that("DafAnnData print works", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    expect_output(print(adata), "DafAnnData")
})

test_that("DafAnnData rejects a missing x_name at construction", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    expect_error(
        as_anndata(daf, obs_axis = "cell", var_axis = "gene", x_name = "Nope"),
        "is not available"
    )
})

test_that("DafAnnData rejects unknown axes at construction", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    expect_error(
        as_anndata(daf, obs_axis = "notanaxis", var_axis = "gene", x_name = "UMIs"),
        "is not one of the Daf axes"
    )
    expect_error(
        as_anndata(daf, obs_axis = "cell", var_axis = "notanaxis", x_name = "UMIs"),
        "is not one of the Daf axes"
    )
})

test_that("DafAnnData auto-detect fails cleanly when axes are absent", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf()
    add_axis(daf, "row", c("r1", "r2"))
    add_axis(daf, "col", c("c1", "c2", "c3"))
    expect_error(as_anndata(daf), "Cannot auto-detect obs_axis")
    expect_error(as_anndata(daf, obs_axis = "row"), "Cannot auto-detect var_axis")
})

test_that("DafAnnData $obs and $obs_names agree on row names", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- example_cells_daf()
    adata <- as_anndata(daf)
    expect_equal(rownames(adata$obs), adata$obs_names)
})
