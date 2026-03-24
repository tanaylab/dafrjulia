expect_c_equal <- function(query, expected) {
    expect_equal(as.character(query), expected)
}

set <- function(x) {
    sort(unique(x))
}

# Test query formatting
test_that("query formatting works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_c_equal(Axis("cell"), "@ cell")
    expect_c_equal(Axis() |> Names(), "@ ?")
    expect_c_equal(LookupVector("version"), ": version")
    expect_c_equal(Axis("cell") |> LookupVector("batch") |> LookupVector("age"), "@ cell : batch : age")
    expect_c_equal(Axis("cell") |> LookupVector("manual") |> AsAxis("type") |> LookupVector("color"), "@ cell : manual =@ type : color")
    expect_c_equal(Axis("cell") |> LookupVector("batch") |> AsAxis() |> CountBy("manual") |> AsAxis("type"), "@ cell : batch =@ * manual =@ type")
    expect_c_equal(Axis("cell") |> LookupVector("age") |> GroupBy("batch") |> AsAxis() |> Mean(), "@ cell : age / batch =@ >> Mean")
    expect_c_equal(Axis("cell") |> LookupVector("batch") |> IfNot() |> LookupVector("age"), "@ cell : batch ?? : age")
    expect_c_equal(Axis("cell") |> LookupVector("type") |> IfNot("Outlier"), "@ cell : type ?? Outlier")
    expect_c_equal(Axis("cell") |> LookupVector("batch") |> IfNot(1) |> LookupVector("age"), "@ cell : batch ?? 1.0 : age")
    expect_c_equal(Axis("gene") |> LookupVector("is_marker") |> IfMissing(FALSE), "@ gene : is_marker || false")
    expect_c_equal(Axis("cell") |> LookupVector("type") |> IfMissing("red") |> LookupVector("color"), "@ cell : type || red : color")
    expect_c_equal(Axis("cell") |> LookupVector("type") |> IsEqual("LMPP") |> LookupVector("age") |> Max() |> IfMissing(as.integer(0)), "@ cell : type = LMPP : age >> Max || 0")
    expect_c_equal(Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs") |> Log(base = 2, eps = 1), "@ cell @ gene :: UMIs % Log base 2.0 eps 1.0")
    expect_c_equal(Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs") |> Sum(), "@ cell @ gene :: UMIs >> Sum")
    expect_c_equal(Axis("cell") |> LookupVector("age") |> CountBy("type"), "@ cell : age * type")
    expect_c_equal(Axis("cell") |> LookupVector("age") |> GroupBy("type") |> Mean(), "@ cell : age / type >> Mean")
    expect_c_equal(Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs") |> GroupBy("type") |> Max(), "@ cell @ gene :: UMIs / type >> Max")
    expect_c_equal(Axis("gene") |> BeginMask("is_marker") |> EndMask(), "@ gene [ is_marker ]")
    expect_c_equal(Axis("gene") |> BeginNegatedMask("is_marker") |> EndMask(), "@ gene [ ! is_marker ]")
    expect_c_equal(Axis("gene") |> BeginMask("is_marker") |> OrMask("is_noisy") |> EndMask(), "@ gene [ is_marker | is_noisy ]")
    expect_c_equal(Axis("gene") |> BeginMask("is_marker") |> OrNegatedMask("is_noisy") |> EndMask(), "@ gene [ is_marker | ! is_noisy ]")
    expect_c_equal(Axis("gene") |> BeginMask("is_marker") |> XorMask("is_noisy") |> EndMask(), "@ gene [ is_marker ^ is_noisy ]")
    expect_c_equal(Axis("gene") |> BeginMask("is_marker") |> XorNegatedMask("is_noisy") |> EndMask(), "@ gene [ is_marker ^ ! is_noisy ]")
    expect_c_equal(Axis("cell") |> Axis("gene") |> IsEqual("FOX1") |> LookupMatrix("UMIs"), "@ cell @ gene = FOX1 :: UMIs")
    expect_c_equal(Axis("cell") |> BeginMask("age") |> IsEqual(1) |> EndMask(), "@ cell [ age = 1.0 ]")
    expect_c_equal(Axis("cell") |> BeginMask("age") |> IsNotEqual(1) |> EndMask(), "@ cell [ age != 1.0 ]")
    expect_c_equal(Axis("cell") |> BeginMask("age") |> IsLess(1) |> EndMask(), "@ cell [ age < 1.0 ]")
    expect_c_equal(Axis("cell") |> BeginMask("age") |> IsLessEqual(1) |> EndMask(), "@ cell [ age <= 1.0 ]")
    expect_c_equal(Axis("cell") |> BeginMask("age") |> IsGreater(1) |> EndMask(), "@ cell [ age > 1.0 ]")
    expect_c_equal(Axis("cell") |> BeginMask("age") |> IsGreaterEqual(1) |> EndMask(), "@ cell [ age >= 1.0 ]")
    expect_c_equal(Axis("gene") |> BeginMask("name") |> IsMatch("RP[SL]") |> EndMask(), "@ gene [ name ~ RP\\[SL\\] ]")
    expect_c_equal(Axis("gene") |> BeginMask("name") |> IsNotMatch("RP[SL]") |> EndMask(), "@ gene [ name !~ RP\\[SL\\] ]")
})

test_that("IfMissing handles all parameter cases correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Setup a daf for testing
    daf <- memory_daf(name = "test_if_missing")
    set_scalar(daf, "version", "1.0")
    add_axis(daf, "gene", c("X", "Y", "Z"))

    # Create a query using Axis to be used as input
    gene_query <- Axis("gene")

    # Case 1: default_value is a Julia object, no dots
    expect_c_equal(
        IfMissing(gene_query, FALSE),
        "@ gene || false"
    )

    # Case 2: default_value explicit, query in dots
    expect_c_equal(
        IfMissing(FALSE, gene_query),
        "@ gene || false"
    )

    # Case 3: default_value explicit, no Julia object in dots
    expect_c_equal(
        IfMissing(FALSE),
        "|| false"
    )

    # Case 4: default_value is not a Julia object, no dots
    expect_c_equal(
        IfMissing("default"),
        "|| default"
    )

    # Test with real queries in action
    if (has_query(daf, "@ gene : is_marker")) {
        # Skip if property already exists
        skip("gene already has is_marker property in test")
    } else {
        # Test actual query results with IfMissing
        result <- get_query(daf, Axis("gene") |> LookupVector("is_marker") |> IfMissing(FALSE))
        expect_equal(result, c(X = FALSE, Y = FALSE, Z = FALSE))

        # Set the property for one gene
        set_vector(daf, "gene", "is_marker", c(TRUE, FALSE, FALSE))

        # Now the result should reflect the actual values
        result <- get_query(daf, Axis("gene") |> LookupVector("is_marker") |> IfMissing(FALSE))
        expect_equal(result, c(X = TRUE, Y = FALSE, Z = FALSE))
    }
})

# Test query results
test_that("query results are correct", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test!")
    set_scalar(daf, "version", "1.0")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    add_axis(daf, "batch", c("U", "V", "W"))
    set_vector(daf, "cell", "batch", c("U", "V"))
    set_vector(daf, "cell", "age", c(-1.0, 2.0))
    set_vector(daf, "batch", "sex", c("Male", "Female", "Male"))
    set_matrix(daf, "gene", "cell", "UMIs", matrix(c(1, 4, 2, 5, 3, 6), nrow = 3, ncol = 2))

    # Test query result dimensions
    expect_equal(query_result_dimensions(". version"), 0)
    expect_equal(query_result_dimensions("@ cell : age"), 1)
    expect_equal(query_result_dimensions("@ cell @ gene :: UMIs"), 2)

    # Test scalar queries
    expect_equal(get_query(daf, ". version"), "1.0")
    expect_equal(set(get_query(daf, ". ?")), set(c("version")))
    expect_equal(set(get_query(daf, "@ cell : ?")), set(c("batch", "age")))
    expect_equal(set(get_query(daf, "@ gene @ cell :: ?")), set(c("UMIs")))

    # Test has_query
    expect_true(has_query(daf, "@ cell : age"))
    expect_false(has_query(daf, "@ cell : youth"))

    # Test direct vector queries
    expect_equal(get_query(daf, "@ cell : age"), c(A = -1, B = 2))
    expect_equal(get_query(daf, "@ cell : batch"), c(A = "U", B = "V"))

    # Test direct matrix queries
    expect_equal(get_query(daf, "@ cell @ gene :: UMIs"), matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c("A", "B"), c("X", "Y", "Z"))))

    # Test query operations
    expect_equal(get_query(daf, "@ cell : age % Abs"), c(A = 1.0, B = 2.0))
    expect_equal(get_query(daf, "@ cell : age % Clamp min 0.5"), c(A = 0.5, B = 2.0))
    expect_equal(get_query(daf, "@ cell : age % Convert type Int8"), c(A = -1L, B = 2L))
    expect_equal(get_query(daf, "@ cell : age % Abs % Fraction"), c(A = 1 / 3, B = 2 / 3))
    expect_equal(get_query(daf, "@ cell : age % Abs % Log base 2"), c(A = 0.0, B = 1.0))
    expect_equal(get_query(daf, "@ cell : age % Significant high 2"), c(A = 0, B = 2))

    # Test reduction operations
    expect_equal(get_query(daf, "@ cell : age >> Max"), 2)
    expect_equal(get_query(daf, "@ cell : age >> Min"), -1)
    expect_equal(get_query(daf, "@ cell : age >> Median"), 0.5)
    expect_equal(get_query(daf, "@ cell : age >> Quantile p 0.5"), 0.5)
    expect_equal(get_query(daf, "@ cell : age >> Mean"), 0.5)
    expect_equal(get_query(daf, "@ cell : age >> Mean % Round"), 0)
    expect_equal(get_query(daf, "@ cell : age >> Std"), 1.5)
    expect_equal(get_query(daf, "@ cell : age >> StdN"), 3.0)
    expect_equal(get_query(daf, "@ cell : age >> Var"), 2.25)
    expect_equal(get_query(daf, "@ cell : age >> VarN"), 4.5)

    sparse_matrix <- Matrix::sparseMatrix(
        i = c(1, 1, 1, 2, 2),
        j = c(1, 2, 3, 1, 2),
        x = c(0, 1, 2, 3, 4),
        dims = c(2, 3)
    )
    rownames(sparse_matrix) <- c("A", "B")
    colnames(sparse_matrix) <- c("X", "Y", "Z")

    set_matrix(daf, "cell", "gene", "UMIs", sparse_matrix, overwrite = TRUE)
    m <- get_matrix(daf, "cell", "gene", "UMIs")
    expect_equal(m, sparse_matrix, ignore_attr = TRUE)
    frame <- get_dataframe_query(daf, "@ cell @ gene :: UMIs")
    expect_equal(as.matrix(frame), matrix(c(0, 1, 2, 3, 4, 0), nrow = 2, ncol = 3, byrow = TRUE), ignore_attr = TRUE)
    expect_equal(rownames(frame), c("A", "B"))
    expect_equal(colnames(frame), c("X", "Y", "Z"))

    frame <- get_dataframe(daf, "cell")
    expect_true(is.data.frame(frame))
    expect_equal(nrow(frame), 2)
    expect_true("age" %in% names(frame))
    expect_true("batch" %in% names(frame))

    frame <- get_dataframe(daf, "cell", "age")
    expect_true(is.data.frame(frame))
    expect_equal(names(frame), "age")
    expect_equal(nrow(frame), 2)

    frame <- get_dataframe(daf, "cell", list(age = ": age", sex = ": batch : sex"))
    expect_true(is.data.frame(frame))
    expect_equal(names(frame), c("age", "sex"))
    expect_equal(frame$sex, c("Male", "Female"))

    frame <- get_dataframe(daf, "cell", list(
        age = "age",
        list(sex = ": batch : sex")
    ))
    expect_true(is.data.frame(frame))
    expect_equal(names(frame), c("age", "sex"))
    expect_equal(frame$sex, c("Male", "Female"))
})

test_that("get_dataframe_query handles scalar query results correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_df_query")
    set_scalar(daf, "version", "1.0")
    set_scalar(daf, "numeric_val", 42)

    # Test string scalar
    result <- get_dataframe_query(daf, ". version")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), 1)
    expect_equal(result[1, 1], "1.0")
    expect_equal(colnames(result), "value")

    # Test numeric scalar
    result <- get_dataframe_query(daf, ". numeric_val")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), 1)
    expect_equal(result[1, 1], 42)
    expect_equal(colnames(result), "value")
})

test_that("get_dataframe_query handles vector query results correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "age", c(1.5, 2.5, 3.5))
    set_vector(daf, "cell", "type", c("T", "B", "NK"))

    # Test numeric vector
    result <- get_dataframe_query(daf, "@ cell : age")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 1)
    expect_equal(result$value, c(1.5, 2.5, 3.5))
    expect_equal(rownames(result), c("A", "B", "C"))

    # Test character vector
    result <- get_dataframe_query(daf, "@ cell : type")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 1)
    expect_equal(result$value, c("T", "B", "NK"))
    expect_equal(rownames(result), c("A", "B", "C"))

    # Test vector with operations
    result <- get_dataframe_query(daf, "@ cell : age % Abs")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(result$value, c(1.5, 2.5, 3.5))
})

test_that("get_dataframe_query handles matrix query results correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    # Create a dense matrix
    dense_matrix <- matrix(1:6, nrow = 2, ncol = 3)
    rownames(dense_matrix) <- c("A", "B")
    colnames(dense_matrix) <- c("X", "Y", "Z")
    set_matrix(daf, "cell", "gene", "expression", dense_matrix)

    # Test dense matrix
    result <- get_dataframe_query(daf, "@ cell @ gene :: expression")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 3)
    expect_equal(rownames(result), c("A", "B"))
    expect_equal(colnames(result), c("X", "Y", "Z"))
    expect_equal(as.matrix(result), dense_matrix)

    # Create a sparse matrix
    sparse_matrix <- Matrix::sparseMatrix(
        i = c(1, 2),
        j = c(1, 3),
        x = c(10, 20),
        dims = c(2, 3)
    )
    rownames(sparse_matrix) <- c("A", "B")
    colnames(sparse_matrix) <- c("X", "Y", "Z")
    set_matrix(daf, "cell", "gene", "sparse_expression", sparse_matrix)

    # Test sparse matrix
    result <- get_dataframe_query(daf, "@ cell @ gene :: sparse_expression")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 3)
    expect_equal(rownames(result), c("A", "B"))
    expect_equal(colnames(result), c("X", "Y", "Z"))
    expected <- matrix(c(10, 0, 0, 0, 0, 20), nrow = 2, byrow = TRUE)
    rownames(expected) <- c("A", "B")
    colnames(expected) <- c("X", "Y", "Z")
    expect_equal(as.matrix(result), expected)
})

test_that("get_dataframe_query handles sets of names correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    # Test axes names
    result <- get_dataframe_query(daf, "@ ?")
    expect_true(is.character(result)) # This will return a character vector per your implementation
    expect_true(all(c("cell", "gene") %in% result))

    # Test vector properties
    set_vector(daf, "cell", "age", c(1, 2, 3))
    set_vector(daf, "cell", "type", c("T", "B", "NK"))

    result <- get_dataframe_query(daf, "@ cell : ?")
    expect_true(is.character(result))
    expect_true(all(c("age", "type") %in% result))
})

test_that("get_dataframe_query can be used with pipes", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "age", c(1, 2, 3))

    # Test with query first
    result <- "@ cell : age" |> get_dataframe_query(daf)
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(result$value, c(1, 2, 3))

    # Test with daf first
    result <- daf |> get_dataframe_query("@ cell : age")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(result$value, c(1, 2, 3))
})

test_that("get_dataframe_query handles complex query operations", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    add_axis(daf, "batch", c("B1", "B2"))
    add_axis(daf, "donor", c("D1", "D2"))
    set_vector(daf, "cell", "age", c(1, 2, 3))
    set_vector(daf, "cell", "batch", c("B1", "B2", "B1"))
    set_vector(daf, "batch", "donor", c("D1", "D2"))

    # Test complex vector query with chained lookup (replaces Fetch)
    result <- get_dataframe_query(daf, Axis("cell") |> LookupVector("batch") |> LookupVector("donor"))
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(result$value, c("D1", "D2", "D1"))

    # Test with filtering
    result <- get_dataframe_query(daf, Axis("cell") |> BeginMask("batch") |> IsEqual("B1") |> EndMask() |> LookupVector("age"))
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 2)
    expect_equal(result$value, c(1, 3))

    # Test with element-wise operations
    result <- get_dataframe_query(daf, Axis("cell") |> LookupVector("age") |> Log(base = 2))
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(result$value, log2(c(1, 2, 3)), tolerance = 1e-10)
})

test_that("get_dataframe_query handles missing data appropriately", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))

    # Test with missing vector
    expect_error(result <- get_dataframe_query(daf, Axis("cell") |> LookupVector("missing") |> IfMissing(NA)))
    result <- get_dataframe_query(daf, Axis("cell") |> LookupVector("missing") |> IfMissing(8))
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)

    expect_equal(result$value, c(8, 8, 8))
    # Test with partial missing data
    expect_error(set_vector(daf, "cell", "partial", c(1, NA, 3)))
})

test_that("get_dataframe_query handles caching parameter", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "age", c(1, 2, 3))

    # Should work the same with cache=TRUE and cache=FALSE
    result1 <- get_dataframe_query(daf, "@ cell : age", cache = TRUE)
    result2 <- get_dataframe_query(daf, "@ cell : age", cache = FALSE)

    expect_equal(result1, result2)
})

test_that("get_dataframe_query reports appropriate errors", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_df_query")

    expect_error(get_dataframe_query(daf), "Please provide both a Daf object and a query")
    expect_error(get_dataframe_query(query = "@ missing_axis : value"), "Please provide both a Daf object and a query")

    add_axis(daf, "cell", c("A", "B", "C"))
    expect_error(get_dataframe_query(daf, "@ missing_axis : value"))
})

test_that("process_frame_columns handles different column formats correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test with NULL
    result <- dafr:::process_frame_columns(NULL)
    expect_null(result)

    # Test with single string
    columns <- "age"
    result <- dafr:::process_frame_columns(columns)
    expect_equal(length(result), 1)

    # Simple named list/vector
    columns <- list(age = "age", batch = "batch")
    result <- dafr:::process_frame_columns(columns)

    # Named list with nested lists
    columns <- list(
        age = "age",
        list(sex = ": batch : sex")
    )
    result <- dafr:::process_frame_columns(columns)

    # Complex mixed format
    columns <- list(
        age = "age",
        list(sex = ": batch : sex"),
        batch = "batch"
    )
    result <- dafr:::process_frame_columns(columns)
})

test_that("get_dataframe handles various column formats correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_frame_columns")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "batch", c("U", "V", "W"))
    set_vector(daf, "cell", "batch", c("U", "V"))
    set_vector(daf, "cell", "age", c(1.0, 2.0))
    set_vector(daf, "batch", "sex", c("Male", "Female", "Male"))

    # Test with NULL columns (should return all columns)
    frame <- get_dataframe(daf, "cell")
    expect_true(is.data.frame(frame))
    expect_equal(nrow(frame), 2)
    expect_true("age" %in% names(frame))
    expect_true("batch" %in% names(frame))

    # Test with single string
    frame <- get_dataframe(daf, "cell", "age")
    expect_true(is.data.frame(frame))
    expect_equal(names(frame), "age")
    expect_equal(nrow(frame), 2)

    # Test with simple named list
    frame <- get_dataframe(daf, "cell", list(age = "age", batch = "batch"))
    expect_true(is.data.frame(frame))
    expect_equal(sort(names(frame)), c("age", "batch"))
    expect_equal(nrow(frame), 2)

    # Test with nested lists in columns parameter
    frame <- get_dataframe(daf, "cell", list(
        age = "age",
        list(sex = ": batch : sex")
    ))
    expect_true(is.data.frame(frame))
    expect_equal(sort(names(frame)), c("age", "sex"))
    expect_equal(frame$sex, c("Male", "Female"))

    # Test with complex mixed format
    frame <- get_dataframe(daf, "cell", list(
        age = "age",
        list(sex = ": batch : sex"),
        batch = "batch"
    ))
    expect_true(is.data.frame(frame))
    expect_equal(sort(names(frame)), c("age", "batch", "sex"))
    expect_equal(frame$sex, c("Male", "Female"))

    # Test with query objects
    frame <- get_dataframe(daf, "cell", list(
        age = LookupVector("age"),
        list(sex = LookupVector("batch") |> LookupVector("sex"))
    ))
    expect_true(is.data.frame(frame))
    expect_equal(sort(names(frame)), c("age", "sex"))
    expect_equal(frame$sex, c("Male", "Female"))
})

test_that("get_tidy returns data in long format", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_tidy")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "batch", c("B1", "B2"))
    set_vector(daf, "cell", "age", c(1, 2, 3))
    set_vector(daf, "cell", "batch", c("B1", "B2", "B1"))
    set_vector(daf, "batch", "sex", c("Male", "Female"))

    # Test with all columns
    result <- get_tidy(daf, "cell", values_transform = list(value = as.character))
    expect_true(tibble::is_tibble(result))
    expect_equal(names(result), c("name", "key", "value"))
    expect_equal(nrow(result), 6) # 3 cells * 2 columns (age, batch)
    expect_equal(unique(result$name), c("A", "B", "C"))
    expect_equal(unique(result$key), c("age", "batch"))
    expect_equal(result$value[result$name == "A" & result$key == "age"], "1")
    expect_equal(result$value[result$name == "B" & result$key == "batch"], "B2")

    # Test with specific columns
    result <- get_tidy(daf, "cell", list(age = "age", sex = ": batch : sex"), values_transform = list(value = as.character))
    expect_true(tibble::is_tibble(result))
    expect_equal(names(result), c("name", "key", "value"))
    expect_equal(nrow(result), 6) # 3 cells * 2 columns (age, sex)
    expect_equal(unique(result$key), c("age", "sex"))
    expect_equal(result$value[result$name == "A" & result$key == "sex"], "Male")
    expect_equal(result$value[result$name == "B" & result$key == "sex"], "Female")

    # Test with matrix data
    add_axis(daf, "gene", c("G1", "G2"))
    set_matrix(daf, "cell", "gene", "expression", matrix(1:6, nrow = 3, ncol = 2))

    # Test matrix data by getting expression for each gene separately
    result <- get_tidy(daf, "cell", list(
        age = "age",
        g1_expr = ":: expression @ gene = G1",
        g2_expr = ":: expression @ gene = G2"
    ), values_transform = list(value = as.character))

    expect_true(tibble::is_tibble(result))
    expect_equal(names(result), c("name", "key", "value"))
    expect_equal(nrow(result), 9) # 3 cells * 3 columns (age, g1_expr, g2_expr)
    expect_equal(unique(result$key), c("age", "g1_expr", "g2_expr"))
    expect_equal(result$value[result$name == "A" & result$key == "g1_expr"], "1")
    expect_equal(result$value[result$name == "B" & result$key == "g2_expr"], "5")
})

# Test the new [ operator for Daf objects
test_that("[ operator for Daf objects works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_subscript_operator")
    set_scalar(daf, "version", "1.0")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    set_vector(daf, "cell", "batch", c("U", "V"))
    set_vector(daf, "cell", "age", c(-1.0, 2.0))
    set_matrix(daf, "gene", "cell", "UMIs", matrix(c(1, 4, 2, 5, 3, 6), nrow = 3, ncol = 2))

    # Test scalar queries
    expect_equal(daf[". version"], "1.0")
    expect_equal(daf[". version"], get_query(daf, ". version", cache = FALSE))

    # Test axes and property sets
    expect_equal(set(daf[". ?"]), set(c("version")))
    expect_equal(set(daf["@ cell : ?"]), set(c("batch", "age")))
    expect_equal(set(daf["@ gene @ cell :: ?"]), set(c("UMIs")))

    # Test vector queries
    expect_equal(daf["@ cell : age"], c(A = -1, B = 2))
    expect_equal(daf["@ cell : batch"], c(A = "U", B = "V"))
    expect_equal(daf["@ cell : age"], get_query(daf, "@ cell : age", cache = FALSE))

    # Test matrix queries
    expect_equal(daf["@ cell @ gene :: UMIs"], matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    expect_equal(daf["@ cell @ gene :: UMIs"], get_query(daf, "@ cell @ gene :: UMIs", cache = FALSE))

    # Test with query operations
    expect_equal(daf["@ cell : age % Abs"], c(A = 1.0, B = 2.0))
    expect_equal(daf["@ cell : age >> Max"], 2)
    expect_equal(daf["@ cell : age >> Min"], -1)

    # Test with query objects
    age_query <- Axis("cell") |> LookupVector("age")
    expect_equal(daf[age_query], c(A = -1, B = 2))
    expect_equal(daf[age_query], get_query(daf, age_query, cache = FALSE))

    # Test with complex query objects
    complex_query <- Axis("cell") |>
        LookupVector("age") |>
        Abs() |>
        Log(base = 2)
    expect_equal(daf[complex_query], log2(c(A = 1, B = 2)))
    expect_equal(daf[complex_query], get_query(daf, complex_query, cache = FALSE))

    # Test caching behavior by modifying data then querying again
    result1 <- daf["@ cell : age"]
    set_vector(daf, "cell", "age", c(3.0, 4.0), overwrite = TRUE)
    result2 <- daf["@ cell : age"]
    expect_false(identical(result1, result2))
    expect_equal(result2, c(A = 3.0, B = 4.0))
})

test_that("[ operator handles errors properly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_subscript_errors")

    # Test with invalid query
    expect_error(daf["invalid query"])

    # Test with non-existent axis
    expect_error(daf["@ non_existent_axis : property"])

    # Test with non-existent property
    add_axis(daf, "cell", c("A", "B"))
    expect_error(daf["@ cell : non_existent_property"])
})

test_that("[ operator retrieves vectors with multiple variants", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Create a test DAF object with a 'cells' axis
    daf <- memory_daf(name = "test_cells_vector")
    add_axis(daf, "cells", c("cell1", "cell2", "cell3", "cell4", "cell5"))

    # Add vector properties to the 'cells' axis
    set_vector(daf, "cells", "type", c("T", "B", "T", "NK", "B"))
    set_vector(daf, "cells", "age", c(1.5, 2.0, 3.5, 2.5, 1.0))
    set_vector(daf, "cells", "is_active", c(TRUE, FALSE, TRUE, TRUE, FALSE))

    # Test 1: Basic vector retrieval using daf['@ cells']
    cells <- daf["@ cells"]
    expect_equal(unname(cells), c("cell1", "cell2", "cell3", "cell4", "cell5"))

    # Test 2: Retrieve a specific property
    cell_types <- daf["@ cells : type"]
    expect_equal(cell_types, c(cell1 = "T", cell2 = "B", cell3 = "T", cell4 = "NK", cell5 = "B"))

    # Test 3: Filter cells by type
    t_cells <- daf["@ cells [ type = T ]"]
    expect_equal(unname(t_cells), c("cell1", "cell3"))

    # Test 4: Get ages of cells of a specific type
    b_cell_ages <- daf["@ cells [ type = B ] : age"]
    expect_equal(b_cell_ages, c(cell2 = 2.0, cell5 = 1.0))

    # Test 5: Use numerical comparison
    old_cells <- daf["@ cells [ age > 2.0 ]"]
    expect_equal(unname(old_cells), c("cell3", "cell4"))

    # Test 6: Combine multiple filters
    active_t_cells <- daf["@ cells [ type = T ] [ is_active ]"]
    expect_equal(unname(active_t_cells), c("cell1", "cell3"))

    # Test 7: Apply transformation to a vector
    log_ages <- daf["@ cells : age % Log base 2 eps 1.0"]
    expect_equal(log_ages, log2(c(cell1 = 1.5, cell2 = 2.0, cell3 = 3.5, cell4 = 2.5, cell5 = 1.0) + 1.0))

    # Test 8: Using pipe notation
    pipe_query <- Axis("cells") |>
        BeginMask("age") |>
        IsGreater(2.0) |>
        EndMask()
    old_cells_ages <- daf[pipe_query |> LookupVector("age")]
    expect_equal(old_cells_ages, c(cell3 = 3.5, cell4 = 2.5))

    # Test 9: Get property names using query
    cell_properties <- daf["@ cells : ?"]
    expect_true(all(c("type", "age", "is_active") %in% cell_properties))
})

test_that("CountBy operation works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Setup test data
    daf <- memory_daf("test_countby")
    add_axis(daf, "cell", c("A", "B", "C", "D", "E", "F"))
    set_vector(daf, "cell", "type", c("T", "B", "T", "B", "NK", "T"))
    set_vector(daf, "cell", "tissue", c("blood", "blood", "spleen", "spleen", "blood", "spleen"))

    # Test basic CountBy operation
    result <- get_query(daf, Axis("cell") |> LookupVector("type") |> CountBy("tissue"))
    expect_true(is.matrix(result))
    expect_equal(dim(result), c(3, 2)) # 3 types x 2 tissues
    expect_equal(rownames(result), c("B", "NK", "T"))
    expect_equal(colnames(result), c("blood", "spleen"))
    expect_equal(as.numeric(result["T", "blood"]), 1)
    expect_equal(as.numeric(result["T", "spleen"]), 2)
    expect_equal(as.numeric(result["B", "blood"]), 1)
    expect_equal(as.numeric(result["B", "spleen"]), 1)
    expect_equal(as.numeric(result["NK", "blood"]), 1)
    expect_equal(as.numeric(result["NK", "spleen"]), 0)

    # Test with filtering applied first
    result <- get_query(daf, Axis("cell") |> BeginMask("type") |> IsEqual("T") |> EndMask() |> LookupVector("type") |> CountBy("tissue"))
    expect_equal(dim(result), c(1, 2)) # 1 type x 2 tissues
    expect_equal(rownames(result), "T")
    expect_equal(as.numeric(result["T", "blood"]), 1)
    expect_equal(as.numeric(result["T", "spleen"]), 2)

    # Test with string query format
    result <- get_query(daf, "@ cell : type * tissue")
    expect_equal(dim(result), c(3, 2))
    expect_equal(sum(as.numeric(result)), 6) # Total count should match number of cells
})

test_that("GroupBy operation works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Setup test data
    daf <- memory_daf("test_groupby")
    add_axis(daf, "cell", c("A", "B", "C", "D", "E", "F"))
    set_vector(daf, "cell", "type", c("T", "B", "T", "B", "NK", "T"))
    set_vector(daf, "cell", "age", c(1, 2, 3, 4, 5, 6))
    set_vector(daf, "cell", "is_active", c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

    # Test GroupBy with Mean
    result <- get_query(daf, Axis("cell") |> LookupVector("age") |> GroupBy("type") |> Mean())
    expect_equal(names(result), c("B", "NK", "T"))
    expect_equal(result["T"], c(T = (1 + 3 + 6) / 3))
    expect_equal(result["B"], c(B = (2 + 4) / 2))
    expect_equal(result["NK"], c(NK = 5))

    # Test GroupBy with Max
    result <- get_query(daf, Axis("cell") |> LookupVector("age") |> GroupBy("type") |> Max())
    expect_equal(result["T"], c(T = 6))
    expect_equal(result["B"], c(B = 4))
    expect_equal(result["NK"], c(NK = 5))

    # Test GroupBy with Sum
    result <- get_query(daf, Axis("cell") |> LookupVector("age") |> GroupBy("type") |> Sum())
    expect_equal(result["T"], c(T = 1 + 3 + 6))
    expect_equal(result["B"], c(B = 2 + 4))
    expect_equal(result["NK"], c(NK = 5))

    # Test GroupBy with boolean values
    result <- get_query(daf, Axis("cell") |> LookupVector("is_active") |> GroupBy("type") |> Sum())
    expect_equal(result["T"], c(T = 2)) # Two active T cells
    expect_equal(result["B"], c(B = 0)) # No active B cells
    expect_equal(result["NK"], c(NK = 1)) # One active NK cell

    # Test with string query format
    result <- get_query(daf, "@ cell : age / type >> Mean")
    expect_equal(length(result), 3)
    expect_equal(names(result), c("B", "NK", "T"))

    # Test with filtering before GroupBy
    result <- get_query(daf, Axis("cell") |> BeginMask("is_active") |> EndMask() |> LookupVector("age") |> GroupBy("type") |> Mean())
    expect_equal(names(result), c("NK", "T")) # B cells are all inactive
    expect_equal(result["T"], c(T = (1 + 3) / 2)) # Only active T cells
    expect_equal(result["NK"], c(NK = 5)) # The NK cell is active
})

test_that("GroupBy and CountBy handle edge cases correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf("test_groupby_edge")
    add_axis(daf, "cell", c("A", "B", "C"))

    # Case with single category
    set_vector(daf, "cell", "type", c("T", "T", "T"))
    set_vector(daf, "cell", "age", c(1, 2, 3))

    result <- get_query(daf, Axis("cell") |> LookupVector("age") |> GroupBy("type") |> Mean())
    expect_equal(length(result), 1)
    expect_equal(names(result), "T")
    expect_equal(result["T"], c(T = 2)) # Mean of 1,2,3
})

test_that("AsAxis operation works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Setup test data
    daf <- memory_daf("test_asaxis")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    add_axis(daf, "sample", c("S1", "S2"))

    set_vector(daf, "cell", "sample_id", c("S1", "S2", "S1"))
    set_vector(daf, "sample", "batch", c("batch1", "batch2"))
    set_vector(daf, "sample", "condition", c("control", "treated"))

    # Test AsAxis with explicit axis
    result <- get_query(daf, Axis("cell") |> LookupVector("sample_id") |> AsAxis("sample") |> LookupVector("batch"))
    expect_equal(result, c(A = "batch1", B = "batch2", C = "batch1"))

    # Test string format
    result <- get_query(daf, "@ cell : sample_id =@ sample : batch")
    expect_equal(result, c(A = "batch1", B = "batch2", C = "batch1"))

    # Test AsAxis with CountBy
    set_vector(daf, "cell", "gene_name", c("X", "Y", "X"))
    result <- get_query(daf, Axis("cell") |> LookupVector("gene_name") |> AsAxis("gene") |> CountBy("sample_id"))
    expect_true(is.matrix(result))
    expect_equal(dim(result), c(3, 2))
    expect_equal(rownames(result), c("X", "Y", "Z"))
    expect_equal(colnames(result), c("S1", "S2"))
    expect_equal(as.numeric(result["X", "S1"]), 2) # X appears twice with sample S1
    expect_equal(as.numeric(result["Y", "S2"]), 1) # Y appears once with sample S2
    expect_equal(as.numeric(result["Z", "S1"]), 0)

    # Test AsAxis with filtering
    result <- get_query(daf, Axis("cell") |> BeginMask("sample_id") |> IsEqual("S1") |> EndMask() |>
        LookupVector("sample_id") |> AsAxis("sample") |> LookupVector("batch"))
    expect_equal(result, c(A = "batch1", C = "batch1"))
})

test_that("chained LookupVector works correctly (replaces Fetch)", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Setup test data
    daf <- memory_daf("test_chained_lookup")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    add_axis(daf, "batch", c("B1", "B2"))

    set_vector(daf, "cell", "batch", c("B1", "B2", "B1"))
    set_vector(daf, "batch", "color", c("red", "blue"))
    set_vector(daf, "batch", "condition", c("control", "treated"))

    # Test basic chained LookupVector
    result <- get_query(daf, Axis("cell") |> LookupVector("batch") |> LookupVector("color"))
    expect_equal(result, c(A = "red", B = "blue", C = "red"))

    # Test chained lookup with string notation
    result <- get_query(daf, "@ cell : batch : color")
    expect_equal(result, c(A = "red", B = "blue", C = "red"))

    # Test chained lookup with filtering
    result <- get_query(daf, Axis("cell") |> BeginMask("batch") |> IsEqual("B1") |> EndMask() |>
        LookupVector("batch") |> LookupVector("condition"))
    expect_equal(result, c(A = "control", C = "control"))
})

test_that("Names operation works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Setup test data
    daf <- memory_daf("test_names")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    set_scalar(daf, "version", "1.0")
    set_scalar(daf, "date", "2023-01-01")

    set_vector(daf, "cell", "age", c(1, 2, 3))
    set_vector(daf, "cell", "type", c("T", "B", "NK"))

    # Test Names for axes (v0.2.0: Axis() |> Names() for axis names)
    result <- get_query(daf, Axis() |> Names())
    expect_true(all(c("cell", "gene") %in% result))

    # Test Names for scalars (v0.2.0: LookupScalar() |> Names())
    result <- get_query(daf, LookupScalar() |> Names())
    expect_true(all(c("version", "date") %in% result))

    # Test Names for vectors on axis
    result <- get_query(daf, Axis("cell") |> LookupVector() |> Names())
    expect_true(all(c("age", "type") %in% result))

    # Test string format
    result <- get_query(daf, "@ cell : ?")
    expect_true(all(c("age", "type") %in% result))
})

test_that("query comparison operations work correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Setup test data
    daf <- memory_daf("test_comparison_ops")
    add_axis(daf, "cell", c("A", "B", "C", "D"))
    set_vector(daf, "cell", "age", c(1, 2, 3, 4))
    set_vector(daf, "cell", "batch", c("batch1", "batch2", "batch1", "batch2"))
    set_vector(daf, "cell", "gene_name", c("FOXP3", "CD4", "RPSL15", "CD8"))

    # Test IsEqual
    result <- get_query(daf, Axis("cell") |> BeginMask("age") |> IsEqual(3) |> EndMask() |> LookupVector("batch"))
    expect_equal(result, c(C = "batch1"))

    result <- Axis("cell") |>
        BeginMask("age") |>
        IsEqual(3) |>
        EndMask() |>
        LookupVector("batch") |>
        get_query(daf)
    expect_equal(result, c(C = "batch1"))

    # Test IsNotEqual
    result <- get_query(daf, Axis("cell") |> BeginMask("age") |> IsNotEqual(3) |> EndMask() |> LookupVector("batch"))
    expect_equal(result, c(A = "batch1", B = "batch2", D = "batch2"))

    result <- Axis("cell") |>
        BeginMask("age") |>
        IsNotEqual(3) |>
        EndMask() |>
        LookupVector("batch") |>
        get_query(daf)
    expect_equal(result, c(A = "batch1", B = "batch2", D = "batch2"))

    # Test IsLess
    result <- get_query(daf, Axis("cell") |> BeginMask("age") |> IsLess(3) |> EndMask() |> LookupVector("batch"))
    expect_equal(result, c(A = "batch1", B = "batch2"))

    result <- Axis("cell") |>
        BeginMask("age") |>
        IsLess(3) |>
        EndMask() |>
        LookupVector("batch") |>
        get_query(daf)
    expect_equal(result, c(A = "batch1", B = "batch2"))

    # Test IsLessEqual
    result <- get_query(daf, Axis("cell") |> BeginMask("age") |> IsLessEqual(2) |> EndMask() |> LookupVector("batch"))
    expect_equal(result, c(A = "batch1", B = "batch2"))

    result <- Axis("cell") |>
        BeginMask("age") |>
        IsLessEqual(2) |>
        EndMask() |>
        LookupVector("batch") |>
        get_query(daf)
    expect_equal(result, c(A = "batch1", B = "batch2"))

    # Test IsGreater
    result <- get_query(daf, Axis("cell") |> BeginMask("age") |> IsGreater(2) |> EndMask() |> LookupVector("batch"))
    expect_equal(result, c(C = "batch1", D = "batch2"))

    result <- Axis("cell") |>
        BeginMask("age") |>
        IsGreater(2) |>
        EndMask() |>
        LookupVector("batch") |>
        get_query(daf)
    expect_equal(result, c(C = "batch1", D = "batch2"))

    # Test IsGreaterEqual
    result <- get_query(daf, Axis("cell") |> BeginMask("age") |> IsGreaterEqual(3) |> EndMask() |> LookupVector("batch"))
    expect_equal(result, c(C = "batch1", D = "batch2"))

    result <- Axis("cell") |>
        BeginMask("age") |>
        IsGreaterEqual(3) |>
        EndMask() |>
        LookupVector("batch") |>
        get_query(daf)
    expect_equal(result, c(C = "batch1", D = "batch2"))

    # Test IsMatch
    result <- get_query(daf, Axis("cell") |> BeginMask("gene_name") |> IsMatch("CD.*") |> EndMask() |> LookupVector("age"))
    expect_equal(result, c(B = 2, D = 4))

    result <- Axis("cell") |>
        BeginMask("gene_name") |>
        IsMatch("CD.*") |>
        EndMask() |>
        LookupVector("age") |>
        get_query(daf)
    expect_equal(result, c(B = 2, D = 4))

    # Test IsNotMatch
    result <- get_query(daf, Axis("cell") |> BeginMask("gene_name") |> IsNotMatch("CD.*") |> EndMask() |> LookupVector("age"))
    expect_equal(result, c(A = 1, C = 3))

    result <- Axis("cell") |>
        BeginMask("gene_name") |>
        IsNotMatch("CD.*") |>
        EndMask() |>
        LookupVector("age") |>
        get_query(daf)
    expect_equal(result, c(A = 1, C = 3))
})

test_that("query boolean mask operations work correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Setup test data
    daf <- memory_daf("test_boolean_ops")
    add_axis(daf, "cell", c("A", "B", "C", "D"))
    set_vector(daf, "cell", "is_tcell", c(TRUE, TRUE, FALSE, FALSE))
    set_vector(daf, "cell", "is_active", c(TRUE, FALSE, TRUE, FALSE))
    set_vector(daf, "cell", "age", c(1, 2, 3, 4))

    # Test AndMask
    result <- get_query(daf, Axis("cell") |> BeginMask("is_tcell") |> EndMask() |> LookupVector("age"))
    expect_equal(result, c(A = 1, B = 2))

    # Test AndNegatedMask
    result <- get_query(daf, Axis("cell") |> BeginNegatedMask("is_tcell") |> EndMask() |> LookupVector("age"))
    expect_equal(result, c(C = 3, D = 4))

    # Test BeginMask with AndMask and IsEqual
    result <- get_query(daf, Axis("cell") |> BeginMask("is_tcell") |> AndMask("age") |> IsEqual(1) |> EndMask() |> LookupVector("is_active"))
    expect_equal(result, c(A = TRUE))

    # Test OrMask
    result <- get_query(daf, Axis("cell") |> BeginMask("is_tcell") |> OrMask("is_active") |> EndMask() |> LookupVector("age"))
    expect_equal(result, c(A = 1, B = 2, C = 3))

    # Test OrNegatedMask
    result <- get_query(daf, Axis("cell") |> BeginMask("is_tcell") |> OrNegatedMask("is_active") |> EndMask() |> LookupVector("age"))
    expect_equal(result, c(A = 1, B = 2, D = 4))

    # Test XorMask
    result <- get_query(daf, Axis("cell") |> BeginMask("is_tcell") |> XorMask("is_active") |> EndMask() |> LookupVector("age"))
    expect_equal(result, c(B = 2, C = 3))

    # Test XorNegatedMask
    result <- get_query(daf, Axis("cell") |> BeginMask("is_tcell") |> XorNegatedMask("is_active") |> EndMask() |> LookupVector("age"))
    expect_equal(result, c(A = 1, D = 4))

    # Test complex combination
    result <- get_query(daf, Axis("cell") |> BeginMask("is_tcell") |> OrNegatedMask("is_active") |> AndMask("age") |> IsLess(4) |> EndMask() |> LookupVector("age"))
    expect_equal(result, c(A = 1, B = 2))
})

test_that("query IfNot operation works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf("test_ifnot")
    add_axis(daf, "cell", c("A", "B", "C", "D"))
    set_vector(daf, "cell", "batch", c("batch1", "batch2", "batch1", "batch2"))
    add_axis(daf, "batch", c("batch1", "batch2"))
    set_vector(daf, "batch", "score", c(1, 3))
    set_vector(daf, "cell", "flag", c(TRUE, FALSE, TRUE, FALSE))
    # Test IfNot with empty string values
    result <- get_query(daf, Axis("cell") |> LookupVector("batch") |> IfNot("batch2") |> LookupVector("score"))
    expect_equal(result, c(A = 1, B = 3, C = 1, D = 3))
})

test_that("query_result_dimensions returns correct dimensions for various queries", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test scalar query dimension
    expect_equal(query_result_dimensions(". version"), 0)
    expect_equal(query_result_dimensions(LookupScalar("version")), 0)

    # Test vector query dimension
    expect_equal(query_result_dimensions("@ cell : age"), 1)
    expect_equal(query_result_dimensions(Axis("cell") |> LookupVector("age")), 1)

    # Test matrix query dimension
    expect_equal(query_result_dimensions("@ cell @ gene :: UMIs"), 2)
    expect_equal(query_result_dimensions(Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs")), 2)

    # Test set of names query dimension
    expect_equal(query_result_dimensions("@ ?"), -1)
    expect_equal(query_result_dimensions(Axis() |> Names()), -1)
    expect_equal(query_result_dimensions("@ cell : ?"), -1)

    # Test query with operations
    expect_equal(query_result_dimensions("@ cell : age >> Mean"), 0)
    expect_equal(query_result_dimensions("@ cell @ gene :: sparse_expression"), 2)

    # Test with filtering operations that don't change dimensions
    expect_equal(query_result_dimensions("@ cell [ age > 2 ] : type"), 1)
    expect_equal(query_result_dimensions(Axis("cell") |> BeginMask("age") |> IsGreater(2) |> EndMask() |> LookupVector("type")), 1)
})

test_that("query_result_dimensions works with pipe operators", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test with string query
    query_str <- "@ cell : age >> Mean"
    dim_result <- query_str |> query_result_dimensions()
    expect_equal(dim_result, 0)

    # Test with query object
    query_obj <- Axis("cell") |>
        LookupVector("age") |>
        Mean()
    dim_result <- query_obj |> query_result_dimensions()
    expect_equal(dim_result, 0)
})

test_that("query_result_dimensions handles errors gracefully", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test with invalid query string
    expect_error(query_result_dimensions("invalid query"))

    # Test with NULL input
    expect_error(query_result_dimensions(NULL))
})

test_that("parse_query handles various query strings correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test basic string parsing
    query_obj <- parse_query("@ cell")
    expect_true(inherits(query_obj, "JuliaObject"))

    # Test more complex query with multiple operations
    query_obj <- parse_query("@ cell : age % Abs >> Mean")
    expect_true(inherits(query_obj, "JuliaObject"))

    # Test full query with various operations
    complex_query <- parse_query("@ cell [ age > 2.5 ] : type / batch >> Mean")
    expect_true(inherits(query_obj, "JuliaObject"))

    # Test using the parsed query on a daf object
    daf <- memory_daf(name = "test_parse_query")
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "age", c(1, 2, 3))

    # Get query using string vs parsed object should be equivalent
    direct_result <- get_query(daf, "@ cell : age")
    parsed_result <- get_query(daf, parse_query("@ cell : age"))
    expect_equal(direct_result, parsed_result)

    # Test with a more complex query
    set_vector(daf, "cell", "type", c("X", "Y", "X"))
    direct_result <- get_query(daf, "@ cell [ type = X ] : age")
    parsed_result <- get_query(daf, parse_query("@ cell [ type = X ] : age"))
    expect_equal(direct_result, parsed_result)
    expect_equal(parsed_result, c(A = 1, C = 3))
})

test_that("parse_query handles errors correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test with malformed query
    expect_error(parse_query("invalid query"))
})

test_that("BeginMask/EndMask operations work correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test function accepts valid input
    expect_no_error(BeginMask("is_marker"))
    expect_no_error(BeginNegatedMask("is_marker"))
    expect_no_error(EndMask())

    # Test function validates input
    expect_error(BeginMask(123), "must be a character string")
    expect_error(BeginMask(), "is missing with no default")
    expect_error(BeginNegatedMask(123), "must be a character string")
    expect_error(BeginNegatedMask(), "is missing with no default")

    # Test string representation
    begin_mask <- BeginMask("is_marker")
    expect_match(as.character(begin_mask), "[ is_marker", fixed = TRUE)

    begin_neg_mask <- BeginNegatedMask("is_marker")
    expect_match(as.character(begin_neg_mask), "[ ! is_marker", fixed = TRUE)

    end_mask <- EndMask()
    expect_match(as.character(end_mask), "]", fixed = TRUE)

    # Test with pipe operator
    query <- Axis("gene") |>
        BeginMask("is_lateral") |>
        EndMask() |>
        LookupVector("label")
    expect_true(inherits(query, "JuliaObject"))

    # Test with actual data
    daf <- memory_daf(name = "test_begin_end_mask")
    add_axis(daf, "gene", c("X", "Y", "Z"))
    set_vector(daf, "gene", "is_lateral", c(TRUE, FALSE, TRUE))
    set_vector(daf, "gene", "label", c("geneX", "geneY", "geneZ"))

    # Test BeginMask + EndMask to filter genes
    result <- get_query(daf, Axis("gene") |> BeginMask("is_lateral") |> EndMask() |> LookupVector("label"))
    expect_equal(result, c(X = "geneX", Z = "geneZ"))

    # Test BeginNegatedMask + EndMask to filter genes
    result <- get_query(daf, Axis("gene") |> BeginNegatedMask("is_lateral") |> EndMask() |> LookupVector("label"))
    expect_equal(result, c(Y = "geneY"))

    # Test string query equivalence
    result <- get_query(daf, "@ gene [ is_lateral ] : label")
    expect_equal(result, c(X = "geneX", Z = "geneZ"))

    result <- get_query(daf, "@ gene [ ! is_lateral ] : label")
    expect_equal(result, c(Y = "geneY"))
})

test_that("SquareColumnIs operation works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test function accepts valid input
    expect_no_error(SquareColumnIs("A"))

    # Test function validates input
    expect_error(SquareColumnIs(123), "must be a character string")
    expect_error(SquareColumnIs(), "is missing with no default")

    # Test string representation
    square_col_is <- SquareColumnIs("A")
    expect_match(as.character(square_col_is), "@| A", fixed = TRUE)

    # Test with pipe operator
    query <- Axis("cell") |> SquareColumnIs("A")
    expect_match(as.character(query), "@ cell @| A", fixed = TRUE)

    # Test with actual data
    daf <- memory_daf(name = "test_square_column_is")
    add_axis(daf, "cell", c("A", "B", "C"))

    # Create a square similarity matrix between cells
    similarity <- matrix(c(
        1.0, 0.8, 0.2, # A's similarity to A, B, C
        0.8, 1.0, 0.6, # B's similarity to A, B, C
        0.2, 0.6, 1.0 # C's similarity to A, B, C
    ), nrow = 3, ncol = 3, byrow = TRUE)

    # Set the matrix (rows and columns are both "cell")
    set_matrix(daf, "cell", "cell", "similarity", similarity)

    # Test getting a column vector from a square matrix
    result <- get_query(daf, Axis("cell") |> LookupMatrix("similarity") |> SquareColumnIs("A"))
    expect_equal(length(result), 3)

    # For the string query approach, validate it's correctly formed
    string_query <- "@ cell :: similarity @| A"
    has_valid_string_query <- has_query(daf, string_query)
    expect_true(has_valid_string_query)
})

test_that("SquareRowIs operation works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test function accepts valid input
    expect_no_error(SquareRowIs("A"))

    # Test function validates input
    expect_error(SquareRowIs(123), "must be a character string")
    expect_error(SquareRowIs(), "is missing with no default")

    # Test string representation
    square_row_is <- SquareRowIs("A")
    expect_match(as.character(square_row_is), "@- A", fixed = TRUE)

    # Test with pipe operator
    query <- Axis("cell") |> SquareRowIs("A")
    expect_match(as.character(query), "@ cell @- A", fixed = TRUE)

    # Test with actual data
    daf <- memory_daf(name = "test_square_row_is")
    add_axis(daf, "cell", c("A", "B", "C"))

    # Create a square similarity matrix between cells
    similarity <- matrix(c(
        1.0, 0.8, 0.2, # A's similarity to A, B, C
        0.8, 1.0, 0.6, # B's similarity to A, B, C
        0.2, 0.6, 1.0 # C's similarity to A, B, C
    ), nrow = 3, ncol = 3, byrow = TRUE)

    # Set the matrix (rows and columns are both "cell")
    set_matrix(daf, "cell", "cell", "similarity", similarity)

    # Test getting a row vector from a square matrix
    result <- get_query(daf, Axis("cell") |> LookupMatrix("similarity") |> SquareRowIs("A"))
    expect_equal(length(result), 3)

    # For the string query approach, validate it's correctly formed
    string_query <- "@ cell :: similarity @- A"
    has_valid_string_query <- has_query(daf, string_query)
    expect_true(has_valid_string_query)
})

test_that("Axis validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Axis() without arguments is valid in v0.2.0 (produces "@")
    expect_no_error(Axis())
    expect_c_equal(Axis(), "@")

    # Test non-character axis parameter
    expect_error(
        Axis(123)
    )
})

test_that("LookupVector validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # LookupVector() without arguments is valid in v0.2.0 (produces ":")
    expect_no_error(LookupVector())
    expect_c_equal(LookupVector(), ":")

    # Test non-character property parameter
    expect_error(
        LookupVector(123)
    )
})

test_that("LookupScalar validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # LookupScalar() without arguments is valid in v0.2.0 (produces ".")
    expect_no_error(LookupScalar())
    expect_c_equal(LookupScalar(), ".")

    # Test non-character property parameter
    expect_error(
        LookupScalar(123)
    )
})

test_that("LookupMatrix validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing property parameter
    expect_error(
        LookupMatrix()
    )

    # Test non-character property parameter
    expect_error(
        LookupMatrix(123)
    )
})

test_that("IfMissing validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing missing_value parameter
    expect_error(
        IfMissing()
    )

    # Test NA as missing_value
    expect_error(
        IfMissing(NA)
    )
})

test_that("IfNot validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # There are no validation errors specific to IfNot as it accepts NULL as default
    # Just test the function exists
    expect_no_error(
        IfNot()
    )
})

test_that("AsAxis validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test non-character axis parameter
    expect_error(
        AsAxis(123)
    )
})

test_that("BeginMask validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing property parameter
    expect_error(
        BeginMask()
    )

    # Test non-character property parameter
    expect_error(
        BeginMask(123)
    )
})

test_that("BeginNegatedMask validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing property parameter
    expect_error(
        BeginNegatedMask()
    )

    # Test non-character property parameter
    expect_error(
        BeginNegatedMask(123)
    )
})

test_that("SquareColumnIs validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing value parameter
    expect_error(
        SquareColumnIs()
    )

    # Test non-character value parameter
    expect_error(
        SquareColumnIs(123)
    )
})

test_that("SquareRowIs validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing value parameter
    expect_error(
        SquareRowIs()
    )

    # Test non-character value parameter
    expect_error(
        SquareRowIs(123)
    )
})

test_that("AndMask validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing property parameter
    expect_error(
        AndMask()
    )

    # Test non-character property parameter
    expect_error(
        AndMask(123)
    )
})

test_that("AndNegatedMask validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing property parameter
    expect_error(
        AndNegatedMask()
    )

    # Test non-character property parameter
    expect_error(
        AndNegatedMask(123)
    )
})

test_that("OrMask validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing property parameter
    expect_error(
        OrMask()
    )

    # Test non-character property parameter
    expect_error(
        OrMask(123)
    )
})

test_that("OrNegatedMask validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing property parameter
    expect_error(
        OrNegatedMask()
    )

    # Test non-character property parameter
    expect_error(
        OrNegatedMask(123)
    )
})

test_that("XorMask validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing property parameter
    expect_error(
        XorMask()
    )

    # Test non-character property parameter
    expect_error(
        XorMask(123)
    )
})

test_that("XorNegatedMask validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing property parameter
    expect_error(
        XorNegatedMask()
    )

    # Test non-character property parameter
    expect_error(
        XorNegatedMask(123)
    )
})

test_that("IsLess validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing value parameter
    expect_error(
        IsLess()
    )
})

test_that("IsLessEqual validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing value parameter
    expect_error(
        IsLessEqual()
    )
})

test_that("IsEqual validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing value parameter
    expect_error(
        IsEqual()
    )
})

test_that("IsNotEqual validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing value parameter
    expect_error(
        IsNotEqual()
    )
})

test_that("IsGreater validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing value parameter
    expect_error(
        IsGreater()
    )
})

test_that("IsGreaterEqual validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing value parameter
    expect_error(
        IsGreaterEqual()
    )
})

test_that("IsMatch validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing value parameter
    expect_error(
        IsMatch()
    )

    # Test non-character value parameter
    expect_error(
        IsMatch(123)
    )
})

test_that("IsNotMatch validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing value parameter
    expect_error(
        IsNotMatch()
    )

    # Test non-character value parameter
    expect_error(
        IsNotMatch(123)
    )
})

test_that("CountBy validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing property parameter
    expect_error(
        CountBy()
    )

    # Test non-character property parameter
    expect_error(
        CountBy(123)
    )
})

test_that("GroupBy validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing property parameter
    expect_error(
        GroupBy()
    )

    # Test non-character property parameter
    expect_error(
        GroupBy(123)
    )
})

test_that("get_query validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing parameters
    expect_error(
        get_query()
    )

    expect_error(
        get_query(NULL, "query")
    )

    expect_error(
        get_query(memory_daf(), NULL)
    )
})

test_that("get_dataframe_query validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test missing parameters
    expect_error(
        get_dataframe_query()
    )

    expect_error(
        get_dataframe_query(NULL, "query")
    )

    expect_error(
        get_dataframe_query(memory_daf(), NULL)
    )
})

# Tests for escape_value and unescape_value

test_that("escape_value escapes simple strings correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # A plain string without special characters should remain the same
    result <- escape_value("hello")
    expect_equal(result, "hello")
})

test_that("escape_value and unescape_value are inverse operations", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test round-trip for simple values
    original <- "simple"
    escaped <- escape_value(original)
    unescaped <- unescape_value(escaped)
    expect_equal(unescaped, original)

    # Test round-trip for values with special characters
    original_special <- "hello world"
    escaped_special <- escape_value(original_special)
    unescaped_special <- unescape_value(escaped_special)
    expect_equal(unescaped_special, original_special)
})

test_that("escape_value handles values with spaces", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    result <- escape_value("hello world")
    # The escaped value should be different from the original since it contains a space
    unescaped <- unescape_value(result)
    expect_equal(unescaped, "hello world")
})

test_that("escape_value handles values with backslashes", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    result <- escape_value("path\\to\\file")
    unescaped <- unescape_value(result)
    expect_equal(unescaped, "path\\to\\file")
})

test_that("unescape_value reverses escape_value for various inputs", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    test_values <- c("simple", "with space", "with\\backslash", "with.dot", "12345")
    for (val in test_values) {
        escaped <- escape_value(val)
        unescaped <- unescape_value(escaped)
        expect_equal(unescaped, val, info = paste("Failed for value:", val))
    }
})

# Tests for query_requires_relayout

test_that("query_requires_relayout returns FALSE for vector queries", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "relayout_test!")
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "age", c(1, 2, 3))

    # A simple vector query should not require relayout
    result <- query_requires_relayout(daf, "@ cell : age")
    expect_false(result)
})

test_that("query_requires_relayout returns FALSE for scalar queries", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "relayout_scalar_test!")
    set_scalar(daf, "version", "1.0")

    result <- query_requires_relayout(daf, ". version")
    expect_false(result)
})

test_that("query_requires_relayout works with matrix queries", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "relayout_matrix_test!")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    # Set matrix in one orientation
    mat <- matrix(1:6, nrow = 2, ncol = 3)
    set_matrix(daf, "cell", "gene", "expr", mat, relayout = FALSE)

    # Query in the same orientation should NOT require relayout
    result <- query_requires_relayout(daf, "@ cell @ gene :: expr")
    expect_false(result)

    # Query in flipped orientation should require relayout
    result <- query_requires_relayout(daf, "@ gene @ cell :: expr")
    expect_true(result)
})

test_that("query_requires_relayout works with query objects", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "relayout_obj_test!")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    mat <- matrix(1:6, nrow = 2, ncol = 3)
    set_matrix(daf, "cell", "gene", "expr", mat, relayout = FALSE)

    # Using query objects
    query <- Axis("cell") |>
        Axis("gene") |>
        LookupMatrix("expr")
    result <- query_requires_relayout(daf, query)
    expect_false(result)
})

# =============================================================================
# Tests for new v0.2.0 functions
# =============================================================================

test_that("LookupScalar query formatting works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test basic formatting
    expect_c_equal(LookupScalar("version"), ". version")

    # Test with Axis (used in scalar queries)
    expect_c_equal(LookupScalar("version") |> IfMissing("unknown"), ". version || unknown")
})

test_that("LookupScalar works with actual data", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_lookup_scalar")
    set_scalar(daf, "version", "1.0")
    set_scalar(daf, "count", 42)

    # Test scalar retrieval
    result <- get_query(daf, LookupScalar("version"))
    expect_equal(result, "1.0")

    result <- get_query(daf, LookupScalar("count"))
    expect_equal(result, 42)

    # Test scalar with IfMissing
    result <- get_query(daf, LookupScalar("nonexistent") |> IfMissing("default"))
    expect_equal(result, "default")
})

test_that("LookupMatrix query formatting works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test basic formatting
    expect_c_equal(LookupMatrix("UMIs"), ":: UMIs")

    # Test with Axis
    expect_c_equal(Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs"), "@ cell @ gene :: UMIs")
})

test_that("LookupMatrix works with actual data", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_lookup_matrix")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    mat <- matrix(1:6, nrow = 2, ncol = 3)
    rownames(mat) <- c("A", "B")
    colnames(mat) <- c("X", "Y", "Z")
    set_matrix(daf, "cell", "gene", "UMIs", mat)

    # Test matrix retrieval
    result <- get_query(daf, Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs"))
    expect_true(is.matrix(result))
    expect_equal(dim(result), c(2, 3))
    expect_equal(rownames(result), c("A", "B"))
    expect_equal(colnames(result), c("X", "Y", "Z"))

    # Test with string query
    result <- get_query(daf, "@ cell @ gene :: UMIs")
    expect_true(is.matrix(result))
    expect_equal(dim(result), c(2, 3))
})

test_that("GroupColumnsBy query formatting works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_c_equal(GroupColumnsBy("type"), "|/ type")
    expect_c_equal(Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs") |> GroupColumnsBy("type") |> ReduceToColumn(Mean()), "@ cell @ gene :: UMIs |/ type >| Mean")
})

test_that("GroupColumnsBy validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_error(GroupColumnsBy())
    expect_error(GroupColumnsBy(123))
})

test_that("GroupRowsBy query formatting works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_c_equal(GroupRowsBy("type"), "-/ type")
    expect_c_equal(Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs") |> GroupRowsBy("type") |> ReduceToRow(Mean()), "@ cell @ gene :: UMIs -/ type >- Mean")
})

test_that("GroupRowsBy validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_error(GroupRowsBy())
    expect_error(GroupRowsBy(123))
})

test_that("ReduceToColumn query formatting works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_c_equal(ReduceToColumn(Sum()), ">| Sum")
    expect_c_equal(ReduceToColumn(Mean()), ">| Mean")
    expect_c_equal(ReduceToColumn(Max()), ">| Max")
})

test_that("ReduceToColumn validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_error(ReduceToColumn())
})

test_that("ReduceToRow query formatting works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_c_equal(ReduceToRow(Sum()), ">- Sum")
    expect_c_equal(ReduceToRow(Mean()), ">- Mean")
    expect_c_equal(ReduceToRow(Max()), ">- Max")
})

test_that("ReduceToRow validates inputs correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    expect_error(ReduceToRow())
})

test_that("ReduceToColumn works with actual data", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_reduce_to_column")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y"))
    mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
    set_matrix(daf, "cell", "gene", "UMIs", mat)

    # ReduceToColumn reduces each row of the matrix to a single value
    # (reduces across columns -> result is a column vector indexed by rows)
    result <- get_query(daf, Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs") |> ReduceToColumn(Sum()))
    expect_equal(length(result), 3)
    expect_equal(names(result), c("A", "B", "C"))
    expect_equal(as.numeric(result), c(1 + 4, 2 + 5, 3 + 6))

    # Test with string query
    result <- get_query(daf, "@ cell @ gene :: UMIs >| Sum")
    expect_equal(length(result), 3)
    expect_equal(as.numeric(result), c(5, 7, 9))
})

test_that("ReduceToRow works with actual data", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_reduce_to_row")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y"))
    mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
    set_matrix(daf, "cell", "gene", "UMIs", mat)

    # ReduceToRow reduces each column of the matrix to a single value
    # (reduces across rows -> result is a row vector indexed by columns)
    result <- get_query(daf, Axis("cell") |> Axis("gene") |> LookupMatrix("UMIs") |> ReduceToRow(Sum()))
    expect_equal(length(result), 2)
    expect_equal(names(result), c("X", "Y"))
    expect_equal(as.numeric(result), c(1 + 2 + 3, 4 + 5 + 6))

    # Test with string query
    result <- get_query(daf, "@ cell @ gene :: UMIs >- Sum")
    expect_equal(length(result), 2)
    expect_equal(as.numeric(result), c(6, 15))
})

test_that("GroupColumnsBy and GroupRowsBy work with actual data", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    daf <- memory_daf(name = "test_group_matrix")
    add_axis(daf, "cell", c("A", "B", "C", "D"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    add_axis(daf, "type", c("T", "B"))

    set_vector(daf, "cell", "type", c("T", "B", "T", "B"))
    set_vector(daf, "gene", "category", c("marker", "marker", "housekeeping"))

    mat <- matrix(c(
        1, 2, 3,
        4, 5, 6,
        7, 8, 9,
        10, 11, 12
    ), nrow = 4, ncol = 3, byrow = TRUE)
    set_matrix(daf, "cell", "gene", "UMIs", mat)

    # GroupRowsBy groups rows (cells) by type, then reduces to a row per group
    result <- get_query(daf, "@ cell @ gene :: UMIs -/ type >- Mean")
    expect_true(is.matrix(result))
    expect_equal(nrow(result), 2) # 2 types
    expect_equal(ncol(result), 3) # 3 genes

    # GroupColumnsBy groups columns (genes) by category, then reduces to a column per group
    result <- get_query(daf, "@ cell @ gene :: UMIs |/ category >| Mean")
    expect_true(is.matrix(result))
    expect_equal(nrow(result), 4) # 4 cells
    expect_equal(ncol(result), 2) # 2 categories
})

test_that("complete_path works correctly", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test with memory_daf (should return NULL)
    daf <- memory_daf(name = "test_complete_path")
    result <- complete_path(daf)
    expect_null(result)

    # Test with files_daf (should return the path)
    tmpdir <- tempfile("daf_test_")
    dir.create(tmpdir, recursive = TRUE)
    fdaf <- files_daf(tmpdir, "w+", name = "test_files_daf")
    add_axis(fdaf, "cell", c("A", "B"))
    result <- complete_path(fdaf)
    expect_true(is.character(result))
    expect_true(nchar(result) > 0)
    unlink(tmpdir, recursive = TRUE)
})

test_that("deprecated functions emit warnings but still work", {
    skip_if(!JULIA_AVAILABLE, "Julia not available")
    # Test that deprecated wrappers call through to new functions
    expect_warning(Lookup("test"), "deprecated")
    expect_warning(And("test"), "deprecated")
    expect_warning(AndNot("test"), "deprecated")
    expect_warning(Or("test"), "deprecated")
    expect_warning(OrNot("test"), "deprecated")
    expect_warning(Xor("test"), "deprecated")
    expect_warning(XorNot("test"), "deprecated")
    expect_warning(SquareMaskColumn("test"), "deprecated")
    expect_warning(SquareMaskRow("test"), "deprecated")

    # Test that Fetch and MaskSlice produce errors (fully removed)
    expect_warning(expect_error(Fetch("test")))
    expect_warning(expect_error(MaskSlice("test")))
})
