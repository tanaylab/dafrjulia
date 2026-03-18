# Test element-wise operations

test_that("Abs operation works", {
    daf <- setup_test_data()

    # Test without piping
    query <- Axis("cell") |>
        LookupVector("values") |>
        Abs()
    result <- get_query(daf, query)
    expect_equal(result, c(A = 1.5, B = 0, C = 2.5))

    # Test with piping
    result <- get_query(daf, Abs(Axis("cell") |> LookupVector("values")))
    expect_equal(result, c(A = 1.5, B = 0, C = 2.5))
})

test_that("Clamp operation works", {
    daf <- setup_test_data()

    # Test with min only - must explicitly set max = NULL
    query <- Axis("cell") |>
        LookupVector("values") |>
        Clamp(min = 0, max = NULL)
    result <- get_query(daf, query)
    expect_equal(result, c(A = 0, B = 0, C = 2.5))

    # Test with max only - must explicitly set min = NULL
    query <- Axis("cell") |>
        LookupVector("values") |>
        Clamp(min = NULL, max = 1)
    result <- get_query(daf, query)
    expect_equal(result, c(A = -1.5, B = 0, C = 1))

    # Test with both min and max
    query <- Axis("cell") |>
        LookupVector("values") |>
        Clamp(min = -1, max = 1)
    result <- get_query(daf, query)
    expect_equal(result, c(A = -1, B = 0, C = 1))

    # Test with piping
    result <- get_query(daf, Clamp(min = 0, max = 2, Axis("cell") |> LookupVector("values")))
    expect_equal(result, c(A = 0, B = 0, C = 2))
})

test_that("Convert operation works", {
    daf <- setup_test_data()

    # Test converting to Int32
    query <- Axis("cell") |>
        LookupVector("values") |>
        Convert("Float32")
    result <- get_query(daf, query)
    expect_equal(result, c(A = -1.5, B = 0, C = 2.5))

    # Test with piping
    result <- get_query(daf, Convert("Float32", Axis("cell") |> LookupVector("values")))
    expect_equal(result, c(A = -1.5, B = 0, C = 2.5))

    # Test missing type parameter
    expect_error(Convert())
})

test_that("Fraction operation works", {
    daf <- setup_test_data()

    # Create a test vector with positive values only
    set_vector(daf, "cell", "positive", c(2, 3, 5))

    # Test Fraction
    query <- Axis("cell") |>
        LookupVector("positive") |>
        Fraction()
    result <- get_query(daf, query)
    expect_equal(result, c(A = 2 / 10, B = 3 / 10, C = 5 / 10))

    # Test with piping
    result <- get_query(daf, Fraction(Axis("cell") |> LookupVector("positive")))
    expect_equal(result, c(A = 2 / 10, B = 3 / 10, C = 5 / 10))
})

test_that("Log operation works", {
    daf <- setup_test_data()

    # Create a test vector with positive values only
    set_vector(daf, "cell", "positive", c(A = 1, B = 10, C = 100))

    # Test with default base (e)
    query <- Axis("cell") |>
        LookupVector("positive") |>
        Log()
    result <- get_query(daf, query)
    expect_equal(result, c(A = log(1), B = log(10), C = log(100)))

    # Test with base 10
    query <- Axis("cell") |>
        LookupVector("positive") |>
        Log(base = 10)
    result <- get_query(daf, query)
    expect_equal(result, c(A = log10(1), B = log10(10), C = log10(100)))

    # Test with base 2
    query <- Axis("cell") |>
        LookupVector("positive") |>
        Log(base = 2)
    result <- get_query(daf, query)
    expect_equal(result, c(A = log2(1), B = log2(10), C = log2(100)))

    # Test with eps to avoid log(0)
    set_vector(daf, "cell", "with_zero", c(A = 0, B = 1, C = 10))
    query <- Axis("cell") |>
        LookupVector("with_zero") |>
        Log(eps = 1e-10)
    result <- get_query(daf, query)
    expect_equal(result[1], c(A = log(1e-10)), tolerance = 1e-10)

    # Test with piping
    result <- get_query(daf, Log(base = 10, Axis("cell") |> LookupVector("positive")))
    expect_equal(result, c(A = log10(1), B = log10(10), C = log10(100)))
})

test_that("Round operation works", {
    daf <- setup_test_data()

    # Create a test vector with decimal values
    set_vector(daf, "cell", "decimals", c(1.4, 2.5, 3.6))

    # Test Round
    query <- Axis("cell") |>
        LookupVector("decimals") |>
        Round()
    result <- get_query(daf, query)
    expect_equal(result, c(A = 1, B = 2, C = 4)) # Julia rounds to nearest even integer for ties

    # Test with piping
    result <- get_query(daf, Round(Axis("cell") |> LookupVector("decimals")))
    expect_equal(result, c(A = 1, B = 2, C = 4))
})

test_that("Significant operation works", {
    daf <- setup_test_data()

    # Create test data with a column that has one significant value
    set_vector(daf, "cell", "effect_sizes", c(1.0, 4.0, 0.5))

    # Test with high threshold only
    query <- Axis("cell") |>
        LookupVector("effect_sizes") |>
        Significant(high = 3.0)
    result <- get_query(daf, query)
    expect_equal(result, c(A = 0, B = 4.0, C = 0))

    # Test with high and low thresholds
    query <- Axis("cell") |>
        LookupVector("effect_sizes") |>
        Significant(high = 3.0, low = 1.0)
    result <- get_query(daf, query)
    expect_equal(result, c(A = 1.0, B = 4.0, C = 0))

    # Test with no significant values
    set_vector(daf, "cell", "low_effects", c(A = 1.0, B = 2.0, C = 0.5))
    query <- Axis("cell") |>
        LookupVector("low_effects") |>
        Significant(high = 3.0)
    result <- get_query(daf, query)
    expect_equal(result, c(A = 0, B = 0, C = 0))

    # Test with missing high parameter
    expect_error(Significant(), "argument high must be provided")

    # Test with piping
    result <- get_query(daf, Significant(high = 3.0, Axis("cell") |> LookupVector("effect_sizes")))
    expect_equal(result, c(A = 0, B = 4.0, C = 0))
})

# Test reduction operations

test_that("Max operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        LookupVector("values") |>
        Max()
    result <- get_query(daf, query)
    expect_equal(result, 2.5)

    # Verify the matrix exists so we can test matrix reduction
    expect_true(has_matrix(daf, "cell", "gene", "data"))

    # Test matrix reduction on columns
    query <- Axis("cell") |>
        Axis("gene") |>
        LookupMatrix("data") |>
        Max()
    result <- get_query(daf, query)
    expect_equal(result, 9) # Max of all matrix values

    # Test with piping
    result <- get_query(daf, Max(Axis("cell") |> LookupVector("values")))
    expect_equal(result, 2.5)
})

test_that("Min operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        LookupVector("values") |>
        Min()
    result <- get_query(daf, query)
    expect_equal(result, -1.5)

    # Test matrix reduction
    query <- Axis("cell") |>
        Axis("gene") |>
        LookupMatrix("data") |>
        Min()
    result <- get_query(daf, query)
    expect_equal(result, 1) # Min of all matrix values

    # Test with piping
    result <- get_query(daf, Min(Axis("cell") |> LookupVector("values")))
    expect_equal(result, -1.5)
})

test_that("Mean operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        LookupVector("values") |>
        Mean()
    result <- get_query(daf, query)
    expect_equal(result, sum(c(-1.5, 0, 2.5)) / 3)

    # Test matrix reduction
    query <- Axis("cell") |>
        Axis("gene") |>
        LookupMatrix("data") |>
        Mean()
    result <- get_query(daf, query)
    expect_equal(result, 5) # Mean of all matrix values

    # Test with piping
    result <- get_query(daf, Mean(Axis("cell") |> LookupVector("values")))
    expect_equal(result, sum(c(-1.5, 0, 2.5)) / 3)
})

test_that("Median operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        LookupVector("values") |>
        Median()
    result <- get_query(daf, query)
    expect_equal(result, 0) # Middle value is 0

    # Test matrix reduction
    query <- Axis("cell") |>
        Axis("gene") |>
        LookupMatrix("data") |>
        Median()
    result <- get_query(daf, query)
    expect_equal(result, 5) # Median of all matrix values

    # Test with piping
    result <- get_query(daf, Median(Axis("cell") |> LookupVector("values")))
    expect_equal(result, 0)
})

test_that("Quantile operation works", {
    daf <- setup_test_data()

    # Test vector reduction with default p=0.5 (median)
    query <- Axis("cell") |>
        LookupVector("values") |>
        Quantile()
    result <- get_query(daf, query)
    expect_equal(result, 0) # Middle value is 0

    # Test with p=0.25 (first quartile)
    query <- Axis("cell") |>
        LookupVector("values") |>
        Quantile(p = 0.25)
    result <- get_query(daf, query)
    expect_equal(result, -0.75) # Interpolated value

    # Test with invalid p value
    expect_error(
        Quantile(p = 2)
    )

    # Test with piping
    result <- get_query(daf, Quantile(p = 0.75, Axis("cell") |> LookupVector("values")))
    expect_equal(result, 1.25) # Interpolated value
})

test_that("Sum operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        LookupVector("values") |>
        Sum()
    result <- get_query(daf, query)
    expect_equal(result, sum(c(-1.5, 0, 2.5)))

    # Test matrix reduction
    query <- Axis("cell") |>
        Axis("gene") |>
        LookupMatrix("data") |>
        Sum()
    result <- get_query(daf, query)
    expect_equal(result, 45) # Sum of all matrix values

    # Test with piping
    result <- get_query(daf, Sum(Axis("cell") |> LookupVector("values")))
    expect_equal(result, sum(c(-1.5, 0, 2.5)))
})

test_that("Std operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        LookupVector("values") |>
        Std()
    result <- get_query(daf, query)
    expect_equal(result, sd_uncorrected(c(-1.5, 0, 2.5)), tolerance = 1e-5)

    # Test with piping
    result <- get_query(daf, Std(Axis("cell") |> LookupVector("values")))
    expect_equal(result, sd_uncorrected(c(-1.5, 0, 2.5)), tolerance = 1e-5)
})

test_that("StdN operation works", {
    daf <- setup_test_data()

    # Create a test vector with non-zero mean
    set_vector(daf, "cell", "positive", c(2, 3, 7))

    # Test StdN (normalized standard deviation: std / mean)
    query <- Axis("cell") |>
        LookupVector("positive") |>
        StdN()
    result <- get_query(daf, query)
    expect_equal(result, sd_uncorrected(c(2, 3, 7)) / mean(c(2, 3, 7)), tolerance = 1e-5)

    # Test with piping
    result <- get_query(daf, StdN(Axis("cell") |> LookupVector("positive")))
    expect_equal(result, sd_uncorrected(c(2, 3, 7)) / mean(c(2, 3, 7)), tolerance = 1e-5)
})

test_that("Var operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        LookupVector("values") |>
        Var()
    result <- get_query(daf, query)
    expect_equal(result, var_uncorrected(c(-1.5, 0, 2.5)), tolerance = 1e-5)

    # Test with piping
    result <- get_query(daf, Var(Axis("cell") |> LookupVector("values")))
    expect_equal(result, var_uncorrected(c(-1.5, 0, 2.5)), tolerance = 1e-5)
})

test_that("VarN operation works", {
    daf <- setup_test_data()

    # Create a test vector with non-zero mean
    set_vector(daf, "cell", "positive", c(2, 3, 7))

    # Test VarN (normalized variance: var / mean)
    query <- Axis("cell") |>
        LookupVector("positive") |>
        VarN()
    result <- get_query(daf, query)
    expect_equal(result, var_uncorrected(c(2, 3, 7)) / mean(c(2, 3, 7)), tolerance = 1e-5)

    # Test with piping
    result <- get_query(daf, VarN(Axis("cell") |> LookupVector("positive")))
    expect_equal(result, var_uncorrected(c(2, 3, 7)) / mean(c(2, 3, 7)), tolerance = 1e-5)
})

test_that("Abs operation validates inputs correctly", {
    expect_error(
        Abs(1, 2)
    )
})

test_that("Clamp operation validates inputs correctly", {
    # Test invalid min parameter
    expect_error(
        Clamp(min = "not_a_number")
    )

    # Test invalid max parameter
    expect_error(
        Clamp(max = "not_a_number")
    )
})

test_that("Convert operation validates inputs correctly", {
    # Test missing type parameter
    expect_error(
        Convert()
    )

    # Test invalid type parameter
    expect_error(
        Convert(123)
    )
})

test_that("Fraction operation validates inputs correctly", {
    expect_error(
        Fraction(1, 2)
    )
})

test_that("Log operation validates inputs correctly", {
    # Test invalid base parameter
    expect_error(
        Log(base = "not_a_number")
    )

    # Test invalid eps parameter
    expect_error(
        Log(eps = "not_a_number")
    )
})

test_that("Round operation validates inputs correctly", {
    expect_error(
        Round(1, 2)
    )
})

test_that("Significant operation validates inputs correctly", {
    # Test missing high parameter
    expect_error(
        Significant()
    )

    # Test invalid high parameter
    expect_error(
        Significant(high = "not_a_number")
    )

    # Test invalid low parameter
    expect_error(
        Significant(high = 1, low = "not_a_number")
    )
})

test_that("Max operation validates inputs correctly", {
    expect_error(
        Max(1, 2)
    )
})

test_that("Min operation validates inputs correctly", {
    expect_error(
        Min(1, 2)
    )
})

test_that("Mean operation validates inputs correctly", {
    expect_error(
        Mean(1, 2)
    )
})

test_that("Median operation validates inputs correctly", {
    expect_error(
        Median(1, 2)
    )
})

test_that("Quantile operation validates inputs correctly", {
    # Test invalid p parameter - string
    expect_error(
        Quantile(p = "not_a_number")
    )

    # Test invalid p parameter - out of range
    expect_error(
        Quantile(p = 2)
    )

    # Test invalid p parameter - negative
    expect_error(
        Quantile(p = -0.5)
    )

    # Test invalid p parameter - vector
    expect_error(
        Quantile(p = c(0.25, 0.75))
    )
})

test_that("Sum operation validates inputs correctly", {
    expect_error(
        Sum(1, 2)
    )
})

test_that("Std operation validates inputs correctly", {
    expect_error(
        Std(1, 2)
    )
})

test_that("StdN operation validates inputs correctly", {
    expect_error(
        StdN(1, 2)
    )
})

test_that("Var operation validates inputs correctly", {
    expect_error(
        Var(1, 2)
    )
})

test_that("VarN operation validates inputs correctly", {
    expect_error(
        VarN(1, 2)
    )
})

# Test new operations: Count, GeoMean, Mode

test_that("Count operation works on vectors", {
    daf <- setup_test_data()

    # Test vector reduction - Count counts all elements
    query <- Axis("cell") |>
        LookupVector("values") |>
        Count()
    result <- get_query(daf, query)
    # values are c(-1.5, 0, 2.5), so 3 elements total
    expect_equal(result, 3)

    # Test with piping
    result <- get_query(daf, Count(Axis("cell") |> LookupVector("values")))
    expect_equal(result, 3)
})

test_that("Count operation works on matrices", {
    daf <- setup_test_data()

    # Test matrix reduction - count non-zero elements per column
    query <- Axis("cell") |>
        Axis("gene") |>
        LookupMatrix("data") |>
        Count()
    result <- get_query(daf, query)
    # matrix data is matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3)
    # Count over matrix returns count of all entries
    expect_equal(result, 9)
})

test_that("Count operation works with all-zero vector", {
    daf <- setup_test_data()
    set_vector(daf, "cell", "zeros", c(0.0, 0.0, 0.0))
    query <- Axis("cell") |>
        LookupVector("zeros") |>
        Count()
    result <- get_query(daf, query)
    # Count counts all elements, not just non-zero
    expect_equal(result, 3)
})

test_that("Count operation with type parameter", {
    daf <- setup_test_data()

    query <- Axis("cell") |>
        LookupVector("values") |>
        Count(type = "Float64")
    result <- get_query(daf, query)
    # Count counts all elements (3), with Float64 type
    expect_equal(result, 3)
})

test_that("Count query string format works", {
    daf <- setup_test_data()
    result <- get_query(daf, "@ cell : values >> Count")
    # Count counts all elements
    expect_equal(result, 3)
})

test_that("GeoMean operation works on vectors", {
    daf <- setup_test_data()

    # Create a test vector with positive values
    set_vector(daf, "cell", "positive", c(1, 4, 16))

    # Test GeoMean - geometric mean of [1, 4, 16] = (1*4*16)^(1/3) = 64^(1/3) = 4
    query <- Axis("cell") |>
        LookupVector("positive") |>
        GeoMean()
    result <- get_query(daf, query)
    expect_equal(result, (1 * 4 * 16)^(1 / 3), tolerance = 1e-5)

    # Test with piping
    result <- get_query(daf, GeoMean(Axis("cell") |> LookupVector("positive")))
    expect_equal(result, (1 * 4 * 16)^(1 / 3), tolerance = 1e-5)
})

test_that("GeoMean operation works on matrices", {
    daf <- setup_test_data()

    # Test matrix reduction - geometric mean of all matrix values
    query <- Axis("cell") |>
        Axis("gene") |>
        LookupMatrix("data") |>
        GeoMean()
    result <- get_query(daf, query)
    expect_equal(result, (1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9)^(1 / 9), tolerance = 1e-5)
})

test_that("GeoMean operation with eps parameter", {
    daf <- setup_test_data()

    # Create a vector with a zero to test eps
    set_vector(daf, "cell", "with_zero", c(0, 4, 16))

    # GeoMean with eps adds eps to each value before computing, then subtracts eps from result
    query <- Axis("cell") |>
        LookupVector("with_zero") |>
        GeoMean(eps = 1)
    result <- get_query(daf, query)
    # With eps=1: geomean of (0+1, 4+1, 16+1) - 1 = (1*5*17)^(1/3) - 1
    expect_equal(result, (1 * 5 * 17)^(1 / 3) - 1, tolerance = 1e-5)
})

test_that("GeoMean operation with type parameter", {
    daf <- setup_test_data()

    set_vector(daf, "cell", "positive", c(1, 4, 16))
    query <- Axis("cell") |>
        LookupVector("positive") |>
        GeoMean(type = "Float64")
    result <- get_query(daf, query)
    expect_equal(result, (1 * 4 * 16)^(1 / 3), tolerance = 1e-5)
})

test_that("Mode operation works on vectors", {
    daf <- memory_daf(name = "mode_test!")
    add_axis(daf, "item", c("A", "B", "C", "D", "E"))
    set_vector(daf, "item", "category", c("X", "Y", "X", "X", "Y"))

    # Test Mode - most common value should be "X" (appears 3 times vs 2)
    query <- Axis("item") |>
        LookupVector("category") |>
        Mode()
    result <- get_query(daf, query)
    expect_equal(result, "X")

    # Test with piping
    result <- get_query(daf, Mode(Axis("item") |> LookupVector("category")))
    expect_equal(result, "X")
})

test_that("Mode operation works on numeric vectors", {
    daf <- memory_daf(name = "mode_numeric_test!")
    add_axis(daf, "item", c("A", "B", "C", "D", "E"))
    set_vector(daf, "item", "vals", c(1, 2, 2, 3, 2))

    query <- Axis("item") |>
        LookupVector("vals") |>
        Mode()
    result <- get_query(daf, query)
    expect_equal(result, 2)
})

test_that("Mode operation validates inputs correctly", {
    expect_error(
        Mode(1, 2)
    )
})

test_that("Mode query string format works", {
    daf <- memory_daf(name = "mode_string_test!")
    add_axis(daf, "item", c("A", "B", "C", "D", "E"))
    set_vector(daf, "item", "vals", c(1, 2, 2, 3, 2))
    result <- get_query(daf, "@ item : vals >> Mode")
    expect_equal(result, 2)
})

# Test type parameter on existing operations

test_that("Abs operation with type parameter works", {
    daf <- setup_test_data()

    query <- Axis("cell") |>
        LookupVector("values") |>
        Abs(type = "Float32")
    result <- get_query(daf, query)
    expect_equal(result, c(A = 1.5, B = 0, C = 2.5), tolerance = 1e-5)
})

test_that("Sum operation with type parameter works", {
    daf <- setup_test_data()

    query <- Axis("cell") |>
        LookupVector("values") |>
        Sum(type = "Float64")
    result <- get_query(daf, query)
    expect_equal(result, sum(c(-1.5, 0, 2.5)))
})

test_that("Mean operation with type parameter works", {
    daf <- setup_test_data()

    query <- Axis("cell") |>
        LookupVector("values") |>
        Mean(type = "Float32")
    result <- get_query(daf, query)
    expect_equal(result, sum(c(-1.5, 0, 2.5)) / 3, tolerance = 1e-5)
})

test_that("Median operation with type parameter works", {
    daf <- setup_test_data()

    query <- Axis("cell") |>
        LookupVector("values") |>
        Median(type = "Float64")
    result <- get_query(daf, query)
    expect_equal(result, 0)
})

test_that("Quantile operation with type parameter works", {
    daf <- setup_test_data()

    query <- Axis("cell") |>
        LookupVector("values") |>
        Quantile(p = 0.5, type = "Float64")
    result <- get_query(daf, query)
    expect_equal(result, 0)
})

test_that("Std operation with type parameter works", {
    daf <- setup_test_data()

    query <- Axis("cell") |>
        LookupVector("values") |>
        Std(type = "Float64")
    result <- get_query(daf, query)
    expect_equal(result, sd_uncorrected(c(-1.5, 0, 2.5)), tolerance = 1e-5)
})

test_that("Var operation with type parameter works", {
    daf <- setup_test_data()

    query <- Axis("cell") |>
        LookupVector("values") |>
        Var(type = "Float64")
    result <- get_query(daf, query)
    expect_equal(result, var_uncorrected(c(-1.5, 0, 2.5)), tolerance = 1e-5)
})

test_that("Fraction operation with type parameter works", {
    daf <- setup_test_data()

    set_vector(daf, "cell", "positive", c(2, 3, 5))
    query <- Axis("cell") |>
        LookupVector("positive") |>
        Fraction(type = "Float32")
    result <- get_query(daf, query)
    expect_equal(result, c(A = 2 / 10, B = 3 / 10, C = 5 / 10), tolerance = 1e-5)
})

test_that("Round operation with type parameter works", {
    daf <- setup_test_data()

    set_vector(daf, "cell", "decimals", c(1.4, 2.5, 3.6))
    query <- Axis("cell") |>
        LookupVector("decimals") |>
        Round(type = "Int32")
    result <- get_query(daf, query)
    expect_equal(result, c(A = 1, B = 2, C = 4))
})

test_that("Clamp operation with type parameter works", {
    daf <- setup_test_data()

    query <- Axis("cell") |>
        LookupVector("values") |>
        Clamp(min = -1, max = 1, type = "Float32")
    result <- get_query(daf, query)
    expect_equal(result, c(A = -1, B = 0, C = 1), tolerance = 1e-5)
})

test_that("Log operation with type parameter works", {
    daf <- setup_test_data()

    set_vector(daf, "cell", "positive", c(1, 10, 100))
    query <- Axis("cell") |>
        LookupVector("positive") |>
        Log(base = 10, type = "Float32")
    result <- get_query(daf, query)
    expect_equal(result, c(A = log10(1), B = log10(10), C = log10(100)), tolerance = 1e-5)
})

test_that("StdN operation with eps parameter works", {
    daf <- setup_test_data()

    set_vector(daf, "cell", "positive", c(2, 3, 7))

    # Test StdN with eps parameter
    query <- Axis("cell") |>
        LookupVector("positive") |>
        StdN(eps = 1e-6)
    result <- get_query(daf, query)
    expected <- sd_uncorrected(c(2, 3, 7)) / (mean(c(2, 3, 7)) + 1e-6)
    expect_equal(result, expected, tolerance = 1e-3)
})

test_that("VarN operation with eps parameter works", {
    daf <- setup_test_data()

    set_vector(daf, "cell", "positive", c(2, 3, 7))

    # Test VarN with eps parameter
    query <- Axis("cell") |>
        LookupVector("positive") |>
        VarN(eps = 1e-6)
    result <- get_query(daf, query)
    expected <- var_uncorrected(c(2, 3, 7)) / (mean(c(2, 3, 7)) + 1e-6)
    expect_equal(result, expected, tolerance = 1e-3)
})

test_that("StdN operation with type parameter works", {
    daf <- setup_test_data()

    set_vector(daf, "cell", "positive", c(2, 3, 7))

    query <- Axis("cell") |>
        LookupVector("positive") |>
        StdN(type = "Float64")
    result <- get_query(daf, query)
    expect_equal(result, sd_uncorrected(c(2, 3, 7)) / mean(c(2, 3, 7)), tolerance = 1e-5)
})

test_that("VarN operation with type parameter works", {
    daf <- setup_test_data()

    set_vector(daf, "cell", "positive", c(2, 3, 7))

    query <- Axis("cell") |>
        LookupVector("positive") |>
        VarN(type = "Float64")
    result <- get_query(daf, query)
    expect_equal(result, var_uncorrected(c(2, 3, 7)) / mean(c(2, 3, 7)), tolerance = 1e-5)
})
