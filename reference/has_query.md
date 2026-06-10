# Check if a query can be applied to a Daf object

Determines whether a query can be validly applied to a Daf object. This
is useful for checking if the properties referenced in a query exist in
the Daf object before attempting to execute the query.

## Usage

``` r
has_query(daf, query)
```

## Arguments

- daf:

  A Daf object

- query:

  Query string or object. Can be created using query operations such as
  Axis(), LookupVector(), IsGreater(), etc.

## Value

TRUE if query can be applied, FALSE otherwise

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/queries.html#DataAxesFormats.Operations.has_query)
for details.

## See also

[`get_query()`](https://tanaylab.github.io/dafrjulia/reference/get_query.md),
[`parse_query()`](https://tanaylab.github.io/dafrjulia/reference/parse_query.md),
and the query operations documentation.
