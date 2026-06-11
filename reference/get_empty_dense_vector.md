# Get an empty dense vector for filling

Returns an empty dense vector for the specified axis and property, which
can be filled in-place. This is useful for efficiently constructing
large vectors without allocating temporary storage. After filling, the
vector is automatically stored in the Daf object.

## Usage

``` r
get_empty_dense_vector(daf, axis, name, eltype, overwrite = FALSE)
```

## Arguments

- daf:

  A Daf object

- axis:

  Name of the axis

- name:

  Name of the vector property

- eltype:

  Element type for the vector (e.g., "Float64", "Int32")

- overwrite:

  Whether to overwrite if vector already exists (FALSE by default)

## Value

A Julia vector object backed by the (uninitialized) property storage

## Details

See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/writers.html#DataAxesFormats.Writers.get_empty_dense_vector!)
for details.
