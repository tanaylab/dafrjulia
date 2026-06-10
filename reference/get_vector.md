# Get vector from a Daf object

Retrieves a vector property with the specified name for a given axis.

## Usage

``` r
get_vector(daf, axis, name, default = NULL)
```

## Arguments

- daf:

  A Daf object

- axis:

  Axis name

- name:

  Name of the vector property

- default:

  Default value if vector doesn't exist (NULL by default)

## Value

A named vector containing the property values, with names set to the
axis entry names, or the default value if the property doesn't exist

## Details

Vector properties store one-dimensional data along an axis, with one
value for each entry in the axis. If the vector doesn't exist and
default is NA, a vector of NAs with appropriate length is returned.

For zero-copy-eligible element types (numeric/integer), the returned
vector is a live view over the Julia-side memory (via `jlview`).
Mutating the Daf through
[`set_vector()`](https://tanaylab.github.io/dafrjulia/reference/set_vector.md)
or file-mapped writes may change the contents of a previously returned
vector. Call `unname(as.vector(v))` or an explicit `as.numeric(v)` to
take a detached copy when stability across modifications is required.
See the Julia
[documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.2.0/readers.html#DataAxesFormats.Readers.get_vector)
for details.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_daf()
daf <- memory_daf("example")
add_axis(daf, "cell", c("A", "B", "C"))
set_vector(daf, "cell", "score", c(1.0, 2.0, 3.0))
get_vector(daf, "cell", "score") # named vector: A=1, B=2, C=3
} # }
```
