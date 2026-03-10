# TRAMO-SEATS dictionary

Functions to provide information for all output objects (series,
diagnostics, parameters) available with
[`tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
function.

## Usage

``` r
tramoseats_dictionary()

tramoseats_full_dictionary()
```

## Value

`tramoseats_dictionary()` returns a character vector containing the
names of all output objects (series, diagnostics, parameters) available
with the
[`tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
function, whereas `tramoseats_full_dictionary()` returns a `data.frame`
with format and description, for all the output objects.

## Details

These functions provide lists of output names (series, diagnostics,
parameters) available with the
[`tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
function. These names can be used to generate customized outputs with
the userdefined option of the
[`tramoseats()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
function (see examples). The `tramoseats_full_dictionary` function
provides additional information on object format and description.

## Examples

``` r
if (FALSE) { # current_java_version >= minimal_java_version
# Visualize the dictionary
print(tramoseats_dictionary())
summary(tramoseats_dictionary())

# first 10 lines
head(tramoseats_full_dictionary(), n = 10)
# For more structured information call `View(tramoseats_full_dictionary())`

# Extract names of output of interest
user_defined_output <- tramoseats_dictionary()[c(65, 95, 135)]
user_defined_output

# Generate the corresponding output in an estimation
y <- rjd3toolkit::ABS$X0.2.09.10.M
# \donttest{
m <- tramoseats(y, "rsafull", userdefined=user_defined_output)

# Retrieve user defined output
tail(m$user_defined$ylin)
m$user_defined$residuals.kurtosis
m$user_defined$sa_f
# }
}
```
