# Seasonal Adjustment with TRAMO-SEATS

Seasonal Adjustment with TRAMO-SEATS

## Usage

``` r
tramoseats(
  ts,
  spec = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"),
  context = NULL,
  userdefined = NULL
)

tramoseats_fast(
  ts,
  spec = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"),
  context = NULL,
  userdefined = NULL
)

.jtramoseats(
  ts,
  spec = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"),
  context = NULL,
  userdefined = NULL
)
```

## Arguments

- ts:

  a univariate time series.

- spec:

  the model specification. Can be either the name of a predefined
  specification or a user-defined specification.

- context:

  the dictionary of variables.

- userdefined:

  a vector containing the additional output variables (see
  [`tramoseats_dictionary()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_dictionary.md)).

## Value

The `tramoseats()` function returns a list with the results, the
estimation specification and the result specification, while
`tramoseats_fast()` is a faster function that only returns the results.
The `.jtramoseats()` functions only results the java object to custom
outputs in other packages (use
[`rjd3toolkit::dictionary()`](https://rjdverse.github.io/rjd3toolkit/reference/dictionary.html)
to get the list of variables and
[`rjd3toolkit::result()`](https://rjdverse.github.io/rjd3toolkit/reference/dictionary.html)
to get a specific variable).

## Examples

``` r
if (FALSE) { # rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
library("rjd3toolkit")

sp <- tramoseats_spec("rsafull")
y <- rjd3toolkit::ABS$X0.2.09.10.M
# \donttest{
tramoseats(y, spec = sp)
tramoseats_fast(y, spec = sp)
# }
sp <- add_outlier(sp,
    type = c("AO"), c("2015-01-01", "2010-01-01")
)
sp <- set_transform(
    set_tradingdays(
        set_easter(sp, enabled = FALSE),
        option = "workingdays"
    ),
    fun = "None"
)
# \donttest{
tramoseats(y, spec = sp)
tramoseats_fast(y, spec = sp)
# }
}
```
