# TRAMO model, pre-adjustment in TRAMO-SEATS

allows to model the series with a Reg-Arima model, estimate outlier,
calendar or other regression effects and produce forecasts

## Usage

``` r
tramo(
  ts,
  spec = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"),
  context = NULL,
  userdefined = NULL
)

tramo_fast(
  ts,
  spec = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"),
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

the `tramo()` function returns a list with the results
(`"JD3_tramo_rslts"` object), the estimation specification and the
result specification, while `tramo_fast()` is a faster function that
only returns the results.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
library("rjd3toolkit")

y <- rjd3toolkit::ABS$X0.2.09.10.M
sp <- tramo_spec("trfull")
sp <- add_outlier(sp,
    type = c("AO"), c("2015-01-01", "2010-01-01")
)
# \donttest{
tramo_fast(y, spec = sp)
# }
sp <- set_transform(
    set_tradingdays(
        set_easter(sp, enabled = FALSE),
        option = "workingdays"
    ),
    fun = "None"
)
# \donttest{
tramo_fast(y, spec = sp)
# }
sp <- set_outlier(sp, outliers.type = c("AO"))

# \donttest{
tramo_fast(y, spec = sp)
# }
}
```
