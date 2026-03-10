# Forecasts with TRAMO

Forecasts with TRAMO

## Usage

``` r
tramo_forecast(
  ts,
  spec = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"),
  nf = -1,
  context = NULL
)
```

## Arguments

- ts:

  a univariate time series.

- spec:

  the model specification. Can be either the name of a predefined
  specification or a user-defined specification.

- nf:

  the forecasting horizon (`numeric`). The forecast length is in periods
  (positive values) or years (negative values). By default, the program
  generates a one-year forecast (`nf = -1`).

- context:

  the dictionary of variables.

## Value

a `mts` object with 7 variables:

- `forecast` the forecast of the actual data at the end of the series.

- `error` standard deviation of the forecast.

- `fraw` the forecast of the transformed series.

- `efraw` the standard deviation of the forecast of the transformed
  series.

## Examples

``` r
if (FALSE) { # current_java_version >= minimal_java_version
# \donttest{
tramo_forecast(rjd3toolkit::ABS$X0.2.09.10.M)
# }
}
```
