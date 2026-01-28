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
# \donttest{
tramo_forecast(rjd3toolkit::ABS$X0.2.09.10.M)
#>          forecast     error     fraw      efraw
#> Sep 2017 1394.524  51.63894 7.240309 0.03699180
#> Oct 2017 1465.852  55.09481 7.290192 0.03754581
#> Nov 2017 1719.851  65.39924 7.449993 0.03798498
#> Dec 2017 2766.983 107.04362 7.925513 0.03864273
#> Jan 2018 1460.928  57.22647 7.286827 0.03912637
#> Feb 2018 1090.905  43.08353 6.994763 0.03944732
#> Mar 2018 1447.182  58.38462 7.277374 0.04029456
#> Apr 2018 1395.393  57.07914 7.240932 0.04085424
#> May 2018 1482.589  60.88645 7.301545 0.04101588
#> Jun 2018 1556.902  64.92067 7.350453 0.04164443
#> Jul 2018 1433.030  60.29701 7.267546 0.04202090
#> Aug 2018 1302.717  55.47047 7.172207 0.04252290
# }
```
