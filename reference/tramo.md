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
library("rjd3toolkit")
#> 
#> Attaching package: ‘rjd3toolkit’
#> The following object is masked from ‘package:rjd3tramoseats’:
#> 
#>     current_java_version
#> The following objects are masked from ‘package:stats’:
#> 
#>     aggregate, mad

y <- rjd3toolkit::ABS$X0.2.09.10.M
sp <- tramo_spec("trfull")
sp <- add_outlier(sp,
    type = c("AO"), c("2015-01-01", "2010-01-01")
)
# \donttest{
tramo_fast(y, spec = sp)
#> Log-transformation: yes 
#> SARIMA model: (2,1,2) (0,1,1)
#> 
#> SARIMA coefficients:
#>    phi(1)    phi(2)  theta(1)  theta(2) btheta(1) 
#>  -0.09251   0.12325  -1.02295   0.24537  -0.43575 
#> 
#> Regression model:
#>             mon             tue             wed             thu             fri 
#>      -1.131e-02       5.831e-03      -8.502e-05       1.293e-02      -2.076e-03 
#>             sat              lp          easter AO (2010-01-01) AO (2015-01-01) 
#>       1.535e-02       3.881e-02       5.207e-02       3.674e-02      -9.888e-03 
#> AO (2000-06-01) AO (2000-07-01) 
#>       1.736e-01      -1.831e-01 
#> 
#> For a more detailed output, use the 'summary()' function.
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
#> Log-transformation: no 
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>   -0.8235   -0.2608 
#> 
#> Regression model:
#>             mon             tue             wed             thu             fri 
#>        -11.7873          0.2507          3.0039         12.8309         -5.4519 
#>             sat              lp AO (2010-01-01) AO (2015-01-01) AO (2000-06-01) 
#>         17.2998         33.6083         40.5331        -10.7096        192.7411 
#> AO (2000-07-01) AO (2005-04-01) 
#>       -200.7316       -177.1356 
#> 
#> For a more detailed output, use the 'summary()' function.
# }
sp <- set_outlier(sp, outliers.type = c("AO"))

# \donttest{
tramo_fast(y, spec = sp)
#> Log-transformation: no 
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>   -0.8235   -0.2608 
#> 
#> Regression model:
#>             mon             tue             wed             thu             fri 
#>        -11.7873          0.2507          3.0039         12.8309         -5.4519 
#>             sat              lp AO (2010-01-01) AO (2015-01-01) AO (2000-06-01) 
#>         17.2998         33.6083         40.5331        -10.7096        192.7411 
#> AO (2000-07-01) AO (2005-04-01) 
#>       -200.7316       -177.1356 
#> 
#> For a more detailed output, use the 'summary()' function.
# }
```
