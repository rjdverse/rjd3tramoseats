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
library("rjd3toolkit")

sp <- tramoseats_spec("rsafull")
y <- rjd3toolkit::ABS$X0.2.09.10.M
# \donttest{
tramoseats(y, spec = sp)
#> Serie span: All 
#> Model: TRAMO-SEATS
#> Log-transformation: yes 
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>   -0.8278   -0.4255 
#> 
#> Regression model:
#>             mon             tue             wed             thu             fri 
#>      -0.0109446       0.0048940       0.0001761       0.0132928      -0.0024801 
#>             sat              lp          easter AO (2000-06-01) AO (2000-07-01) 
#>       0.0153509       0.0410667       0.0503888       0.1681662      -0.1972348 
#> 
#> For a more detailed output, use the 'summary()' function.
tramoseats_fast(y, spec = sp)
#> Model: TRAMO-SEATS
#> Log-transformation: yes 
#> SARIMA model: (0,1,1) (0,1,1)
#> 
#> SARIMA coefficients:
#>  theta(1) btheta(1) 
#>   -0.8278   -0.4255 
#> 
#> Regression model:
#>             mon             tue             wed             thu             fri 
#>      -0.0109446       0.0048940       0.0001761       0.0132928      -0.0024801 
#>             sat              lp          easter AO (2000-06-01) AO (2000-07-01) 
#>       0.0153509       0.0410667       0.0503888       0.1681662      -0.1972348 
#> 
#> For a more detailed output, use the 'summary()' function.
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
#> Serie span: All 
#> Model: TRAMO-SEATS
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
tramoseats_fast(y, spec = sp)
#> Model: TRAMO-SEATS
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
