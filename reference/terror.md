# TERROR Quality Control of Outliers

TRAMO for ERRORs (TERROR) controls the quality of the data by checking
outliers at the end of the series

## Usage

``` r
terror(
  ts,
  spec = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"),
  nback = 1,
  context = NULL
)
```

## Arguments

- ts:

  a univariate time series.

- spec:

  the model specification. Can be either the name of a predefined
  specification or a user-defined specification.

- nback:

  number of last observations considered for the quality check.

- context:

  the dictionary of variables.

## Value

a `mts` object with 7 variables:

1.  **actual**: the actual data at the end of the series;

2.  **forecast**: the forecast of the actual data at the end of the
    series;

3.  **error**: the absolute errors (= observed - forecasts);

4.  **rel.error**: relative errors ("scores") : ratios between the
    forecast errors and the standard deviation of the forecasts of the
    last observations (positive values mean under-estimation);

5.  **raw**: the transformed series. More especially, if the chosen
    model implies a log-transformation, the values are obtained after a
    log-transformation. Other transformations, such leap year
    corrections or length-of periods corrections may also be used;

6.  **fraw**: the forecast of the transformed series.;

7.  **efraw**: the absolute errors of the transformed series.

## Examples

``` r
# \donttest{
terror(rjd3toolkit::ABS$X0.2.09.10.M, nback = 2)
#>          actual forecast     error rel. error transformed tr.fcast   tr.error
#> Jul 2017 1445.5 1468.903 -23.40336 -0.4328439    7.276211 7.292271 0.03710535
#> Aug 2017 1303.1 1341.213 -38.11282 -0.7664734    7.172501 7.201330 0.03761155
# }
```
