# Outlier Detection with a Tramo Model

Tramo is a particular regarima model estimation algorithm, mainly used
to linearized the series before performing a decomposition with Seats

## Usage

``` r
tramo_outliers(
  y,
  order = c(0L, 1L, 1L),
  seasonal = c(0L, 1L, 1L),
  mean = FALSE,
  X = NULL,
  X.td = NULL,
  ao = TRUE,
  ls = TRUE,
  tc = FALSE,
  so = FALSE,
  cv = 0,
  ml = FALSE,
  clean = FALSE
)
```

## Arguments

- y:

  the dependent variable (a `ts` object).

- order, seasonal:

  the orders of the ARIMA model.

- mean:

  Boolean to include or not the mean.

- X:

  user defined regressors (other than calendar).

- X.td:

  calendar regressors.

- ao, ls, so, tc:

  Boolean to indicate which type of outliers should be detected.

- cv:

  `numeric`. The entered critical value for the outliers' detection
  procedure. If equal to 0 the critical value for the outliers'
  detection procedure is automatically determined by the number of
  observations.

- ml:

  Use of maximum likelihood (otherwise approximation by means of
  Hannan-Rissanen).

- clean:

  Clean missing values at the beginning/end of the series. Regression
  variables are automatically resized, if need be.

## Value

a `"JD3_REGARIMA_OUTLIERS"` object.

## Examples

``` r
if (FALSE) { # rjd3jars::check_java_version(silent = TRUE)
tramo_outliers(rjd3toolkit::ABS$X0.2.09.10.M)
}
```
