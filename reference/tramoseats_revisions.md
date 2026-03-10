# Revisions History

Computes revisions history

## Usage

``` r
tramoseats_revisions(
  ts,
  spec,
  data_ids = NULL,
  ts_ids = NULL,
  cmp_ids = NULL,
  context = NULL
)
```

## Arguments

- ts:

  The time series used for the estimation.

- spec:

  The specification used.

- data_ids:

  A `list` of `list` to specify the statistics to export. Each sub-list
  must contain two elements: `start` (first date to compute the history,
  in the format `"YYYY-MM-DD"`) and `id` (the name of the statistics,
  see
  [`tramoseats_dictionary()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_dictionary.md)).
  See example.

- ts_ids:

  A `list` of `list` to specify the specific date of a component whose
  history is to be studied. Each sub-list must contain three elements:
  `start` (first date to compute the history, in the format
  `"YYYY-MM-DD"`), `period` (the date of the studied) and `id` (the name
  of the component, see
  [`tramoseats_dictionary()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_dictionary.md)).
  See example.

- cmp_ids:

  A `list` of `list` to specify the component whose history is to be
  studied. Each sub-list must contain three elements: `start` (first
  date to compute the history, in the format `"YYYY-MM-DD"`), `end`
  (last date to compute the history, in the format `"YYYY-MM-DD"`) and
  `id` (the name of the component, see
  [`tramoseats_dictionary()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_dictionary.md)).
  As many series as periods between `start` and `end` will be exported.
  See example.

- context:

  The context of the specification.

## Value

returns a list

## Examples

``` r
if (FALSE) { # current_java_version >= minimal_java_version
# \donttest{
s <- rjd3toolkit::ABS$X0.2.09.10.M
sa_mod <- tramoseats(s)
data_ids <- list(
    # Get the coefficient of the trading-day coefficient from 2005-jan
    list(start = "2005-01-01", id = "regression.td(1)"),
    # Get the ljung-box statistics on residuals from 2010-jan
    list(start = "2010-01-01", id = "residuals.lb")
)
ts_ids <- list(
    # Get the SA component estimates of 2010-jan from 2010-jan
    list(period = "2010-01-01", start = "2010-01-01", id = "sa"),
    # Get the irregular component estimates of 2010-jan from 2015-jan
    list(period = "2010-01-01", start = "2015-01-01", id = "i")
)
cmp_ids <- list(
    # Get the SA component estimates (full time series) 2010-jan to 2020-jan
    list(start = "2010-01-01", end = "2020-01-01", id = "sa"),
    # Get the trend component estimates (full time series)  2010-jan to 2020-jan
    list(start = "2010-01-01", end = "2020-01-01", id = "t")
)
rh <- tramoseats_revisions(s, sa_mod$result_spec, data_ids, ts_ids, cmp_ids)
# }
}
```
