
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjd3tramoseats

rjd3tramoseats offers full acces to options and outputs of TRAMO-SEATS
(`rjd3tramoseats::tramoseats()`), including TRAMO modelling
(`rjd3tramoseats::tramo()`) and SEATS decomposition
(`rjd3tramoseats::seats_decompose()`).

The specification can be created with the functions
`rjd3tramoseats::tramo_spec()` or `rjd3tramoseats::tramoseats_spec()`
and can be modified with the function:

- for the pre-processing: `rjd3toolkit::set_arima()`,
  `rjd3toolkit::set_automodel()`, `rjd3toolkit::set_basic()`,
  `rjd3toolkit::set_easter()`, `rjd3toolkit::set_estimate()`,
  `rjd3toolkit::set_outlier()`, `rjd3toolkit::set_tradingdays()`,
  `rjd3toolkit::set_transform()`, `rjd3toolkit::add_outlier()`,
  `rjd3toolkit::remove_outlier()`, `rjd3toolkit::add_ramp()`,
  `rjd3toolkit::remove_ramp()`, `rjd3toolkit::add_usrdefvar()`;

- for the decomposition: `rjd3tramoseats::set_seats()`;

- for the benchmarking: `rjd3toolkit::set_benchmarking()`.

## Installation

``` r
# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("rjdemetra/rjd3toolkit", "main")
remotes::install_github("rjdemetra/rjd3tramoseats", "main")
```
