# TRAMO/TRAMO-SEATS Default Specification

Set of functions(`tramoseats_spec()`,`tramo_spec()`) to create default
specifications associated with the TRAMO-SEATS seasonal adjustment
method. Specification creation can be restricted to the tramo part with
the `tramo_spec()` function.

## Usage

``` r
tramo_spec(name = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"))

tramoseats_spec(
  name = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
)
```

## Arguments

- name:

  the name of a predefined specification.

## Value

an object of class `"JD3_TRAMOSEATS_SPEC"` (`tramoseats_spec()`) or
`"JD3_TRAMO_SPEC"` (`tramo_spec()`).

## Details

Without argument `tramo_spec()` yields a TR5 specification

without argument `tramoseats_spec()` yields a RSA5 specification

The available predefined 'JDemetra+' model specifications are described
in the table below:

|                   |                            |                           |                         |                       |              |                |              |
|-------------------|----------------------------|---------------------------|-------------------------|-----------------------|--------------|----------------|--------------|
| **Identifier** \| | **Log/level detection** \| | **Outliers detection** \| | **Calendar effects** \| | **ARIMA**             | RSA0/TR0 \|  | *NA* \|        | *NA* \|      |
| *NA* \|           | Airline(+mean)             | RSA1/TR1 \|               | automatic \|            | AO/LS/TC \|           | *NA* \|      | Airline(+mean) | RSA2/TR2 \|  |
| automatic \|      | AO/LS/TC \|                | 2 td vars + Easter \|     | Airline(+mean)          | RSA3/TR3 \|           | automatic \| | AO/LS/TC \|    | *NA* \|      |
| automatic         | RSA4/TR3 \|                | automatic \|              | AO/LS/TC \|             | 2 td vars + Easter \| | automatic    | RSA5/TR5 \|    | automatic \| |
| AO/LS/TC \|       | 7 td vars + Easter \|      | automatic                 | RSAfull/TRfull \|       | automatic \|          | AO/LS/TC \|  | automatic \|   | automatic    |

## See also

1.  To set the pre-processing parameters:
    [`rjd3toolkit::set_arima()`](https://rjdverse.github.io/rjd3toolkit/reference/set_arima.html),
    [`rjd3toolkit::set_automodel()`](https://rjdverse.github.io/rjd3toolkit/reference/set_automodel.html),
    [`rjd3toolkit::set_basic()`](https://rjdverse.github.io/rjd3toolkit/reference/set_basic.html),
    [`rjd3toolkit::set_easter()`](https://rjdverse.github.io/rjd3toolkit/reference/set_easter.html),
    [`rjd3toolkit::set_estimate()`](https://rjdverse.github.io/rjd3toolkit/reference/set_estimate.html),
    [`rjd3toolkit::set_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/set_outlier.html),
    [`rjd3toolkit::set_tradingdays()`](https://rjdverse.github.io/rjd3toolkit/reference/set_tradingdays.html),
    [`rjd3toolkit::set_transform()`](https://rjdverse.github.io/rjd3toolkit/reference/set_transform.html),
    [`rjd3toolkit::add_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html),
    [`rjd3toolkit::remove_outlier()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html),
    [`rjd3toolkit::add_ramp()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html),
    [`rjd3toolkit::remove_ramp()`](https://rjdverse.github.io/rjd3toolkit/reference/add_outlier.html),
    [`rjd3toolkit::add_usrdefvar()`](https://rjdverse.github.io/rjd3toolkit/reference/add_usrdefvar.html);

2.  To set the decomposition parameters:
    [`set_seats()`](https://rjdverse.github.io/rjd3tramoseats/reference/set_seats.md);

3.  To set the benchmarking parameters:
    [`rjd3toolkit::set_benchmarking()`](https://rjdverse.github.io/rjd3toolkit/reference/set_benchmarking.html).

## Examples

``` r
if (FALSE) { # rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
init_spec <- tramoseats_spec()
init_spec <- tramo_spec()
init_spec <- tramoseats_spec("rsa3")
init_spec <- tramo_spec("tr3")
}
```
