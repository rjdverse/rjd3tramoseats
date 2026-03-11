# SEATS Decomposition

SEATS Decomposition

## Usage

``` r
seats_decompose(
  sarima,
  seas.tolerance = 2,
  trend.boundary = 0.5,
  seas.boundary = 0.8,
  seas.boundary.unique = 0.8,
  approximation = c("None", "Legacy", "Noisy")
)
```

## Arguments

- sarima:

  SARIMA model (see
  [`rjd3toolkit::sarima_model()`](https://rjdverse.github.io/rjd3toolkit/reference/sarima_model.html)).

- seas.tolerance:

  numeric: the seasonal tolerance (epsphi). The tolerance (measured in
  degrees) to allocate the AR non-real roots to the seasonal component
  (if the modulus of the inverse complex AR root is greater than the
  trend boundary and the frequency of this root differs from one of the
  seasonal frequencies by less than Seasonal tolerance) or the
  transitory component (otherwise). Possible values in \[0,10\]. Default
  value 2.

- trend.boundary:

  numeric: the trend boundary (rmod). The boundary beyond which an AR
  root is integrated in the trend component. If the modulus of the
  inverse real root is greater than the trend boundary, the AR root is
  integrated in the trend component. Below this value, the root is
  integrated in the transitory component. Possible values \[0,1\].
  Default = 0.5.

- seas.boundary:

  numeric: the seasonal boundary (sbound). The boundary beyond which a
  real negative AR root is integrated in the seasonal component. If the
  modulus of the inverse negative real root is greater (or equal) than
  Seasonal boundary, the AR root is integrated into the seasonal
  component. Otherwise the root is integrated into the trend or
  transitory component. Possible values \[0,1\]. Default=0.8.

- seas.boundary.unique:

  numeric: the seasonal boundary (unique), (sboundatpi). The boundary
  beyond which a negative AR root is integrated in the seasonal
  component, when the root is the unique seasonal root. If the modulus
  of the inverse negative real root is greater (or equal) than Seasonal
  boundary, the AR root is integrated into the seasonal component.
  Otherwise the root is integrated into the trend or transitory
  component. Possible values \[0,1\]. Default=0.8.

- approximation:

  character: the approximation mode. When the ARIMA model estimated by
  TRAMO does not accept an admissible decomposition, SEATS: `"None"` -
  performs an approximation; `"Legacy"` - replaces the model with a
  decomposable one; `"Noisy"` - estimates a new model by adding a white
  noise to the non-admissible model estimated by TRAMO.
  Default="Legacy".

## Value

returns a "JD3_UCARIMA" object

## Examples

``` r
if (FALSE) { # rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
seats_decompose(rjd3toolkit::sarima_model(period = 12, phi = c(0, 1), bd = 1))
}
```
