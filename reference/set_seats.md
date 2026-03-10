# Set Seats Specification

Function allowing to customize parameters in the decomposition part
(Seats) of a Tramo-Seats seasonal adjustment process. (Seats is an Arima
Model Based decomposition algorithm working in conjunction with Tramo.)

## Usage

``` r
set_seats(
  x,
  approximation = c(NA, "None", "Legacy", "Noisy"),
  trend.boundary = NA,
  seas.boundary = NA,
  seas.boundary.unique = NA,
  seas.tolerance = NA,
  ma.boundary = NA,
  fcasts = NA,
  bcasts = NA,
  algorithm = c(NA, "Burman", "KalmanSmoother"),
  bias = NA
)
```

## Arguments

- x:

  the specification to be modified, object of class
  `"JD3_TRAMOSEATS_SPEC"`, has to be generated with
  [`tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.md)
  function

- approximation:

  character: the approximation mode. When the ARIMA model estimated by
  TRAMO does not accept an admissible decomposition, SEATS: `"None"` -
  performs an approximation; `"Legacy"` - replaces the model with a
  decomposable one; `"Noisy"` - estimates a new model by adding a white
  noise to the non-admissible model estimated by TRAMO.
  Default="Legacy".

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

- seas.tolerance:

  numeric: the seasonal tolerance (epsphi). The tolerance (measured in
  degrees) to allocate the AR non-real roots to the seasonal component
  (if the modulus of the inverse complex AR root is greater than the
  trend boundary and the frequency of this root differs from one of the
  seasonal frequencies by less than Seasonal tolerance) or the
  transitory component (otherwise). Possible values in \[0,10\]. Default
  value 2.

- ma.boundary:

  numeric: the MA unit root boundary. When the modulus of an estimated
  MA root falls in the range \[xl, 1\], it is set to xl. Possible values
  \[0.9,1\]. Default=0.95.

- bcasts, fcasts:

  numeric: the number of backasts (`bcasts`) or forecasts (`fcasts`)
  used in the decomposition in periods (positive values) or years
  (negative values). Default `bcasts` = 0. Default `fcasts` = 0.

- algorithm:

  character: the estimation method for the unobserved components. The
  choice can be made from:

  1.  **Burman**: the default value. May result in a significant
      underestimation of the components' standard deviation, as it may
      become numerically unstable when some roots of the MA polynomial
      are near 1;

  2.  **KalmanSmoother**: it is not disturbed by the (quasi-) unit roots
      in MA.

- bias:

  TODO.

## Value

an object of class `"JD3_TRAMOSEATS_SPEC"`.

## References

More information and examples related to 'JDemetra+' features in the
online documentation: <https://jdemetra-new-documentation.netlify.app/>

## See also

[`tramoseats_spec()`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats_spec.md).

## Examples

``` r
if (FALSE) { # current_java_version >= minimal_java_version
init_spec <- tramoseats_spec("rsafull")
new_spec <- set_seats(init_spec,
    approximation = "Legacy",
    trend.boundary = 0.8,
    seas.boundary = 0.5,
    fcasts = -3,
    algorithm = "KalmanSmoother",
    bias = TRUE
)
y <- rjd3toolkit::ABS$X0.2.09.10.M
# \donttest{
sa <- tramoseats(y, spec = new_spec)
# }
}
```
