# Refresh a specification with constraints

Functions `tramoseats_refresh()` and `tramo_refresh()` allow to create a
new specification by updating a specification used for a previous
estimation. Some selected parameters will be kept fixed (previous
estimation results) while others will be freed for re-estimation in a
domain of constraints. See details and examples.

## Usage

``` r
tramo_refresh(
  spec,
  refspec = NULL,
  policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
    "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"),
  period = 0,
  start = NULL,
  end = NULL
)

tramoseats_refresh(
  spec,
  refspec = NULL,
  policy = c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers",
    "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"),
  period = 0,
  start = NULL,
  end = NULL
)
```

## Arguments

- spec:

  the current specification to be refreshed (`"result_spec"`).

- refspec:

  the reference specification used to define the domain considered for
  re-estimation (`"domain_spec"`). By default this is the `"TRfull"` or
  `"RSAfull"` specification.

- policy:

  the refresh policy to apply (see details).

- period, start, end:

  additional parameters used to specify the span on which additive
  outliers (AO) are introduced when `policy = "Current"` or to specify
  the span on which outliers will be re-detected when
  `policy = "Outliers"` or `policy = "Outliers_StochasticComponent"`, is
  this case `end` is unused. If `start` is not specified, outliers will
  be re-identified on the whole series. Span definition: `period`:
  numeric, number of observations in a year (12, 4...). `start` and
  `end`: defined as arrays of two elements: year and first period (for
  example, `period = 12` and `c(1980, 1)` stands for January 1980) The
  dates corresponding `start` and `end` are included in the span
  definition.

## Value

a new specification, an object of class `"JD3_TRAMOSEATS_SPEC"` or
`"JD3_TRAMO_SPEC"`.

## Details

The selection of constraints to be kept fixed or re-estimated is called
a revision policy. User-defined parameters are always copied to the new
refreshed specifications. This revision applies to the estimation done
in Tramo (pre-adjustment phase), Seats will then run a new decomposition
which might be in some (rare) cases based on a different model.

Available refresh policies are:

**Current**: applying the current pre-adjustment reg-arima model and
handling the new raw data points, or any sub-span of the series as
Additive Outliers (defined as new intervention variables)

**Fixed**: applying the current pre-adjustment reg-arima model and
replacing forecasts by new raw data points.

**FixedParameters**: pre-adjustment reg-arima model is partially
modified: regression coefficients will be re-estimated but regression
variables, Arima orders and coefficients are unchanged.

**FixedAutoRegressiveParameters**: same as FixedParameters but Arima
Moving Average coefficients (MA) are also re-estimated, Auto-regressive
(AR) coefficients are kept fixed.

**FreeParameters**: all regression and Arima model coefficients are
re-estimated, regression variables and Arima orders are kept fixed.

**Outliers**: regression variables and Arima orders are kept fixed, but
outliers will be re-detected on the defined span, thus all regression
and Arima model coefficients are re-estimated

**Outliers_StochasticComponent**: same as "Outliers" but Arima model
orders (p,d,q)(P,D,Q) can also be re-identified.

**Complete**: All the parameters are re-identified and re-estimated,
unless constrained in the domain spec.

## References

More information on revision policies in JDemetra+ online documentation:
<https://jdemetra-new-documentation.netlify.app/a-rev-policies>

## Examples

``` r
if (FALSE) { # rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
# \donttest{
y <- rjd3toolkit::ABS$X0.2.08.10.M
# raw series for first estimation
y_raw <- window(y, end = c(2016, 12))
# raw series for second (refreshed) estimation
y_new <- window(y, end = c(2017, 6))

# specification for first estimation
spec_tramoseats_1 <- tramoseats_spec("rsafull")

# first estimation
sa_tramoseats <- tramoseats(y_raw, spec_tramoseats_1)
# refreshing the specification
current_result_spec <- sa_tramoseats$result_spec
current_domain_spec <- sa_tramoseats$estimation_spec

# policy = "Fixed"
spec_tramoseats_ref <- tramoseats_refresh(current_result_spec, # point spec to be refreshed
    current_domain_spec, # domain spec (set of constraints)
    policy = "Fixed"
)

# 2nd estimation with refreshed specification
sa_tramoseats_ref <- tramoseats(y_new, spec_tramoseats_ref)
# policy = "Outliers"
spec_tramoseats_ref <- tramoseats_refresh(current_result_spec,
    current_domain_spec,
    policy = "Outliers",
    period = 12,
    start = c(2017, 1)
) # outliers will be re-detected from January 2017 included
# 2nd estimation with refreshed specification
sa_tramoseats_ref <- tramoseats(y_new, spec_tramoseats_ref)

# policy = "Current"
spec_tramoseats_ref <- tramoseats_refresh(current_result_spec,
    current_domain_spec,
    policy = "Current",
    period = 12,
    start = c(2017, 1),
    end = end(y_new)
)
# points from January 2017 (included) until the end of the series will be treated
# as Additive Outliers, the previous reg-Arima model being otherwise kept fixed

# 2nd estimation with refreshed specification
sa_tramoseats_ref <- tramoseats(y_new, spec_tramoseats_ref) #'
# same procedure using tramo_refresh
# specification for first estimation
spec_1 <- tramo_spec("tr3")

# first estimation
tramo_model <- tramo(y_raw, spec_1)
tramo_model$estimation_spec

# refreshing the specification
current_result_spec <- tramo_model$result_spec
current_domain_spec <- tramo_model$estimation_spec

# policy = "Fixed"
spec_1_ref <- tramo_refresh(current_result_spec, # point spec to be refreshed
                             current_domain_spec, # domain spec (set of constraints)
                              policy = "Fixed"
                               )

# 2nd estimation with refreshed specification
tramo_model_ref <- tramo(y_new, spec_1_ref)
# }
}
```
