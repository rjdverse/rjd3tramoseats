# TRAMO-SEATS Dictionary

Function providing the names all output objects (series, diagnostics,
parameters) available with
[`tramoseats( )`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
function. Can be used to generate an output non available by default
with userdefined option in
[`tramoseats( )`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)function
(see examples).

## Usage

``` r
tramoseats_dictionary()
```

## Value

returns a vector containing the names of all output objects (series,
diagnostics, parameters) available with
[`tramoseats( )`](https://rjdverse.github.io/rjd3tramoseats/reference/tramoseats.md)
function.

## See also

`tramoseats_full_dictionary` for a detailed version of the output
description

## Examples

``` r
# visualize the list of names
summary(tramoseats_dictionary())
#> List of possible outputs:
#> 
#> - period
#> - span.start
#> - span.end
#> - span.n
#> - span.missing
#> - log
#> - adjust
#> - likelihood.ll
#> - likelihood.adjustedll
#> - likelihood.ssqerr
#> - likelihood.aic
#> - likelihood.bic
#> - likelihood.aicc
#> - likelihood.bicc
#> - likelihood.bic2
#> - likelihood.hannanquinn
#> - likelihood.nparams
#> - likelihood.nobs
#> - likelihood.neffectiveobs
#> - likelihood.df
#> - arima.p
#> - arima.d
#> - arima.q
#> - arima.bp
#> - arima.bd
#> - arima.bq
#> - arima.theta(*)
#> - arima.phi(*)
#> - arima.btheta(*)
#> - arima.bphi(*)
#> - regression.espan.start
#> - regression.espan.end
#> - regression.espan.n
#> - regression.espan.missing
#> - regression.mean
#> - regression.nlp
#> - regression.ntd
#> - regression.leaster
#> - regression.nmh
#> - regression.nout
#> - regression.nao
#> - regression.nls
#> - regression.ntc
#> - regression.nso
#> - regression.nusers
#> - regression.mu
#> - regression.lp
#> - regression.td(*)
#> - regression.td-derived
#> - regression.td-ftest
#> - regression.easter
#> - regression.outlier(*)
#> - regression.user(*)
#> - regression.missing(*)
#> - residuals.res
#> - residuals.tsres
#> - residuals.n
#> - residuals.df
#> - residuals.dfc
#> - residuals.ser
#> - residuals.ser_ml
#> - residuals.type
#> - residuals.mean
#> - residuals.skewness
#> - residuals.kurtosis
#> - residuals.doornikhansen
#> - residuals.lb
#> - residuals.bp
#> - residuals.lb2
#> - residuals.bp2
#> - residuals.seaslb
#> - residuals.seasbp
#> - residuals.nruns
#> - residuals.lruns
#> - residuals.nudruns
#> - residuals.ludruns
#> - regression.ml.parameters
#> - regression.ml.pcovar
#> - regression.ml.pcovar-ml
#> - regression.ml.pcorr
#> - regression.ml.pscore
#> - regression.details.description
#> - regression.details.type
#> - regression.details.coefficients
#> - regression.details.covar
#> - regression.details.covar-ml
#> - y
#> - y_f(?)
#> - y_ef(?)
#> - y_b(?)
#> - y_eb(?)
#> - yc
#> - yc_f(?)
#> - yc_b(?)
#> - ylin
#> - ylin_f(?)
#> - ylin_b(?)
#> - det
#> - det_f(?)
#> - det_b(?)
#> - cal
#> - cal_f(?)
#> - cal_b(?)
#> - ycal
#> - ycal_f(?)
#> - ycal_b(?)
#> - tde
#> - tde_f(?)
#> - tde_b(?)
#> - ee
#> - ee_f(?)
#> - ee_b(?)
#> - omhe
#> - omhe_f(?)
#> - omhe_b(?)
#> - mhe
#> - mhe_f(?)
#> - mhe_b(?)
#> - out
#> - out_f(?)
#> - out_b(?)
#> - reg
#> - reg_f(?)
#> - reg_b(?)
#> - l
#> - l_f(?)
#> - l_b(?)
#> - full_res
#> - out_t
#> - out_s
#> - out_i
#> - reg_t
#> - reg_s
#> - reg_i
#> - reg_sa
#> - reg_u
#> - reg_y
#> - det_t
#> - det_s
#> - det_i
#> - out_t_f(?)
#> - out_s_f(?)
#> - out_i_f(?)
#> - reg_t_f(?)
#> - reg_s_f(?)
#> - reg_i_f(?)
#> - reg_sa_f(?)
#> - reg_u_f(?)
#> - reg_y_f(?)
#> - det_t_f(?)
#> - det_s_f(?)
#> - det_i_f(?)
#> - out_t_b(?)
#> - out_s_b(?)
#> - out_i_b(?)
#> - reg_t_b(?)
#> - reg_s_b(?)
#> - reg_i_b(?)
#> - reg_sa_b(?)
#> - reg_u_b(?)
#> - reg_y_b(?)
#> - det_t_b(?)
#> - det_s_b(?)
#> - det_i_b(?)
#> - mode
#> - seasonal
#> - sa
#> - t
#> - s
#> - i
#> - sa_f
#> - t_f
#> - s_f
#> - i_f
#> - decomposition.y_lin
#> - decomposition.sa_lin
#> - decomposition.t_lin
#> - decomposition.s_lin
#> - decomposition.i_lin
#> - decomposition.sa_lin_e
#> - decomposition.t_lin_e
#> - decomposition.s_lin_e
#> - decomposition.i_lin_e
#> - decomposition.y_lin_f
#> - decomposition.sa_lin_f
#> - decomposition.t_lin_f
#> - decomposition.s_lin_f
#> - decomposition.i_lin_f
#> - decomposition.y_lin_ef
#> - decomposition.sa_lin_ef
#> - decomposition.t_lin_ef
#> - decomposition.s_lin_ef
#> - decomposition.i_lin_ef
#> - decomposition.y_lin_b
#> - decomposition.sa_lin_b
#> - decomposition.t_lin_b
#> - decomposition.s_lin_b
#> - decomposition.i_lin_b
#> - decomposition.y_lin_eb
#> - decomposition.sa_lin_eb
#> - decomposition.t_lin_eb
#> - decomposition.s_lin_eb
#> - decomposition.i_lin_eb
#> - decomposition.y_cmp
#> - decomposition.y_cmp_f
#> - decomposition.y_cmp_b
#> - decomposition.sa_cmp
#> - decomposition.t_cmp
#> - decomposition.s_cmp
#> - decomposition.i_cmp
#> - diagnostics.seas-lin-combined
#> - diagnostics.seas-lin-evolutive
#> - diagnostics.seas-lin-stable
#> - diagnostics.seas-si-combined
#> - diagnostics.seas-si-combined3
#> - diagnostics.seas-si-evolutive
#> - diagnostics.seas-si-stable
#> - diagnostics.seas-res-combined
#> - diagnostics.seas-res-combined3
#> - diagnostics.seas-res-evolutive
#> - diagnostics.seas-res-stable
#> - diagnostics.seas-sa-combined
#> - diagnostics.seas-sa-combined3
#> - diagnostics.seas-sa-evolutive
#> - diagnostics.seas-sa-stable
#> - diagnostics.seas-i-combined
#> - diagnostics.seas-i-combined3
#> - diagnostics.seas-i-evolutive
#> - diagnostics.seas-i-stable
#> - diagnostics.seas-lin-qs
#> - diagnostics.seas-lin-f
#> - diagnostics.seas-lin-friedman
#> - diagnostics.seas-lin-kw
#> - diagnostics.seas-lin-periodogram
#> - diagnostics.seas-lin-spectralpeaks
#> - diagnostics.seas-res-qs
#> - diagnostics.seas-res-f
#> - diagnostics.seas-res-friedman
#> - diagnostics.seas-res-kw
#> - diagnostics.seas-res-periodogram
#> - diagnostics.seas-res-spectralpeaks
#> - diagnostics.seas-sa-qs
#> - diagnostics.seas-sa-f
#> - diagnostics.seas-sa-friedman
#> - diagnostics.seas-sa-kw
#> - diagnostics.seas-sa-periodogram
#> - diagnostics.seas-sa-spectralpeaks
#> - diagnostics.seas-i-qs
#> - diagnostics.seas-i-f
#> - diagnostics.seas-i-friedman
#> - diagnostics.seas-i-kw
#> - diagnostics.seas-i-periodogram
#> - diagnostics.seas-i-spectralpeaks
#> - diagnostics.seas-sa-ac1
#> - diagnostics.td-res-all
#> - diagnostics.td-res-last
#> - diagnostics.td-sa-all
#> - diagnostics.td-sa-last
#> - diagnostics.td-i-all
#> - diagnostics.td-i-last
#> - diagnostics.fcast-insample-mean
#> - diagnostics.fcast-outsample-mean
#> - diagnostics.fcast-outsample-variance
#> - variancedecomposition.cycle
#> - variancedecomposition.seasonality
#> - variancedecomposition.irregular
#> - variancedecomposition.tdh
#> - variancedecomposition.others
#> - variancedecomposition.total
#> - quality.summary
#> - benchmarking.original
#> - benchmarking.target
#> - benchmarking.result 
#> 
#>  For a detailled summary of all outputs, please use the function `x13_full_dictionary()` or `tramoseats_full_dictionary()`
# set up vector with names of output objects of interest
user_defined_output <- c("ylin", "residuals.kurtosis")
# generate the corresponding output in an estimation
library(rjd3toolkit)
y <- rjd3toolkit::ABS$X0.2.09.10.M
m<-tramoseats(y,"rsafull", userdefined=user_defined_output)
# retrieve user defined output
tail(m$user_defined$ylin)
#>           Mar      Apr      May      Jun      Jul      Aug
#> 2017 1389.905 1492.068 1460.957 1540.453 1468.642 1279.390
m$user_defined$residuals.kurtosis
#> Value: 3.495232 
#> P-Value: 0.0402 
```
