#' @include utils.R tramoseats_spec.R tramoseats_rslts.R
NULL


#' TRAMO model, pre-adjustment in TRAMO-SEATS
#'
#' @param ts a univariate time series.
#' @param spec the model specification. Can be either the name of a predefined specification or a user-defined specification.
#' @param context the dictionnary of variables.
#' @param userdefined a vector containing the additional output variables.
#'
#' @return the `tramo()` function returns a list with the results (`"JD3_regarima_rslts"` object), the estimation specification and the result specification, while `fast_tramo()` is a faster function that only returns the results.
#'
#' @examples
#' library(rjd3toolkit)
#' y = rjd3toolkit::ABS$X0.2.09.10.M
#' sp = spec_tramo("trfull")
#' sp = add_outlier(sp,
#'                  type = c("AO"), c("2015-01-01", "2010-01-01"))
#' fast_tramo(y, spec = sp)
#' sp = set_transform(
#'   set_tradingdays(
#'     set_easter(sp, enabled = FALSE),
#'     option = "workingdays"
#'   ),
#'   fun = "None"
#' )
#' fast_tramo(y, spec = sp)
#' sp = set_outlier(sp, outliers.type = c("AO"))
#' fast_tramo(y, spec = sp)
#' @export
tramo<-function(ts, spec=c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"), context=NULL, userdefined = NULL){
  # TODO : check parameters
  jts<-rjd3toolkit::.r2jd_ts(ts)
  if (is.character(spec)){
    spec = gsub("rsa", "tr", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")
    )
    jrslt<-.jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/tramoseats/base/core/tramo/TramoOutput;", "fullProcess", jts, spec)
  }else{
    jspec<-.r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/tramoseats/base/core/tramo/TramoOutput;", "fullProcess", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = .tramo_output(jrslt)
    return (.add_ud_var(res, jrslt, userdefined = userdefined))
  }
}

#' @export
#' @rdname tramo
fast_tramo<-function(ts, spec=c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"), context=NULL, userdefined = NULL){
  # TODO : check parameters
  jts<-rjd3toolkit::.r2jd_ts(ts)
  if (is.character(spec)){
    spec = gsub("rsa", "tr", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")
    )
    jrslt<-.jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/toolkit/base/core/regsarima/regular/RegSarimaModel;", "process", jts, spec)
  }else{
    jspec<-.r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/toolkit/base/core/regsarima/regular/RegSarimaModel;", "process", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = .regarima_rslts(jrslt)
    return (.add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
  }
}


.tramo_output<-function(jq){
  if (is.jnull(jq))
    return (NULL)
  q<-.jcall("jdplus/tramoseats/base/r/Tramo", "[B", "toBuffer", jq)
  p<-RProtoBuf::read(tramoseats.TramoOutput, q)
  return (structure(list(
    result=rjd3toolkit::.p2r_regarima_rslts(p$result),
    estimation_spec=.p2r_spec_tramo(p$estimation_spec),
    result_spec=.p2r_spec_tramo(p$result_spec)
  ),
  class=c("JD3_TRAMO_OUTPUT", "JD3"))
  )
}

#' Seasonal Adjustment with  TRAMO-SEATS
#'
#' @inheritParams tramo
#'
#'
#' @examples
#' library(rjd3toolkit)
#' sp = spec_tramoseats("rsafull")
#' y = rjd3toolkit::ABS$X0.2.09.10.M
#' fast_tramoseats(y, spec = sp)
#' sp = add_outlier(sp,
#'                  type = c("AO"), c("2015-01-01", "2010-01-01"))
#' sp = set_transform(
#'   set_tradingdays(
#'     set_easter(sp, enabled = FALSE),
#'     option = "workingdays"
#'   ),
#'   fun = "None"
#' )
#' fast_tramoseats(y, spec = sp)
#' @return the `tramoseats()` function returns a list with the results, the estimation specification and the result specification, while `fast_tramoseats()` is a faster function that only returns the results.
#' The `jtramoseats()` functions only results the java object to custom outputs in other packages (use [rjd3toolkit::dictionary()] to
#' get the list of variables and [rjd3toolkit::result()] to get a specific variable).
#' @export
tramoseats<-function(ts, spec=c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"), context=NULL, userdefined = NULL){
  # TODO : check parameters
  jts<-rjd3toolkit::.r2jd_ts(ts)
  if (is.character(spec)){
    spec = gsub("tr", "rsa", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
    )
    jrslt<-.jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsOutput;", "fullProcess", jts, spec)
  }else{
    jspec<-.r2jd_spec_tramoseats(spec)
    if (is.null(context)){
      jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsOutput;", "fullProcess", jts, jspec, jcontext )
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = .tramoseats_output(jrslt)
    return (.add_ud_var(res, jrslt, userdefined = userdefined))
  }
}

#' @export
#' @rdname tramoseats
fast_tramoseats<-function(ts, spec=c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"), context=NULL, userdefined = NULL){
  jts<-rjd3toolkit::.r2jd_ts(ts)
  if (is.character(spec)){
    spec = gsub("tr", "rsa", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
    )
    jrslt<-.jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsResults;", "process", jts, spec)
  }else{
    jspec<-.r2jd_spec_tramoseats(spec)
    if (is.null(context)){
      jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsResults;", "process", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = .tramoseats_rslts(jrslt)
    return (.add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
  }
}

#' @export
#' @rdname tramoseats
jtramoseats<-function(ts, spec=c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"), context=NULL, userdefined = NULL){
  jts<-rjd3toolkit::.r2jd_ts(ts)
  if (is.character(spec)){
    spec = gsub("tr", "rsa", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
    )
    jrslt<-.jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsResults;", "process", jts, spec)
  }else{
    jspec<-.r2jd_spec_tramoseats(spec)
    if (is.null(context)){
      jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsResults;", "process", jts, jspec, jcontext)
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    res = rjd3toolkit::.jd3_object(jrslt, result = TRUE)
    return (res)
  }
}

.tramoseats_output<-function(jq){
  if (is.jnull(jq))
    return (NULL)
  q<-.jcall("jdplus/tramoseats/base/r/TramoSeats", "[B", "toBuffer", jq)
  p<-RProtoBuf::read(tramoseats.TramoSeatsOutput, q)
  return (structure(list(
    result=.p2r_tramoseats_rslts(p$result),
    estimation_spec=.p2r_spec_tramoseats(p$estimation_spec),
    result_spec=.p2r_spec_tramoseats(p$result_spec)
  ),
  class="JD3_TRAMOSEATS_OUTPUT")
  )

}

#' Refresh a specification with constraints
#'
#' @description
#' Function allowing to create a new specification by updating a specification used for a previous estimation.
#' Some selected parameters will be kept fixed (previous estimation results) while others will be freed for re-estimation
#' in a domain of constraints. See details and examples.
#'
#' @details
#' The selection of constraints to be kept fixed or re-estimated is called a revision policy.
#' User-defined parameters are always copied to the new refreshed specifications. Only the Tramo
#' (reg-arima pre-adjustment part) is refreshed, but the final ARIMA model handed down to SEATS for the decomposition part
#' might be changed as a result of the refreshing process.
#'
#' Available refresh policies are:
#'
#' \strong{Current}: applying the current pre-adjustment reg-arima model and adding the new raw data points as Additive Outliers (defined as new intervention variables)
#'
#' \strong{Fixed}: applying the current pre-adjustment reg-arima model and replacing forecasts by new raw data points.
#'
#' \strong{FixedParameters}: pre-adjustment reg-arima model is partially modified: regression coefficients will be re-estimated but regression variables, Arima orders
#' and coefficients are unchanged.
#'
#' \strong{FixedAutoRegressiveParameters}: same as FixedParameters but Arima Moving Average coefficients (MA) are also re-estimated, Auto-regressive (AR) coefficients are kept fixed.
#' When using Seats for decomposition it avoids a possible re-allocation of roots between the trend and seasonal components,
#' which might have led to strong revisions.
#'
#' \strong{FreeParameters}: all regression and Arima model coefficients are re-estimated, regression variables and Arima orders are kept fixed.
#'
#' \strong{Outliers}: regression variables and Arima orders are kept fixed, but outliers will be re-detected on the defined span, thus all regression and Arima model coefficients are re-estimated
#'
#' \strong{Outliers_StochasticComponent}: same as "Outliers" but Arima model orders (p,d,q)(P,D,Q) can also be re-identified.
#'
#' @param spec the current specification to be refreshed ("result_spec")
#' @param refspec the reference specification used to define the domain considered for re-estimation ("domain_spec")
#' By default this is the `"TRFull"` or `"RSAFull"` specification.
#' @param policy the refresh policy to apply (see details)
#' @param period,start,end to specify the span on which outliers will be re-identified when `policy` equals to `"Outliers"`
#' or `"Outliers_StochasticComponent"`. Span definition: \code{period}: numeric, number of observations in a year (12,4...). \code{start}: vector
#' indicating the start of the series in the format c(YYYY,MM). \code{end}: vector in the format c(YYYY,MM) indicating the date from which outliers
#' will be re-identified. If span parameters are not specified outliers will be re-detected on the whole series.
#'
#' @return a new specification, an object of class `"JD3_X13_SPEC"` (`spec_x13()`),
#' `"JD3_REGARIMA_SPEC"` (`spec_regarima()`)
#'
#' @references
#' More information on revision policies in JDemetra+ online documentation:
#' \url{https://jdemetra-new-documentation.netlify.app/t-rev-policies-production}
#'
#' @examples
#'y<- rjd3toolkit::ABS$X0.2.08.10.M
#'# raw series for first estimation
#'y_raw <-window(y,end = 2009)
#'# raw series for second (refreshed) estimation
#'y_new <-window(y,end = 2010)
#' # specification for first estimation
#'spec_ts_1<-spec_tramoseats("RSAFull")
#' # first estimation
#'sa_ts<- tramoseats(y_raw, spec_ts_1)
#' # refreshing the specification
#' current_result_spec <- sa_ts$result_spec
#' current_domain_spec <- sa_ts$estimation_spec
#' spec_ts_ref <- tramoseats_refresh(current_result_spec, # point spec to be refreshed
#'   current_domain_spec, #domain spec (set of constraints)
#'   policy = "FixedAutoRegressiveParameters")
#' # 2nd estimation with refreshed specification
#' sa_ts_ref <- tramoseats(y_new, spec_ts_ref)
#' @name refresh
#' @rdname refresh
#' @export
tramo_refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  if (!inherits(spec, "JD3_TRAMO_SPEC"))
    stop("Invalid specification type")
  jspec<-.r2jd_spec_tramo(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("jdplus/tramoseats/base/api/tramo/TramoSpec", "Ljdplus/tramoseats/base/api/tramo/TramoSpec;", "fromString", "trfull")

  }else{
    if (!inherits(refspec, "JD3_TRAMO_SPEC"))
      stop("Invalid specification type")
    jrefspec<-.r2jd_spec_tramo(refspec)
  }
  jdom<-rjd3toolkit::.jdomain(period, start, end)
  jnspec<-.jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/tramoseats/base/api/tramo/TramoSpec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (.jd2r_spec_tramo(jnspec))
}

#' @rdname refresh
#' @export
tramoseats_refresh<-function(spec, refspec=NULL, policy=c("FreeParameters", "Complete", "Outliers_StochasticComponent", "Outliers", "FixedParameters", "FixedAutoRegressiveParameters", "Fixed", "Current"), period=0, start=NULL, end=NULL){
  policy=match.arg(policy)
  if (!inherits(spec, "JD3_TRAMOSEATS_SPEC"))
    stop("Invalid specification type")
  jspec<-.r2jd_spec_tramoseats(spec)
  if (is.null(refspec)){
    jrefspec<-.jcall("jdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec", "Ljdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec;", "fromString", "rsafull")

  }else{
    if (!inherits(refspec, "JD3_TRAMOSEATS_SPEC"))
      stop("Invalid specification type")
    jrefspec<-.r2jd_spec_tramoseats(refspec)
  }
  jdom<-rjd3toolkit::.jdomain(period, start, end)
  jnspec<-.jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec;", "refreshSpec", jspec, jrefspec, jdom, policy)
  return (.jd2r_spec_tramoseats(jnspec))

}



terror_names<-c("actual", "forecast", "error", "rel. error", "raw", "fraw", "efraw")
forecast_names<-c("forecast", "error", "fraw", "efraw")

#' TERROR Quality Control of Outliers
#'
#' TRAMO for ERRORs (TERROR) controls the quality of the data by checking outliers at the end of the series
#'
#' @inheritParams tramo
#' @param nback number of last observations considered for the quality check.
#'
#' @return a `mts` object with 7 variables:
#' - `actual` the actual data at the end of the series.
#'
#' - `forecast` the forecast of the actual data at the end of the series.
#'
#' - `error` the absolute errors (= observed - forecasts).
#'
#' - `rel.error` relative errors ("scores") : ratios between the forecast
#'errors and the standard deviation of the forecasts of the last observations
#'(positive values mean under-estimation).
#'
#' - `raw` the transformed series. More especially, if the chosen model implies
#' a log-transformation, the values are obtained after a log-transformation.
#' Other transformations, such leap year corrections or length-of periods corrections may also be used.
#' - `fraw` the forecast of the transformed series.
#' - `efraw` the absolute errors of the transformed series.
#'
#' @examples
#' terror(rjd3toolkit::ABS$X0.2.09.10.M, nback = 2)
#' @export
terror<-function(ts, spec=c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"), nback=1, context=NULL){
  jts<-rjd3toolkit::.r2jd_ts(ts)
  if (is.character(spec)){
    spec = gsub("rsa", "tr", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")
    )
    jrslt<-.jcall("jdplus/tramoseats/base/r/Terror", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "process", jts, spec, as.integer(nback))
  }else{
    jspec<-.r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("jdplus/tramoseats/base/r/Terror", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "process", jts, jspec, jcontext, as.integer(nback))
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    rslt <- rjd3toolkit::.jd2r_matrix(jrslt)
    rslt <- ts(rslt, end = end(ts), frequency = frequency(ts))
    colnames(rslt)<-terror_names
    return (rslt)
  }
}

#' Forecasts with TRAMO
#'
#' @inheritParams tramo
#' @param nf the forecasting horizon (`numeric`). The forecast length is in periods (positive values) or years (negative values). By default, the program generates a one-year forecast (`nf = -1`).
#'
#' @return a `mts` object with 7 variables:
#' - `forecast` the forecast of the actual data at the end of the series.
#'
#' - `error` standard deviation of the forecast.
#'
#' - `fraw` the forecast of the transformed series.
#' - `efraw` the standard deviation of the forecast of the transformed series.
#' @examples
#' tramo_forecast(rjd3toolkit::ABS$X0.2.09.10.M)
#' @export
tramo_forecast<-function(ts, spec= c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"), nf=-1, context=NULL){
  # TODO : check parameters
  jts<-rjd3toolkit::.r2jd_ts(ts)
  if (nf<0) nf<-frequency(ts)*(-nf)

  if (is.character(spec)){
    spec = gsub("rsa", "tr", tolower(spec), fixed = TRUE)
    spec = match.arg(spec[1],
                     choices = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")
    )
    jrslt<-.jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "forecast", jts, spec, as.integer(nf))
  }else{
    jspec<-.r2jd_spec_tramo(spec)
    if (is.null(context)){
      jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
    } else {
      jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
    }
    jrslt<-.jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "forecast", jts, jspec, jcontext, as.integer(nf))
  }
  if (is.jnull(jrslt)){
    return (NULL)
  }else{
    rslt<-rjd3toolkit::.jd2r_matrix(jrslt)
    rslt <- ts(rslt, frequency = frequency(ts),
               start = time(ts)[length(ts)] + 1/frequency(ts))
    colnames(rslt)<-forecast_names
    return (rslt)
  }
}
