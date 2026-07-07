#' @include utils.R tramoseats_spec.R tramoseats_rslts.R
NULL


#' @title TRAMO model, pre-adjustment in TRAMO-SEATS
#'
#' @description allows to model the series with a Reg-Arima model, estimate outlier,
#' calendar or other regression effects and produce forecasts
#'
#'
#' @param ts a univariate time series.
#' @param spec the model specification. Can be either the name of a predefined
#' specification or a user-defined specification.
#' @param context the dictionary of variables.
#' @param userdefined a vector containing the additional output variables
#' (see [tramoseats_dictionary()]).
#'
#' @returns the `tramo()` function returns a list with the results
#' (`"JD3_tramo_rslts"` object), the estimation specification and the result
#' specification, while `tramo_fast()` is a faster function that only returns
#' the results.
#'
#' @examplesIf rjd3jars::check_java_version()
#' library("rjd3toolkit")
#'
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' sp <- tramo_spec("trfull")
#' sp <- add_outlier(sp,
#'     type = c("AO"), c("2015-01-01", "2010-01-01")
#' )
#' \donttest{
#' tramo_fast(y, spec = sp)
#' }
#' sp <- set_transform(
#'     set_tradingdays(
#'         set_easter(sp, enabled = FALSE),
#'         option = "workingdays"
#'     ),
#'     fun = "None"
#' )
#' \donttest{
#' tramo_fast(y, spec = sp)
#' }
#' sp <- set_outlier(sp, outliers.type = c("AO"))
#'
#' \donttest{
#' tramo_fast(y, spec = sp)
#' }
#'
#' @export
#'
tramo <- function(ts,
                  spec = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"),
                  context = NULL,
                  userdefined = NULL) {
    # TODO : check parameters
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("rsa", "tr", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")
        )
        jrslt <- .jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/tramoseats/base/core/tramo/TramoOutput;", "fullProcess", jts, spec)
    } else {
        jspec <- .r2jd_spec_tramo(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/tramoseats/base/core/tramo/TramoOutput;", "fullProcess", jts, jspec, jcontext)
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        res <- .tramo_output(jrslt)
        return(rjd3toolkit::.add_ud_var(res, jrslt, userdefined = userdefined))
    }
}

#' @export
#' @rdname tramo
tramo_fast <- function(ts, spec = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"), context = NULL, userdefined = NULL) {
    # TODO : check parameters
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("rsa", "tr", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")
        )
        jrslt <- .jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/toolkit/base/core/regsarima/regular/RegSarimaModel;", "process", jts, spec)
    } else {
        jspec <- .r2jd_spec_tramo(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/toolkit/base/core/regsarima/regular/RegSarimaModel;", "process", jts, jspec, jcontext)
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        res <- .regarima_rslts(jrslt)
        return(rjd3toolkit::.add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
    }
}


.tramo_output <- function(jq) {
    if (is.jnull(jq)) {
        return(NULL)
    }
    q_obj <- .jcall("jdplus/tramoseats/base/r/Tramo", "[B", "toBuffer", jq)
    p <- RProtoBuf::read(tramoseats.TramoOutput, q_obj)
    return(structure(
        list(
            result = rjd3toolkit::.p2r_regarima_rslts(p$result),
            estimation_spec = .p2r_spec_tramo(p$estimation_spec),
            result_spec = .p2r_spec_tramo(p$result_spec)
        ),
        class = c("JD3_TRAMO_OUTPUT", "JD3")
    ))
}

#' @title Seasonal Adjustment with  TRAMO-SEATS
#'
#' @inheritParams tramo
#'
#' @returns The `tramoseats()` function returns a list with the results, the
#' estimation specification and the result specification, while
#' `tramoseats_fast()` is a faster function that only returns the results.
#' The `.jtramoseats()` functions only results the java object to custom outputs
#' in other packages (use [rjd3toolkit::dictionary()] to get the list of
#' variables and [rjd3toolkit::result()] to get a specific variable).
#'
#' @examplesIf rjd3jars::check_java_version()
#' library("rjd3toolkit")
#'
#' sp <- tramoseats_spec("rsafull")
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' \donttest{
#' tramoseats(y, spec = sp)
#' tramoseats_fast(y, spec = sp)
#' }
#' sp <- add_outlier(sp,
#'     type = c("AO"), c("2015-01-01", "2010-01-01")
#' )
#' sp <- set_transform(
#'     set_tradingdays(
#'         set_easter(sp, enabled = FALSE),
#'         option = "workingdays"
#'     ),
#'     fun = "None"
#' )
#' \donttest{
#' tramoseats(y, spec = sp)
#' tramoseats_fast(y, spec = sp)
#' }
#'
#' @export
tramoseats <- function(ts,
                       spec = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"),
                       context = NULL,
                       userdefined = NULL) {
    # TODO : check parameters
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("tr", "rsa", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
        )
        jrslt <- .jcall("jdplus/tramoseats/base/r/TramoSeats",
                        "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsOutput;",
                        "fullProcess", jts, spec)
    } else {
        jspec <- .r2jd_spec_tramoseats(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/tramoseats/base/r/TramoSeats",
                        "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsOutput;",
                        "fullProcess", jts, jspec, jcontext)
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        res <- .tramoseats_output(jrslt)
        return(rjd3toolkit::.add_ud_var(res, jrslt, userdefined = userdefined))
    }
}

#' @export
#' @rdname tramoseats
tramoseats_fast <- function(ts, spec = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"), context = NULL, userdefined = NULL) {
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("tr", "rsa", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
        )
        jrslt <- .jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsResults;", "process", jts, spec)
    } else {
        jspec <- .r2jd_spec_tramoseats(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsResults;", "process", jts, jspec, jcontext)
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        res <- .tramoseats_rslts(jrslt)
        return(rjd3toolkit::.add_ud_var(res, jrslt, userdefined = userdefined, result = TRUE))
    }
}

#' @export
#' @rdname tramoseats
.jtramoseats <- function(ts, spec = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"), context = NULL, userdefined = NULL) {
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("tr", "rsa", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
        )
        jrslt <- .jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsResults;", "process", jts, spec)
    } else {
        jspec <- .r2jd_spec_tramoseats(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/core/tramoseats/TramoSeatsResults;", "process", jts, jspec, jcontext)
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        res <- rjd3toolkit::.jd3_object(jrslt, result = TRUE)
        return(res)
    }
}

.tramoseats_output <- function(jq) {
    if (is.jnull(jq)) {
        return(NULL)
    }
    q_obj <- .jcall("jdplus/tramoseats/base/r/TramoSeats", "[B", "toBuffer", jq)
    p <- RProtoBuf::read(tramoseats.TramoSeatsOutput, q_obj)
    return(structure(
        list(
            result = .p2r_tramoseats_rslts(p$result),
            estimation_spec = .p2r_spec_tramoseats(p$estimation_spec),
            result_spec = .p2r_spec_tramoseats(p$result_spec)
        ),
        class = "JD3_TRAMOSEATS_OUTPUT"
    ))
}
#' @title Refresh a specification with constraints
#'
#' @description
#' Functions `tramoseats_refresh` and `tramo_refresh` allow to create a new
#' specification by updating an existing one.
#' Some selected parameters will be kept fixed while others will be freed within the boundaries of a
#' reference specification. In practice each freed parameter of the specification to be updated
#' (`spec`) is replaced by the corresponding parameter of the reference specification (`refspec`).
#' See details and examples.
#'
#' @details
#' A particular selection of parameters to be kept fixed or re-estimated is called a
#' revision policy.
#'
#' Available refresh policies are:
#' \enumerate{
#' \item \strong{Current}: applying the current pre-adjustment reg-arima model
#' and handling the new raw data points, or any sub-span of the series as
#' Additive Outliers (defined as new intervention variables);
#' SEATS and Benchmarking part parameters are untouched.
#' \item \strong{Fixed}: applying the current pre-adjustment reg-arima model
#' and replacing forecasts by new raw data points;
#' SEATS and Benchmarking part parameters are untouched.
#' \item \strong{FixedParameters}: pre-adjustment reg-arima model is partially
#' modified: regression coefficients will be re-estimated but regression
#' variables, Arima orders and coefficients are unchanged;
#' \item \strong{FixedAutoRegressiveParameters}: same as FixedParameters but
#' Arima Moving Average coefficients (MA) are also re-estimated, Auto-regressive
#'  (AR) coefficients are kept fixed;
#' SEATS and Benchmarking part parameters are untouched.
#' \item \strong{FreeParameters}: all regression and Arima model coefficients
#' are re-estimated, regression variables and Arima orders are kept fixed;
#' SEATS and Benchmarking part parameters are untouched.
#' \item \strong{Outliers}: regression variables and Arima orders are kept
#' fixed, but outliers will be re-detected on the defined span, thus all
#' regression and Arima model coefficients are re-estimated;
#' SEATS and Benchmarking part parameters are untouched.
#' \item \strong{Outliers_StochasticComponent}: same as "Outliers" but Arima
#' model orders (p,d,q)(P,D,Q) can also be re-identified;
#' SEATS and Benchmarking part parameters are untouched.
#' \item \strong{Complete}: All the parameters are re-identified and
#' re-estimated, unless constrained in the reference spec.
#' SEATS and Benchmarking part parameters are entirely reset to values in the reference spec.
#' }
#'
#' @param spec specification to be refreshed
#' Object of class "JD3_tramoseats_SPEC" or "JD3_tramo_SPEC",
#' can be obtained as an output of `tramoseats_spec` or `tramo_spec` and customised with `set_` functions,
#' see `tramoseats_spec` documentation

#' @param refspec reference specification
#' By default `"rsa4"` or `"tr5"` specification.
#' Object of class "JD3_tramoseats_SPEC" or "JD3_tramo_SPEC",
#' can be obtained as an output of `tramoseats_spec` or `tramo_spec` and customised with `set_` functions,
#' see `tramoseats_spec` documentation
#'
#' @param policy refresh policy to apply (see details)
#'
#' @param period,start,end  additional parameters used to specify the span
#' when `policy = "Current"` or `policy = "Outliers"` or `policy = "Outliers_StochasticComponent"`
#' \code{period}: numeric, number of observations in a year (12, 4...), compulsory, if false or missing,
#' re-estimation with refreshed specification won't work.
#' When `policy = "Outliers"` or `policy = "Outliers_StochasticComponent"`
#' \code{start} has to be specified as the date from which outliers will be re-identified
#' \code{end} is not used, if specified it will be ignored.
#' When `policy = "Current"`
#' \code{start} and \code{end} have to be both specified and indicate the span on which
#' additive outliers (AO) will be added.

#' Span definition:
#' \code{start} and \code{end}: defined as arrays of two elements: year and
#' first period (for example, `period = 12` and `start=c(1980, 1)` stands for January
#' 1980)
#'
#'
#' @returns a new specification, an object of class `"JD3_tramoseats_SPEC"` or
#' `"JD3_tramo_SPEC"`.
#'
#' @references
#' More information on revision policies in JDemetra+ documentation:
#' \url{https://doc.jdemetra.org/a-rev-policies}
#'
#' @examplesIf rjd3jars::check_java_version()
#' library("rjd3toolkit")
#' y <- rjd3toolkit::ABS$X0.2.08.10.M

#' # raw series for first estimation
#' y_raw <- window(y, end = c(2016, 12))
#' # raw series for second (refreshed) estimation: new data points
#' y_new <- window(y, end = c(2017, 6))
#' \donttest{
#' # Example 1 : refresh mechanism
#' # Create reference spec, here the default "rsa3"
#' rsa3<- tramoseats_spec("rsa3")
#' # Customize this spec
#' ## Tramo part
#' ### For example, disable automatic arima modelling
#' user_spec <- set_automodel(rsa3, enabled = FALSE)
#' ### set a user-defined arima model
#' user_spec <- set_arima(
#'    user_spec,
#'    mean = 0.2,
#'    mean.type = "Fixed",
#'    p = 1,
#'    d = 2,
#'    q = 0,
#'    bp = 1,
#'    bd = 1,
#'    bq = 0,
#'    coef = c(0.6, 0.7),
#'    coef.type = c("Initial", "Fixed")
#')
#' #print(user_spec)
#'
#'## Customize the x11 part
#'user_spec<-set_seats(user_spec,
#'                     fcasts = -2,
#'                     bcasts = -1,
#'                     trend.boundary=0.6,
#'                     seas.boundary=0.75,
#'                     algorithm= "KalmanSmoother")

#' #print(user_spec)
## Customize the benchmarking part
#' user_spec<- set_benchmarking(
#'    user_spec,
#'    enabled = TRUE,
#'    target = "Original",
#'    rho = 0.7,
#'    lambda = 0.5,
#'    forecast = TRUE,
#'    bias = "Multiplicative")

#' #print(user_spec)

#' # Use policy: "Outliers_StochasticComponent"

#' tramoseats_spec_ref <- tramoseats_refresh(spec= user_spec,
#'                            refspec= rsa3,
#'                            policy = "Outliers_StochasticComponent"
#')

#' # print(tramoseats_spec_ref)
#' # user defined reg-arima model is reset and outliers will be re-identified
#' # on the whole series as no start and end specified, SEATS and Benchmarking parameters
#' # are left unchanged

#' # Use policy: "Complete"

#' tramoseats_spec_ref <- tramoseats_refresh(spec= user_spec,
#'                            refspec= rsa3,
#'                            policy = "Complete"
#')

#' # print(tramoseats_spec_ref)
#' # all user defined parameters are reset and replaced with "rsa3" parameters,
# including for SEATS and Benchmarking parameters

#' # Example 2 : practical re-estimation use-case

# '# first estimation
#' sa_tramoseats <- tramoseats(y_raw, user_spec)

#' # refreshing the specification resulting from the first estimation
#' # to partially adapt to new data

#' spec_to_refresh <- sa_tramoseats$result_spec
#' reference_spec <- sa_tramoseats$estimation_spec

#' # policy = "Fixed"
#' spec_tramoseats_ref <- tramoseats_refresh(spec_to_refresh,
#'                             reference_spec,
#'                             policy = "Fixed"
#' )
#' # 2nd estimation with refreshed specification
#' sa_tramoseats_ref <- tramoseats(y_new, spec_tramoseats_ref)

#' # policy = "Outliers"
#' spec_tramoseats_ref <- tramoseats_refresh(spec_to_refresh,
#'                            reference_spec,
#'                            policy = "Outliers",
#'                            period = 12,
#'                            start = c(2017, 1)
#')
#'# outliers will be re-detected from January 2017 included
#'# 2nd estimation with refreshed specification
#'sa_tramoseats_ref <- tramoseats(y_new, spec_tramoseats_ref)
#'
#'# policy = "Current"
#'
#'spec_tramoseats_ref <- tramoseats_refresh(spec_to_refresh,
#'                            reference_spec,
#'                            policy = "Current",
#'                            period = 12,
#'                            start = c(2017, 1),
#'                            end = end(y_new)
#')
#' sa_tramoseats_ref <- tramoseats(y_new, spec_tramoseats_ref)
#'}
#' # Points from January 2017 (included) until the end of the series will be
#' # treated as Additive Outliers, the previous reg-Arima model being otherwise
#' # kept fixed 2nd estimation with refreshed specification


# Procedure is the same procedure using `tramo_refresh` instead of `tramoseats_refresh`
#' @name refresh
#' @rdname refresh
#' @export
tramo_refresh <- function(spec,
                          refspec = NULL,
                          policy = c("FreeParameters", "Complete",
                                     "Outliers_StochasticComponent", "Outliers",
                                     "FixedParameters",
                                     "FixedAutoRegressiveParameters",
                                     "Fixed", "Current"),
                          period = 0,
                          start = NULL,
                          end = NULL) {
    policy <- match.arg(policy)
    if (!inherits(spec, "JD3_TRAMO_SPEC")) {
        stop("Invalid specification type")
    }
    jspec <- .r2jd_spec_tramo(spec)
    if (is.null(refspec)) {
        jrefspec <- .jcall("jdplus/tramoseats/base/api/tramo/TramoSpec",
                           "Ljdplus/tramoseats/base/api/tramo/TramoSpec;",
                           "fromString", "trfull")
    } else {
        if (!inherits(refspec, "JD3_TRAMO_SPEC")) {
            stop("Invalid specification type")
        }
        jrefspec <- .r2jd_spec_tramo(refspec)
    }
    if (policy == "Current") {
        if (end[2] == period) end <- c(end[1] + 1, 1) else end <- c(end[1], end[2] + 1)
        jdom <- rjd3toolkit::.jdomain(period, start, end)
    } else if (policy == "Outliers") {
        jdom <- rjd3toolkit::.jdomain(period, NULL, start)
    } else {
        jdom <- jdom <- rjd3toolkit::.jdomain(0, NULL, NULL)
    }
    jnspec <- .jcall("jdplus/tramoseats/base/r/Tramo",
                     "Ljdplus/tramoseats/base/api/tramo/TramoSpec;",
                     "refreshSpec", jspec, jrefspec, jdom, policy)
    return(.jd2r_spec_tramo(jnspec))
}

#' @rdname refresh
#' @export
tramoseats_refresh <- function(spec,
                               refspec = NULL,
                               policy = c("FreeParameters", "Complete",
                                          "Outliers_StochasticComponent",
                                          "Outliers", "FixedParameters",
                                          "FixedAutoRegressiveParameters",
                                          "Fixed", "Current"),
                               period = 0,
                               start = NULL,
                               end = NULL) {
    policy <- match.arg(policy)
    if (!inherits(spec, "JD3_TRAMOSEATS_SPEC")) {
        stop("Invalid specification type")
    }
    jspec <- .r2jd_spec_tramoseats(spec)
    if (is.null(refspec)) {
        jrefspec <- .jcall("jdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec",
                           "Ljdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec;",
                           "fromString", "rsafull")
    } else {
        if (!inherits(refspec, "JD3_TRAMOSEATS_SPEC")) {
            stop("Invalid specification type")
        }
        jrefspec <- .r2jd_spec_tramoseats(refspec)
    }

    if (policy == "Current") {
        if (end[2] == period) end <- c(end[1] + 1, 1) else end <- c(end[1], end[2] + 1)
        jdom <- rjd3toolkit::.jdomain(period, start, end)
    } else if (policy == "Outliers") {
        jdom <- rjd3toolkit::.jdomain(period, NULL, start)
    } else {
        jdom <- jdom <- rjd3toolkit::.jdomain(0, NULL, NULL)
    }
    jnspec <- .jcall("jdplus/tramoseats/base/r/TramoSeats",
                     "Ljdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec;",
                     "refreshSpec", jspec, jrefspec, jdom, policy)
    return(.jd2r_spec_tramoseats(jnspec))
}



terror_names <- c("actual", "forecast", "error", "rel. error", "transformed", "tr.fcast", "tr.error")
forecast_names <- c("forecast", "error", "fraw", "efraw")

#' @title TERROR Quality Control of Outliers
#'
#' @description TRAMO for ERRORs (TERROR) controls the quality of the data by checking outliers at the end of the series
#'
#' @inheritParams tramo
#' @param nback number of last observations considered for the quality check.
#'
#' @returns a `mts` object with 7 variables:
#'
#' \enumerate{
#' \item \strong{actual}: the actual data at the end of the series;
#' \item \strong{forecast}: the forecast of the actual data at the end of the
#' series;
#' \item \strong{error}: the absolute errors (= observed - forecasts);
#' \item \strong{rel.error}: relative errors ("scores") : ratios between the
#' forecast errors and the standard deviation of the forecasts of the last
#' observations (positive values mean under-estimation);
#' \item \strong{raw}: the transformed series. More especially, if the chosen
#' model implies a log-transformation, the values are obtained after a
#' log-transformation. Other transformations, such leap year corrections or
#' length-of periods corrections may also be used;
#' \item \strong{fraw}: the forecast of the transformed series.;
#' \item \strong{efraw}: the absolute errors of the transformed series.
#' }
#'
#' @examplesIf rjd3jars::check_java_version()
#' \donttest{
#' terror(rjd3toolkit::ABS$X0.2.09.10.M, nback = 2)
#' }
#' @export
terror <- function(ts, spec = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"), nback = 1, context = NULL) {
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (is.character(spec)) {
        spec <- gsub("rsa", "tr", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")
        )
        jrslt <- .jcall("jdplus/tramoseats/base/r/Terror",
                        "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
                        "process", jts, spec, as.integer(nback))
    } else {
        jspec <- .r2jd_spec_tramo(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/tramoseats/base/r/Terror",
                        "Ljdplus/toolkit/base/api/math/matrices/Matrix;",
                        "process", jts, jspec, jcontext, as.integer(nback))
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        rslt <- rjd3toolkit::.jd2r_matrix(jrslt)
        rslt <- ts(rslt, end = end(ts), frequency = frequency(ts))
        colnames(rslt) <- terror_names
        return(rslt)
    }
}

#' @title Forecasts with TRAMO
#'
#' @inheritParams tramo
#' @param nf the forecasting horizon (`numeric`). The forecast length is in
#' periods (positive values) or years (negative values). By default, the program
#' generates a one-year forecast (`nf = -1`).
#'
#' @returns a `mts` object with 7 variables:
#' - `forecast` the forecast of the actual data at the end of the series.
#'
#' - `error` standard deviation of the forecast.
#'
#' - `fraw` the forecast of the transformed series.
#' - `efraw` the standard deviation of the forecast of the transformed series.
#'
#' @examplesIf rjd3jars::check_java_version()
#' \donttest{
#' tramo_forecast(rjd3toolkit::ABS$X0.2.09.10.M)
#' }
#'
#' @export
tramo_forecast <- function(ts, spec = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"), nf = -1, context = NULL) {
    # TODO : check parameters
    jts <- rjd3toolkit::.r2jd_tsdata(ts)
    if (nf < 0) nf <- frequency(ts) * (-nf)

    if (is.character(spec)) {
        spec <- gsub("rsa", "tr", tolower(spec), fixed = TRUE)
        spec <- match.arg(spec[1],
            choices = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")
        )
        jrslt <- .jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "forecast", jts, spec, as.integer(nf))
    } else {
        jspec <- .r2jd_spec_tramo(spec)
        if (is.null(context)) {
            jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
        } else {
            jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
        }
        jrslt <- .jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "forecast", jts, jspec, jcontext, as.integer(nf))
    }
    if (is.jnull(jrslt)) {
        return(NULL)
    } else {
        rslt <- rjd3toolkit::.jd2r_matrix(jrslt)
        rslt <- ts(rslt,
            frequency = frequency(ts),
            start = time(ts)[length(ts)] + 1 / frequency(ts)
        )
        colnames(rslt) <- forecast_names
        return(rslt)
    }
}


#' @title TRAMO-SEATS dictionary
#'
#' @description
#' Functions to provide information for all output objects (series, diagnostics,
#' parameters) available with `tramoseats()` function.
#'
#' @returns \code{tramoseats_dictionary()} returns a character vector containing the
#' names of all output objects (series, diagnostics, parameters) available with
#' the `tramoseats()` function, whereas \code{tramoseats_full_dictionary()} returns a
#' \code{data.frame} with format and description, for all the output objects.
#'
#' @name tramoseats_dictionary
#'
#' @details
#' These functions provide lists of output names (series, diagnostics,
#' parameters) available with the \code{tramoseats()} function. These names can be
#' used to generate customized outputs with the userdefined option of the
#' \code{tramoseats()} function (see examples).
#' The \code{tramoseats_full_dictionary} function provides additional information on
#' object format and description.
#'
#' @examplesIf rjd3jars::check_java_version()
#' # Visualize the dictionary
#' print(tramoseats_dictionary())
#' summary(tramoseats_dictionary())
#'
#' # first 10 lines
#' head(tramoseats_full_dictionary(), n = 10)
#' # For more structured information call `View(tramoseats_full_dictionary())`
#'
#' # Extract names of output of interest
#' user_defined_output <- tramoseats_dictionary()[c(65, 95, 135)]
#' user_defined_output
#'
#' # Generate the corresponding output in an estimation
#' y <- rjd3toolkit::ABS$X0.2.09.10.M
#' \donttest{
#' m <- tramoseats(y, "rsafull", userdefined=user_defined_output)
#'
#' # Retrieve user defined output
#' tail(m$user_defined$ylin)
#' m$user_defined$residuals.kurtosis
#' m$user_defined$sa_f
#' }
#'
#' @export
tramoseats_dictionary <- function() {
    output <- .jcall("jdplus/tramoseats/base/r/TramoSeats", "[S", "dictionary")
    class(output) <- "JD3_DICTIONARY"
    return(output)
}

#' @export
#' @rdname tramoseats_dictionary
tramoseats_full_dictionary <- function() {
    dico <- .jcall("jdplus/tramoseats/base/r/TramoSeats", "[S", "fullDictionary")
    dico <- `dim<-`(dico, c(6, length(dico) / 6))
    dico <- t(dico)
    dico <- `colnames<-`(dico, c("name", "description", "detail", "output", "type", "fullname"))
    dico <- as.data.frame(dico)
    class(dico) <- c("JD3_FULL_DICTIONARY", "data.frame")
    return(dico)
}
