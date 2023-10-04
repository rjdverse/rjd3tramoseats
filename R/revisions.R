#' @include utils.R tramoseats_spec.R tramoseats_rslts.R
NULL

.jrevisions<-function(jts, jspec, jcontext){
  jrslt<-.jcall("jdplus/tramoseats/base/r/TramoSeatsRevisionHistory",
                "Ljdplus/toolkit/base/r/timeseries/Revisions;", "revisions", jts, jspec, jcontext)
  return (jrslt)
}


#' Revisions History
#'
#' Compute revisions history
#'
#' @param ts The time series used for the estimation.
#' @param spec The specification used.
#' @param data_ids A `list` of `list` to specify the statistics to export.
#' Each sub-list must contain two elements:
#' `start` (first date to compute the history, in the format `"YYYY-MM-DD"`)
#' and `id` (the name of the statistics, see [tramoseats_dictionary()]).
#' See example.
#' @param ts_ids A `list` of `list` to specify the specific date of a component whose history is to be studied.
#' Each sub-list must contain three elements:
#' `start` (first date to compute the history, in the format `"YYYY-MM-DD"`),
#' `period` (the date of the studied)
#' and `id` (the name of the component, see [tramoseats_dictionary()]).
#' See example.
#' @param cmp_ids A `list` of `list` to specify the component whose history is to be studied.
#' Each sub-list must contain three elements:
#' `start` (first date to compute the history, in the format `"YYYY-MM-DD"`),
#' `end` (last date to compute the history, in the format `"YYYY-MM-DD"`)
#' and `id` (the name of the component, see [tramoseats_dictionary()]).
#' As many series as periods between `start` and `end` will be exported.
#' See example.
#' @param context The context of the specification.
#'
#' @examples
#' s <- rjd3toolkit::ABS$X0.2.09.10.M
#' sa_mod <- tramoseats(s)
#' data_ids <- list(
#'   # Get the coefficient of the trading-day coefficient from 2005-jan
#'   list(start = "2005-01-01", id = "regression.td(1)"),
#'   # Get the ljung-box statistics on residuals from 2010-jan
#'   list(start = "2010-01-01", id = "residuals.lb"))
#' ts_ids <- list(
#'   # Get the SA component estimates of 2010-jan from 2010-jan
#'   list(period = "2010-01-01", start = "2010-01-01", id = "sa"),
#'   # Get the irregular component estimates of 2010-jan from 2015-jan
#'   list(period = "2010-01-01", start = "2015-01-01", id = "i"))
#' cmp_ids <- list(
#'   # Get the SA component estimates (full time series) 2010-jan to 2020-jan
#'   list(start = "2010-01-01", end = "2020-01-01", id = "sa"),
#'   # Get the trend component estimates (full time series)  2010-jan to 2020-jan
#'   list(start = "2010-01-01", end = "2020-01-01", id = "t"))
#' rh <- tramoseats_revisions(s, sa_mod$result_spec, data_ids, ts_ids, cmp_ids)
#' @export
tramoseats_revisions<-function(ts, spec, data_ids=NULL, ts_ids=NULL, cmp_ids=NULL, context=NULL){
  jts<-rjd3toolkit::.r2jd_tsdata(ts)
  jspec<-.r2jd_spec_tramoseats(spec)
  if (is.null(context)){
    jcontext <- .jnull("jdplus/toolkit/base/api/timeseries/regression/ModellingContext")
  } else {
    jcontext <- rjd3toolkit::.r2jd_modellingcontext(context)
  }
  ldata<-NULL
  jr<-.jrevisions(jts, jspec, jcontext)
  if (! is.null(data_ids)){
    ldata<-lapply(data_ids, function(data_id){
      w<-.jcall(jr, "Ljdplus/toolkit/base/api/timeseries/TsData;", "history", data_id$id, data_id$start)
      return (rjd3toolkit::.jd2r_tsdata(w))
    })
    names(ldata) <- sapply(data_ids, `[[`,"id")
  }
  lts<-NULL
  if (! is.null(ts_ids)){
    lts<-lapply(ts_ids, function(ts_id){
      w<-.jcall(jr, "Ljdplus/toolkit/base/api/timeseries/TsData;", "tsHistory", ts_id$id, ts_id$period, ts_id$start)
      return (rjd3toolkit::.jd2r_tsdata(w))
    })
    names(lts) <- sapply(ts_ids, `[[`,"id")
  }
  lcmp<-NULL
  if (! is.null(cmp_ids)){
    lcmp<-lapply(cmp_ids, function(cmp_id){
      w<-.jcall(jr, "Ljdplus/toolkit/base/api/timeseries/TsDataTable;", "tsSelect", cmp_id$id, cmp_id$start, cmp_id$end)
      return (rjd3toolkit::.jd2r_mts(w))
    })
    names(lcmp) <- sapply(cmp_ids, `[[`,"id")
  }

  return (list(data=ldata, series=lts, components=lcmp))
}
