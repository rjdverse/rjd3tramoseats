#' @include utils.R tramoseats_spec.R tramoseats_rslts.R
NULL

.jrevisions<-function(jts, jspec, jcontext){
  jrslt<-.jcall("jdplus/tramoseats/base/r/TramoSeatsRevisionHistory",
                "Ljdplus/tramoseats/base/r/TramoSeatsRevisionHistory$Revisions;", "revisions", jts, jspec, jcontext)
  return (jrslt)
}


#' Title
#'
#' @param ts
#' @param spec
#' @param context
#' @param start
#' @param period
#' @param data_ids
#' @param ts_ids
#'
#' @return
#' @export
#'
#' @examples
#' s<-rjd3toolkit::ABS$X0.2.09.10.M
#' q<-rjd3tramoseats::tramoseats(s)
#' spec<-rjd3tramoseats::tramoseats_refresh(q$result_spec)
#' ts_ids<-list(list(period="2010-01-01", start="2010-01-01", id="sa"), list(period="2010-01-01", start="2015-01-01", id="i"))
#' data_ids<-list(list(start="2005-01-01", id="regression.td(1)"), list(start="2010-01-01", id="residuals.lb"))
#' cmp_ids<-list(list(start="2010-01-01", end="2020-01-01", id="sa"), list(start="2010-01-01", end="2020-01-01", id="t"))
#' ww<-revisions(s, spec, data_ids, ts_ids, cmp_ids)
revisions<-function(ts, spec, data_ids=NULL, ts_ids=NULL, cmp_ids=NULL, context=NULL){
  jts<-rjd3toolkit::.r2jd_ts(ts)
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
      return (rjd3toolkit::.jd2r_ts(w))
    })
    ldata<-`names<-`(ldata, data_ids)
  }
  lts<-NULL
  if (! is.null(ts_ids)){
    lts<-lapply(ts_ids, function(ts_id){
      w<-.jcall(jr, "Ljdplus/toolkit/base/api/timeseries/TsData;", "tsHistory", ts_id$id, ts_id$period, ts_id$start)
      return (rjd3toolkit::.jd2r_ts(w))
    })
    lts<-`names<-`(lts, sapply(ts_ids, function(z)z$id))
  }
  lcmp<-NULL
  if (! is.null(cmp_ids)){
    lcmp<-lapply(cmp_ids, function(cmp_id){
      w<-.jcall(jr, "Ljdplus/toolkit/base/api/timeseries/TsDataTable;", "tsSelect", cmp_id$id, cmp_id$start, cmp_id$end)
      return (rjd3toolkit::.jd2r_mts(w))
    })
    lcmp<-`names<-`(lcmp, sapply(cmp_ids, function(z)z$id))
  }

  return (list(data=ldata, series=lts, components=lcmp))
}
