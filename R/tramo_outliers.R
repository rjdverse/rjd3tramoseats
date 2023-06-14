#' @include utils.R
#' @importFrom stats is.ts frequency
NULL

#' Outlier Detection with a Tramo Model
#'
#' Tramo is a particular regarima model estimation algorithm, mainly used to linearized the series before performing a decomposition with Seats
#'
#' @param y the dependent variable (a `ts` object).
#' @param order,seasonal the orders of the ARIMA model.
#' @param mean Boolean to include or not the mean.
#' @param X user defined regressors (other than calendar).
#' @param X.td calendar regressors.
#' @param ao,ls,so,tc Boolean to indicate which type of outliers should be detected.
#' @param cv  `numeric`. The entered critical value for the outliers' detection procedure.
#' If equal to 0 the critical value for the outliers' detection procedure is automatically determined
#' by the number of observations.
#' @param ml Use of maximum likelihood (otherwise approximation by means of Hannan-Rissanen).
#' @param clean Clean missing values at the beginning/end of the series. Regression variables are automatically resized, if need be.
#'
#' @return a `"JDSTS"` object.
#'
#' @examples
#' tramo_outliers(rjd3toolkit::ABS$X0.2.09.10.M)
#' @export
tramo_outliers<-function(y, order=c(0L,1L,1L), seasonal=c(0L,1L,1L), mean=F,
                      X=NULL, X.td=NULL, ao=T, ls=T, tc=F, so=F, cv=0, ml=F, clean=F){
  if (!is.ts(y)){
    stop("y must be a time series")
  }
  if (! is.null(X.td)){
    td<-rjd3toolkit::td(s = y, groups=X.td)
    X<-cbind(X, td)
  }


  jtramo<-.jcall("jdplus/tramoseats/base/r/TramoOutliersDetection", "Ljdplus/tramoseats/base/r/TramoOutliersDetection$Results;", "process", rjd3toolkit::.r2jd_ts(y),
               as.integer(order), as.integer(seasonal), mean, rjd3toolkit::.r2jd_matrix(X),
               ao, ls, tc, so, cv, ml, clean)
  model<-list(
    y=rjd3toolkit::.proc_ts(jtramo, "y"),
    variables=rjd3toolkit::.proc_vector(jtramo, "variables"),
    X=rjd3toolkit::.proc_matrix(jtramo, "regressors"),
    b=rjd3toolkit::.proc_vector(jtramo, "b"),
    bcov=rjd3toolkit::.proc_matrix(jtramo, "bvar"),
    linearized=rjd3toolkit::.proc_vector(jtramo, "linearized")
  )

  ll0<-rjd3toolkit::.proc_likelihood(jtramo, "initiallikelihood.")
  ll1<-rjd3toolkit::.proc_likelihood(jtramo, "finallikelihood.")

  return(structure(list(
    model=model,
    likelihood=list(initial=ll0, final=ll1)),
    class="JD3_REGARIMA_OUTLIERS"))
}
