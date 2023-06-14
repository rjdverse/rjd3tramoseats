#' @include utils.R
NULL


#' SEATS Decomposition
#'
#' @param sarima SARIMA model (see [rjd3toolkit::sarima_model()]).
#' @inheritParams set_seats
#'
#'
#' @examples
#' seats_decompose(rjd3toolkit::sarima_model(period = 12,phi = c(0,1),bd = 1))
#' @export
seats_decompose<-function(sarima, seas.tolerance=2, trend.boundary=.5, seas.boundary=.8,
                          seas.boundary.unique=.8, approximation=c("None", "Legacy", "Noisy")){
  if (!inherits(sarima, "JD3_SARIMA"))
    stop("Invalid model")
  approximation<-match.arg(approximation)
  jsarima<-rjd3toolkit::.r2jd_sarima(sarima)
  jucm<-.jcall("jdplus/tramoseats/base/r/Seats", "Ljdplus/toolkit/base/core/ucarima/UcarimaModel;", "decompose",
         jsarima, seas.tolerance, trend.boundary, seas.boundary, seas.boundary.unique, approximation)
  if (is.jnull(jucm)){
    return (NULL)
  } else {
    return (rjd3toolkit::.jd2r_ucarima(jucm))
  }
}
