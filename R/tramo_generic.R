#' @importFrom stats coef df.residual logLik residuals vcov nobs
#' @export
coef.JD3_TRAMO_OUTPUT <- function(object, component = c("regression", "arima", "both"), ...){
  coef(object$result, component = component, ...)
}
#' @export
logLik.JD3_TRAMO_OUTPUT <- function(object, ...) {
  logLik(object$result, ...)
}
#' @export
vcov.JD3_TRAMO_OUTPUT <- function(object, ...){
  vcov(object$result, ...)
}
#' @export
df.residual.JD3_TRAMO_OUTPUT <- function(object, ...){
  df.residual(object$result, ...)
}
#' @export
nobs.JD3_TRAMO_OUTPUT <- function(object, ...){
  nobs(object$result, ...)
}
#' @export
residuals.JD3_TRAMO_OUTPUT <- function(object, ...){
  residuals(object$result, ...)
}
#' @export
summary.JD3_TRAMO_OUTPUT <- function(object, ...){
  x <- summary(object$result, ...)
  x$method <- "TRAMO"
  x
}
#' @export
print.JD3_TRAMO_OUTPUT <- function(x, summary_info = getOption("summary_info"), ...){
  cat("Method: TRAMO\n")
  print(x$result, summary_info = summary_info, ...)
}
#' @export
diagnostics.JD3_TRAMO_OUTPUT <- function(x, ...){
  diagnostics(x$result, ...)
}

#' @export
coef.JD3_TRAMOSEATS_OUTPUT <- function(object, component = c("regression", "arima", "both"), ...){
  coef(object$result$preprocessing, component = component, ...)
}
#' @export
logLik.JD3_TRAMOSEATS_OUTPUT <- function(object, ...) {
  logLik(object$result$preprocessing, ...)
}
#' @export
vcov.JD3_TRAMOSEATS_OUTPUT <- function(object, ...){
  vcov(object$result$preprocessing, ...)
}
#' @export
df.residual.JD3_TRAMOSEATS_OUTPUT <- function(object, ...){
  df.residual(object$result$preprocessing, ...)
}
#' @export
nobs.JD3_TRAMOSEATS_OUTPUT <- function(object, ...){
  nobs(object$result$preprocessing, ...)
}
#' @export
residuals.JD3_TRAMOSEATS_OUTPUT <- function(object, ...){
  residuals(object$result$preprocessing, ...)
}
#' @export
residuals.JD3_TRAMOSEATS_OUTPUT <- function(object, ...){
  residuals(object$result$preprocessing, ...)
}
