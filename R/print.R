#'@importFrom stats printCoefmat end time
#'@importFrom utils capture.output
print_diagnostics <- function(x, digits = max(3L, getOption("digits") - 3L),
                              ...){
  diagnostics = rjd3toolkit::diagnostics(x)
  variance_decomposition = diagnostics$variance_decomposition
  residuals_test = diagnostics$residuals_test

  cat("Relative contribution of the components to the stationary",
      "portion of the variance in the original series,",
      "after the removal of the long term trend (in %)",
      sep = "\n"
  )
  cat("\n")
  cat(paste0(" ",
             capture.output(
               printCoefmat(variance_decomposition*100, digits = digits, ...)
             )),
      sep ="\n")
  cat("\n")

  cat("Residual seasonality tests")
  cat("\n")
  cat(paste0(" ",
             capture.output(
               printCoefmat(residuals_test[,"P.value", drop = FALSE], digits = digits,
                            na.print = "NA", ...)
             )
  ),
  sep ="\n")
  cat("\n")

  invisible(x)
}
print_final <- function(x, ...){
  print(rjd3toolkit::sa.decomposition(x), ...)
  invisible(x)
}

#' @export
print.JD3_SEATS <- function(x, ...) {

  print(x$seatsmodel)
  print(x$canonicaldecomposition)

  tableau <- cbind(
    x$stochastics$series$data,
    x$stochastics$sa$data,
    x$stochastics$t$data,
    x$stochastics$sa$data,
    x$stochastics$i$data
  )
  colnames(tableau) <- c("Series", "Seasonally adjusted", "Trend", "Seasonal", "Irregular")

  cat("Last values\n")
  print(tail(.preformat.ts(tableau)))

  return(invisible(x))
}

#' @export
print.JD3_TRAMO_SPEC <- function(x, ...) {

  cat("Specification", "\n", sep = "")


  cat("\n", "Series", "\n", sep = "")

  cat("Serie span: ", x$basic$span$type, "\n", sep = "")
  cat("Preliminary Check: ", ifelse(x$basic$preliminaryCheck, "Yes", "No"), "\n", sep = "")


  cat("\n", "Estimate", "\n", sep = "")

  cat("Model span: ", x$estimate$span$type, "\n", sep = "")
  cat("Tolerance: ", x$estimate$tol, "\n", sep = "")
  cat("Exact ML: ", ifelse(x$estimate$ml, "Yes", "No"), "\n", sep = "")
  cat("Unit root limit: ", x$estimate$ubp, "\n", sep = "")


  cat("\n", "Transformation", "\n", sep = "")

  cat("Function: ", x$transform$fn, "\n", sep = "")
  cat("AIC difference: ", x$transform$aicdiff, "\n", sep = "")
  cat("Adjust: ", x$transform$adjust, "\n", sep = "")


  cat("\n", "Regression", "\n", sep = "")

  if (!is.null(x$regression$td$users) && length(x$regression$td$users) > 0) {
    cat("Calendar regressor: user-defined calendar", "\n", sep = "")
    cat("Test: ", x$regression$td$test, "\n", sep = "")
  } else if (x$regression$td$td == "TD_NONE") {
    cat("No calendar regressor", "\n", sep = "")
  } else {
    cat("Calendar regressor: ", x$regression$td$td, "\n", sep = "")
    cat("with Leap Year: ", ifelse(x$regression$td$lp == "LEAPYEAR", "Yes", "No"), "\n", sep = "")
    cat("AutoAdjust: ", x$regression$td$autoadjust, "\n", sep = "")
    cat("Test: ", x$regression$td$test, "\n", sep = "")
  }
  cat("\n")

  cat("Easter: ", x$regression$easter$type, "\n", sep = "")
  cat("\n")

  cat("Pre-specified outliers: ", length(x$regression$outliers), "\n", sep = "")
  if (!is.null(x$regression$outliers) && length(x$regression$outliers) > 0) {
    for (out in x$regression$outliers) {
      cat("\t-", out$name, "\n")
    }
  }
  cat("Ramps: ", ifelse(!is.null(x$regression$ramps) && length(x$regression$ramps) > 0, "Yes", "No"), "\n", sep = "")
  cat("User-defined variables: ", ifelse(!is.null(x$regression$users) && length(x$regression$users) > 0, "Yes", "No"), "\n", sep = "")


  cat("\n", "Outliers", "\n", sep = "")

  if (is.null(x$outlier$outliers) || length(x$outlier$outliers) == 0) {
    cat("Is enabled: No\n")
  } else {
    cat("Detection span: ", x$outlier$span$type, sep = "")
    if (toupper(x$outlier$span$type) %in% c("FROM", "BETWEEN")) {
      cat(" from", x$outlier$span$d0)
    }
    if (toupper(x$outlier$span$type) %in% c("TO", "BETWEEN")) {
      cat(" to", x$outlier$span$d1)
    }
    if (x$outlier$span == "All") {
      cat("Detection span: All\n")
    }
    cat("\n")

    list_outliers <- c("ao", "ls", "tc", "so")
    detected_outliers <- c("ao", "ls", "tc", "so")[do.call(
      args = x$outlier[c("ao", "ls", "tc", "so")],
      what = c)]

    if (length(detected_outliers) > 0) {
      cat("Outliers type: ", paste(detected_outliers, collapse = ", "), "\n", sep = "")
    }

    cat("Critical value: ", ifelse(x$outlier$va == 0, "0 (Auto)", x$outlier$va), "\n", sep = "")
    cat("TC rate: ", ifelse(x$outlier$tcrate == 0.7, "0,7 (Auto)", x$outlier$tcrate), "\n", sep = "")
    cat("EML estimation: ", ifelse(x$outlier$ml, "Yes", "No"), "\n", sep = "")
  }


  cat("\n", "ARIMA", "\n", sep = "")

  print(x$arima)

  cat("\n")
  return(invisible(x))
}


#' @export
print.JD3_SEATS_SPEC <- function(x, ...) {

  cat("Specification SEATS", "\n", sep = "")


  cat("Approximation mode: ", x$approximation, "\n", sep = "")
  cat("MA unit root boundary: ", x$xl, "\n", sep = "")
  cat("Trend boundary: ", x$rmod, "\n", sep = "")
  cat("Seasonal tolerance: ", x$epsphi, "\n", sep = "")
  cat("Seasonal boundary: ", x$sbound, "\n", sep = "")
  cat("Method: ", x$algorithm, "\n", sep = "")

  return(invisible(x))
}

#' @export
print.JD3_TRAMOSEATS_SPEC <- function(x, ...) {

  print(x$tramo, ...)
  print(x$seats, ...)

  cat("\n", "Benchmarking", "\n", sep = "")

  if (!x$benchmarking$enabled) {
    cat("Is enabled: No\n")
  } else {
    cat("Enabled: Yes", sep = "")
    cat("Target: ", x$benchmarking$target, "\n", sep = "")
    cat("Lambda: ", x$benchmarking$lambda, "\n", sep = "")
    cat("Rho: ", x$benchmarking$rho, "\n", sep = "")
    cat("Use forecast: ", ifelse(x$benchmarking$forecast, "Yes", "No"), "\n", sep = "")
  }

  cat("\n")
  return(invisible(x))
}

#' @export
print.JD3_TRAMOSEATS_RSLTS <- function(x, digits = max(3L, getOption("digits") - 3L),
                                ...){

  cat("TRAMO","\n",sep="")
  print(x$preprocessing, digits = digits, ...)
  cat("\n", "Decomposition","\n",sep="")
  print(x$decomposition$canonicaldecomposition, ...)
  cat("\n", "Diagnostics","\n",sep="")
  print_diagnostics(x, digits = digits, ...)
  cat("\n", "Final","\n",sep="")
  print_final(x, digits = digits, ...)

  return(invisible(x))
}

#' @export
print.JD3_TRAMOSEATS_OUTPUT<- function(x, digits = max(3L, getOption("digits") - 3L),
                                ...){
  print(x$result, digits = digits, ...)

  return(invisible(x))
}

#' @export
plot.JD3_TRAMOSEATS_RSLTS <- function(x, first_date = NULL, last_date = NULL,
                               type_chart = c("sa-trend", "seas-irr"),
                               caption = c("sa-trend" = "Y, Sa, trend",
                                           "seas-irr" = "Sea., irr.")[type_chart],
                               colors = c(y = "#F0B400", t = "#1E6C0B", sa = "#155692",
                                          s = "#1E6C0B", i = "#155692"),
                               ...){
  plot(rjd3toolkit::sa.decomposition(x),
       first_date = first_date, last_date = last_date,
       type_chart = type_chart,
       caption = caption,
       colors = colors,
       ...)
}
#' @export
plot.JD3_TRAMOSEATS_OUTPUT <- function(x, first_date = NULL, last_date = NULL,
                                type_chart = c("sa-trend", "seas-irr"),
                                caption = c("sa-trend" = "Y, Sa, trend",
                                            "seas-irr" = "Sea., irr.")[type_chart],
                                colors = c(y = "#F0B400", t = "#1E6C0B", sa = "#155692",
                                           s = "#1E6C0B", i = "#155692"),
                                ...){
  plot(x$result,
       first_date = first_date, last_date = last_date,
       type_chart = type_chart,
       caption = caption,
       colors = colors,
       ...)
}

#' @importFrom rjd3toolkit diagnostics
#' @export
diagnostics.JD3_TRAMOSEATS_RSLTS<-function(x, ...){
  if (is.null(x)) return (NULL)
  variance_decomposition = x$diagnostics$vardecomposition
  variance_decomposition = matrix(unlist(variance_decomposition),
                                  ncol = 1,
                                  dimnames = list(names(variance_decomposition), "Component"))
  residuals_test = x$diagnostics[grep("test", names(x$diagnostics))]
  residuals_test = data.frame(Statistic = sapply(residuals_test, function(test) test[["value"]]),
                              P.value = sapply(residuals_test, function(test) test[["pvalue"]]),
                              Description = sapply(residuals_test, function(test) attr(test, "distribution")))
  list(variance_decomposition = variance_decomposition,
       residuals_test = residuals_test)
}

#' @export
diagnostics.JD3_TRAMOSEATS_OUTPUT<-function(x, ...){
  return (rjd3toolkit::diagnostics(x$result, ...))
}

