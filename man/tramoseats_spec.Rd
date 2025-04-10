% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tramoseats_spec.R
\name{tramoseats_spec}
\alias{tramoseats_spec}
\alias{tramo_spec}
\title{TRAMO/TRAMO-SEATS Default Specification}
\usage{
tramo_spec(name = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"))

tramoseats_spec(
  name = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
)
}
\arguments{
\item{name}{the name of a predefined specification.}
}
\value{
an object of class \code{"JD3_TRAMOSEATS_SPEC"} (\code{tramoseats_spec()}) or
\code{"JD3_TRAMO_SPEC"} (\code{tramo_spec()}).
}
\description{
Set of functions(\code{tramoseats_spec()},\code{tramo_spec()}) to create default specifications associated with the TRAMO-SEATS seasonal adjustment method.
Specification creation can be restricted to the tramo part with the \code{tramo_spec()} function.
}
\details{
Without argument \code{tramo_spec()} yields a TR5 specification

without argument \code{tramoseats_spec()}  yields a RSA5 specification

The available predefined 'JDemetra+' model specifications are described in the table below:

\tabular{rrrrrrrr}{
\strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calendar effects} |\tab \strong{ARIMA}\cr
RSA0/TR0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
RSA1/TR1 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab Airline(+mean)\cr
RSA2/TR2 |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
RSA3/TR3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
RSA4/TR3 |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
RSA5/TR5 |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic\cr
RSAfull/TRfull |\tab automatic |\tab AO/LS/TC |\tab automatic |\tab automatic
}
}
\examples{
init_spec <- tramoseats_spec()
init_spec <- tramo_spec()
init_spec <- tramoseats_spec("rsa3")
init_spec <- tramo_spec("tr3")

}
\seealso{
\itemize{
\item To set the pre-processing parameters:
\code{\link[rjd3toolkit:set_arima]{rjd3toolkit::set_arima()}}, \code{\link[rjd3toolkit:set_automodel]{rjd3toolkit::set_automodel()}},
\code{\link[rjd3toolkit:set_basic]{rjd3toolkit::set_basic()}}, \code{\link[rjd3toolkit:set_easter]{rjd3toolkit::set_easter()}},
\code{\link[rjd3toolkit:set_estimate]{rjd3toolkit::set_estimate()}}, \code{\link[rjd3toolkit:set_outlier]{rjd3toolkit::set_outlier()}},
\code{\link[rjd3toolkit:set_tradingdays]{rjd3toolkit::set_tradingdays()}}, \code{\link[rjd3toolkit:set_transform]{rjd3toolkit::set_transform()}},
\code{\link[rjd3toolkit:add_outlier]{rjd3toolkit::add_outlier()}}, \code{\link[rjd3toolkit:add_outlier]{rjd3toolkit::remove_outlier()}},
\code{\link[rjd3toolkit:add_outlier]{rjd3toolkit::add_ramp()}}, \code{\link[rjd3toolkit:add_outlier]{rjd3toolkit::remove_ramp()}},
\code{\link[rjd3toolkit:add_usrdefvar]{rjd3toolkit::add_usrdefvar()}}.
\item To set the decomposition parameters: \code{\link[=set_seats]{set_seats()}}.
\item To set the benchmarking parameters: \code{\link[rjd3toolkit:set_benchmarking]{rjd3toolkit::set_benchmarking()}}.
}
}
