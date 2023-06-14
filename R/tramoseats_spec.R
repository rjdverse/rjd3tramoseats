#' @include utils.R
NULL

#' TRAMO/TRAMO-SEATS Default Specification
#'
#' Set of functions(`spec_tramoseats()`,`spec_tramo()`) to create default specifications associated with the TRAMO-SEATS seasonal adjustment method.
#' Specification creation can be restricted to the tramo part with the `spec_tramo()` function.
#'
#' Without argument `spec_tramo()` yields a TR5 specification
#'
#' without argument `spec_tramoseats()`  yields a RSA5 specification
#'
#' @param name the name of a predefined specification.
#'
#' @examples
#' init_spec <- spec_tramoseats()
#' init_spec <- spec_tramo()
#' init_spec <- spec_tramoseats("rsa3")
#' init_spec <- spec_tramo("tr3")
#'
#' @return an object of class `"JD3_TRAMOSEATS_SPEC"` (`spec_tramoseats()`) or
#' `"JD3_TRAMO_SPEC"` (`spec_tramo()`).
#'
#' @details
#' The available predefined 'JDemetra+' model specifications are described in the table below:
#'
#' \tabular{rrrrrrrr}{
#' \strong{Identifier} |\tab \strong{Log/level detection} |\tab \strong{Outliers detection} |\tab \strong{Calendar effects} |\tab \strong{ARIMA}\cr
#' RSA0/TR0 |\tab \emph{NA} |\tab \emph{NA} |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA1/TR1 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab Airline(+mean)\cr
#' RSA2/TR2 |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab Airline(+mean)\cr
#' RSA3/TR3 |\tab automatic |\tab AO/LS/TC |\tab \emph{NA} |\tab automatic\cr
#' RSA4/TR3 |\tab automatic |\tab AO/LS/TC |\tab 2 td vars + Easter |\tab automatic\cr
#' RSA5/TR5 |\tab automatic |\tab AO/LS/TC |\tab 7 td vars + Easter |\tab automatic\cr
#' RSAfull/TRfull |\tab automatic |\tab AO/LS/TC |\tab automatic |\tab automatic
#' }
#' @name tramoseats_spec
#' @rdname tramoseats_spec
#' @export
spec_tramo<-function(name=c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")){
  name = gsub("rsa", "tr", tolower(name), fixed = TRUE)
  name = match.arg(name[1],
                   choices = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")
  )
  jspec<-.jcall("jdplus/tramoseats/base/api/tramo/TramoSpec", "Ljdplus/tramoseats/base/api/tramo/TramoSpec;", "fromString", name)
  return (.jd2r_spec_tramo(jspec))
}


#' @rdname tramoseats_spec
#' @export
spec_tramoseats<-function(name=c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")){
  name = gsub("tr", "rsa", tolower(name), fixed = TRUE)
  name = match.arg(name[1],
                   choices = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
  )
  jspec<-.jcall("jdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec", "Ljdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec;", "fromString", name)
  return (.jd2r_spec_tramoseats(jspec))
}

## JD <-> R

#' @export
#' @rdname jd3_utilities
.jd2r_spec_tramo<-function(jspec){
  q<-.jcall("jdplus/tramoseats/base/r/Tramo", "[B", "toBuffer", jspec)
  rq<-RProtoBuf::read(tramoseats.TramoSpec, q)
  return (.p2r_spec_tramo(rq))
}

#' @export
#' @rdname jd3_utilities
.r2jd_spec_tramo<-function(spec){
  pspec<-.r2p_spec_tramo(spec)
  nq<-RProtoBuf::serialize(pspec, NULL)
  nspec<-.jcall("jdplus/tramoseats/base/r/Tramo", "Ljdplus/tramoseats/base/api/tramo/TramoSpec;", "specOf", nq)
  return (nspec)
}

#' @export
#' @rdname jd3_utilities
.jd2r_spec_tramoseats<-function(jspec){
  q<-.jcall("jdplus/tramoseats/base/r/TramoSeats", "[B", "toBuffer", jspec)
  rq<-RProtoBuf::read(tramoseats.Spec, q)
  return (.p2r_spec_tramoseats(rq))
}

#' @export
#' @rdname jd3_utilities
.r2jd_spec_tramoseats<-function(spec){
  pspec<-.r2p_spec_tramoseats(spec)
  nq<-RProtoBuf::serialize(pspec, NULL)
  nspec<-.jcall("jdplus/tramoseats/base/r/TramoSeats", "Ljdplus/tramoseats/base/api/tramoseats/TramoSeatsSpec;", "specOf", nq)
  return (nspec)
}


## P <-> R


.p2r_spec_tramo<-function(pspec){
  b<-pspec$basic
  basic<-list(
    span=rjd3toolkit::.p2r_span(b$span),
    preliminaryCheck = b$preliminary_check
    )
  t<-pspec$transform
  transform=list(
    fn=rjd3toolkit::.enum_extract(modelling.Transformation, t$transformation),
    fct=t$fct,
    adjust=rjd3toolkit::.enum_extract(modelling.LengthOfPeriod, t$adjust),
    outliers=t$outliers_correction
    )
  a<-pspec$automodel
  automodel=list(
    enabled=a$enabled,
    acceptdef=a$accept_def,
    cancel=a$cancel,
    ub1=a$ub1,
    ub2=a$ub2,
    pcr=a$pcr,
    pc=a$pc,
    tsig=a$tsig,
    amicompare=a$ami_compare
    )
  arima=rjd3toolkit::.p2r_spec_sarima(pspec$arima)
  o<-pspec$outlier
  outlier<-list(enabled=o$enabled, span=rjd3toolkit::.p2r_span(o$span), ao=o$ao, ls=o$ls, tc=o$tc, so=o$so, va=o$va, tcrate=o$tcrate, ml=o$ml)
  r<-pspec$regression
  ptd<-pspec$regression$td
  pee<-pspec$regression$easter
  td<-list(
    td=rjd3toolkit::.enum_sextract(modelling.TradingDays, ptd$td),
    lp=rjd3toolkit::.enum_extract(modelling.LengthOfPeriod, ptd$lp),
    holidays=ptd$holidays,
    users=unlist(ptd$users),
    w=ptd$w,
    test=rjd3toolkit::.enum_extract(tramoseats.TradingDaysTest, ptd$test),
    auto=rjd3toolkit::.enum_extract(tramoseats.AutomaticTradingDays, ptd$auto),
    ptest=ptd$ptest,
    autoadjust=ptd$auto_adjust,
    tdcoefficients=rjd3toolkit::.p2r_parameters(ptd$tdcoefficients),
    lpcoefficient=rjd3toolkit::.p2r_parameter(ptd$lpcoefficient)
    )
  easter<-list(type=rjd3toolkit::.enum_extract(tramoseats.EasterType, pee$type), duration=pee$duration, julian=pee$julian, test=pee$test,
               coefficient=rjd3toolkit::.p2r_parameter(pee$coefficient))
  # TODO: complete regression
  regression<-list(
    mean=rjd3toolkit::.p2r_parameter(r$mean),
    check_mean=r$check_mean,
    td=td,
    easter=easter,
    outliers=rjd3toolkit::.p2r_outliers(r$outliers),
    ramps=rjd3toolkit::.p2r_ramps(r$ramps),
    users=rjd3toolkit::.p2r_uservars(r$users)
  )
  e<-pspec$estimate
  estimate<-list(span=rjd3toolkit::.p2r_span(e$span), ml=e$ml, tol=e$tol, ubp=e$ubp)
  return (structure(
    list(basic=basic, transform=transform, outlier=outlier,
         arima=arima, automodel=automodel, regression=regression, estimate=estimate),
    class="JD3_TRAMO_SPEC"))
}


.r2p_spec_tramo<-function(rspec){
  pspec<-tramoseats.TramoSpec$new()
  # BIAS
  pspec$basic$span<-rjd3toolkit::.r2p_span(rspec$basic$span)
  pspec$basic$preliminary_check<-rspec$basic$preliminaryCheck

  # TRANSFORM
  pspec$transform$transformation<-rjd3toolkit::.enum_of(modelling.Transformation, rspec$transform$fn, "FN")
  pspec$transform$fct<-rspec$transform$fct
  pspec$transform$adjust<-rspec$transform$adjust<-rjd3toolkit::.enum_of(modelling.LengthOfPeriod, rspec$transform$adjust, "LP")
  pspec$transform$outliers_correction<-rspec$transform$outliers

  #OUTLIER

  pspec$outlier$enabled<-rspec$outlier$enabled
  pspec$outlier$span<-rjd3toolkit::.r2p_span(rspec$outlier$span)
  pspec$outlier$ao<-rspec$outlier$ao
  pspec$outlier$ls<-rspec$outlier$ls
  pspec$outlier$tc<-rspec$outlier$tc
  pspec$outlier$so<-rspec$outlier$so
  pspec$outlier$va<-rspec$outlier$va
  pspec$outlier$tcrate=rspec$outlier$tcrate
  pspec$outlier$ml<-rspec$outlier$ml

  #AMI

  pspec$automodel$enabled<-rspec$automodel$enabled
  pspec$automodel$cancel<-rspec$automodel$cancel
  pspec$automodel$ub1<-rspec$automodel$ub1
  pspec$automodel$ub2<-rspec$automodel$ub2
  pspec$automodel$pcr<-rspec$automodel$pcr
  pspec$automodel$pc<-rspec$automodel$pc
  pspec$automodel$tsig<-rspec$automodel$tsig
  pspec$automodel$accept_def<-rspec$automodel$acceptdef
  pspec$automodel$ami_compare<-rspec$automodel$amicompare

  #ARIMA
  pspec$arima<-rjd3toolkit::.r2p_spec_sarima(rspec$arima)

  #REGRESSION

  pspec$regression$mean<-rjd3toolkit::.r2p_parameter(rspec$regression$mean)
  pspec$regression$check_mean<-rspec$regression$check_mean
  pspec$regression$outliers<-rjd3toolkit::.r2p_outliers(rspec$regression$outliers)
  pspec$regression$ramps<-rjd3toolkit::.r2p_ramps(rspec$regression$ramps)

  #TD
  pspec$regression$td$td<-rjd3toolkit::.enum_sof(modelling.TradingDays, rspec$regression$td$td)
  pspec$regression$td$lp<-rjd3toolkit::.enum_of(modelling.LengthOfPeriod, rspec$regression$td$lp, "LP")
  pspec$regression$td$holidays<-rspec$regression$td$holidays
  pspec$regression$td$users<-rspec$regression$td$users
  pspec$regression$td$w<-rspec$regression$td$w
  pspec$regression$td$test <-rjd3toolkit::.enum_of(tramoseats.TradingDaysTest, rspec$regression$td$test, "TD")
  pspec$regression$td$auto <-rjd3toolkit::.enum_of(tramoseats.AutomaticTradingDays, rspec$regression$td$auto, "TD")
  pspec$regression$td$auto_adjust <-rspec$regression$td$autoadjust
  pspec$regression$td$ptest<-rspec$regression$td$ptest
  pspec$regression$td$tdcoefficients<-rjd3toolkit::.r2p_parameters(rspec$regression$td$tdcoefficients)
  pspec$regression$td$lpcoefficient<-rjd3toolkit::.r2p_parameter(rspec$regression$td$lpcoefficient)

  #EASTER
  pspec$regression$easter$type<-rjd3toolkit::.enum_of(tramoseats.EasterType, rspec$regression$easter$type, "EASTER")
  pspec$regression$easter$duration<-rspec$regression$easter$duration
  pspec$regression$easter$julian<-rspec$regression$easter$julian
  pspec$regression$easter$test<-rspec$regression$easter$test
  pspec$regression$easter$coefficient<-rjd3toolkit::.r2p_parameter(rspec$regression$easter$coefficient)

  pspec$regression$users <- rjd3toolkit::.r2p_uservars(rspec$regression$users)
  #ESTIMATE
  pspec$estimate$span<-rjd3toolkit::.r2p_span(rspec$estimate$span)
  pspec$estimate$ml-rspec$estimate$ml
  pspec$estimate$tol<-rspec$estimate$tol
  pspec$estimate$ubp<-rspec$estimate$ubp

  return (pspec)
}

# SEATS

.p2r_spec_seats<-function(spec){
  return (structure(list(
    xl=spec$xl_boundary,
    approximation=rjd3toolkit::.enum_extract(tramoseats.SeatsApproximation, spec$approximation),
    epsphi=spec$seastolerance,
    rmod=spec$trend_boundary,
    sbound=spec$seas_boundary,
    sboundatpi=spec$seas_boundary_at_pi,
    bias=spec$bias_correction,
    nfcasts=spec$nfcasts,
    nbcasts=spec$nbcasts,
    algorithm=rjd3toolkit::.enum_extract(tramoseats.SeatsAlgorithm, spec$algorithm)
  ), class = "JD3_SEATS_SPEC"))
}

.r2p_spec_seats<-function(spec){
  pspec<-tramoseats.DecompositionSpec$new()
  pspec$xl_boundary<-spec$xl
  pspec$approximation<-rjd3toolkit::.enum_of(tramoseats.SeatsApproximation, spec$approximation, "SEATS")
  pspec$seastolerance<-spec$epsphi
  pspec$trend_boundary<-spec$rmod
  pspec$seas_boundary<-spec$sbound
  pspec$seas_boundary_at_pi<-spec$sboundatpi
  pspec$bias_correction<-spec$bias
  pspec$nfcasts<-spec$nfcasts
  pspec$nbcasts<-spec$nbcasts
  pspec$algorithm<-rjd3toolkit::.enum_of(tramoseats.SeatsAlgorithm, spec$algorithm, "SEATS")
  return (pspec)
}

.p2r_spec_tramoseats<-function(pspec){
  return (structure(list(
    tramo=.p2r_spec_tramo(pspec$tramo),
    seats=.p2r_spec_seats(pspec$seats),
    benchmarking=rjd3toolkit::.p2r_spec_benchmarking(pspec$benchmarking)
    ), class="JD3_TRAMOSEATS_SPEC"))
}

.r2p_spec_tramoseats<-function(r){
  p<-tramoseats.Spec$new()
  p$tramo<-.r2p_spec_tramo(r$tramo)
  p$seats<-.r2p_spec_seats(r$seats)
  p$benchmarking<-rjd3toolkit::.r2p_spec_benchmarking(r$benchmarking)
  return (p)
}
