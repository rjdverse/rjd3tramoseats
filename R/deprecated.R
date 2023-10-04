#' Deprecated functions
#'
#'
#' @param ts,spec,context,userdefined,name Parameters.
#' @name deprecated-rjd3tramoseats
#' @export
fast_tramoseats<-function(ts, spec=c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"), context=NULL, userdefined = NULL){
  .Deprecated("tramoseats_fast")
  tramoseats_fast(ts, spec, context, userdefined)
}
#' @name deprecated-rjd3tramoseats
#' @export
fast_tramo<-function(ts, spec=c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"), context=NULL, userdefined = NULL){
  .Deprecated("tramo_fast")
  tramo_fast(ts, spec, context, userdefined)
}
#' @name deprecated-rjd3tramoseats
#' @export
spec_tramoseats<-function(name=c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")){
  .Deprecated("tramoseats_spec")
  tramoseats_spec(name)
}
#' @name deprecated-rjd3tramoseats
#' @export
spec_tramo<-function(name=c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")){
  .Deprecated("tramo_spec")
  tramo_spec(name)
}
