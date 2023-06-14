#' @include utils.R
NULL

.onLoad <- function(libname, pkgname) {
  if (! requireNamespace("rjd3toolkit", quietly = T)) stop("Loading rjd3 libraries failed")

  result <- rJava::.jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  proto.dir <- system.file("proto", package = pkgname)
  RProtoBuf::readProtoFiles2(protoPath = proto.dir)

  # reload extractors
  .jcall("jdplus/toolkit/base/api/information/InformationExtractors", "V", "reloadExtractors")
}

