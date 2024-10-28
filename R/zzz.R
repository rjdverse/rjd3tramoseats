#' @include utils.R
NULL

.onLoad <- function(libname, pkgname) {
    if (!requireNamespace("rjd3toolkit", quietly = TRUE)) stop("Loading rjd3 libraries failed")

    result <- rJava::.jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed")

    proto.dir <- system.file("proto", package = pkgname)
    RProtoBuf::readProtoFiles2(protoPath = proto.dir)

    # reload extractors
    rjd3toolkit::reload_dictionaries()

    if (is.null(getOption("summary_info"))) {
        options(summary_info = TRUE)
    }
}
