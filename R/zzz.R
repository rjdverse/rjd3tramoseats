#' @include utils.R
NULL

.onAttach <- function(libname, pkgname) {
    current_java_version <- rjd3toolkit::get_java_version()
    if (current_java_version < rjd3toolkit::minimal_java_version) {
        packageStartupMessage(sprintf("Your java version is %s. %s or higher is needed.",
                                      current_java_version, rjd3toolkit::minimal_java_version))
    }
}

.onLoad <- function(libname, pkgname) {
    result <- rJava::.jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed", call. = FALSE)

    current_java_version <- rjd3toolkit::get_java_version()
    if (current_java_version >= rjd3toolkit::minimal_java_version) {
        rjd3toolkit::reload_dictionaries()
    }

    proto.dir <- system.file("proto", package = pkgname)
    RProtoBuf::readProtoFiles2(protoPath = proto.dir)

    if (is.null(getOption("summary_info"))) {
        options(summary_info = TRUE)
    }
}
