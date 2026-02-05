#' @include utils.R
NULL

#' @title Java version.
#'
#' @returns \code{current_java_version} is the current Java version and \code{minimal_java_version} is the minimum accepted Java version.
#'
#' @importFrom rjd3toolkit current_java_version minimal_java_version
#' @export
#' @name java_version
#'
#' @examples
#' print(minimal_java_version)
#' print(current_java_version)
#' @export
current_java_version <- rjd3toolkit::current_java_version

#' @rdname java_version
#' @export
minimal_java_version <- rjd3toolkit::minimal_java_version

.onAttach <- function(libname, pkgname) {
    if (current_java_version < minimal_java_version) {
        packageStartupMessage(sprintf("Your java version is %s. %s or higher is needed.",
                                      current_java_version, minimal_java_version))
    }
}

.onLoad <- function(libname, pkgname) {
    result <- rJava::.jpackage(pkgname, lib.loc = libname)
    if (!result) stop("Loading java packages failed", call. = FALSE)

    if (current_java_version >= minimal_java_version) {
        rjd3toolkit::reload_dictionaries()
    }

    proto.dir <- system.file("proto", package = pkgname)
    RProtoBuf::readProtoFiles2(protoPath = proto.dir)

    if (is.null(getOption("summary_info"))) {
        options(summary_info = TRUE)
    }
}
