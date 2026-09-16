#' @include utils.R
NULL

#' @importFrom rjd3jars check_java_version
.onAttach <- function(libname, pkgname) {
    # Check Java version
    rjd3jars::check_java_version(silent = FALSE, startup = TRUE)
}

#' @importFrom rjd3jars check_java_version reload_dictionaries
#' @importFrom stats is.ts start
#' @importFrom RProtoBuf read readProtoFiles2
#' @importFrom rJava .jpackage .jcall .jnull is.jnull .jfield
.onLoad <- function(libname, pkgname) {
    # Loading dependencies
    if (!requireNamespace("rjd3jars", quietly = TRUE)) {
        stop("Loading {rjd3jars} failed", call. = FALSE)
    }
    if (!requireNamespace("rjd3toolkit", quietly = TRUE)) {
        stop("Loading {rjd3toolkit} failed", call. = FALSE)
    }

    # Loading Java class
    jar_dir <- file.path(libname, pkgname, "inst", "java")
    jars_inst <- list.files(
        jar_dir,
        pattern = "\\.jar$",
        full.names = TRUE,
        all.files = TRUE
    )
    result <- rJava::.jpackage(
        pkgname,
        lib.loc = libname,
        morePaths = jars_inst
    )
    if (!result) {
        stop("Loading Java packages failed")
    }

    # If java >= 21, then reload dictionnaries
    has_java <- rjd3jars::check_java_version(silent = TRUE)
    if (has_java) {
        rjd3jars::reload_dictionaries()
    }

    # Loading Proto class
    proto.dir <- system.file("proto", package = pkgname)
    RProtoBuf::readProtoFiles2(protoPath = proto.dir)

    # assign("providers", list(), rjd3toolkit::.jd3_env)
    # assign("tramoseats", list(), rjd3toolkit::.jd3_env)

    # Set options
    if (is.null(getOption("summary_info"))) {
        options(summary_info = TRUE)
    }
}

#' Set an option for tramoseats
#'
#' @param name Name of the option
#' @param obj Option
#'
#' @export
#'
#' @examples
#' tramoseats_option("test", "DUMMY")
tramoseats_option <- function(name, obj) {
    options <- rjd3toolkit::.jd3_env$tramoseats
    options[[name]] <- obj
    assign("tramoseats", options, rjd3toolkit::.jd3_env)
    invisible()
}

#' Set an option for tramoseats
#'
#' @param name Name of the option
#'
#' @returns The requested option or NULL if it doesn't exist
#' @export
#'
#' @examples
#' tramoseats_option("test", "DUMMY")
#' get_tramoseats_option("test")
get_tramoseats_option <- function(name) {
    options <- rjd3toolkit::.jd3_env$tramoseats
    return(options[[name]])
}
