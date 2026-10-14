#' @title Set an option for tramoseats
#'
#' @param name Name of the option
#' @param obj Option
#'
#' @returns Invisibly `NULL`
#'
#' @export
#'
#' @examples
#' tramoseats_option("test", "DUMMY")
tramoseats_option <- function(name, obj) {
    options <- rjd3toolkit::.jd3_env$tramoseats
    options[[name]] <- obj
    assign("tramoseats", options, rjd3toolkit::.jd3_env)
    return(invisible(NULL))
}

#' @title Set an option for tramoseats
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
