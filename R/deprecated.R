#' @title Deprecated functions
#'
#' @param x useless parameter
#' @inheritParams tramoseats
#' @inheritParams tramoseats_spec
#'
#' @returns All these functions are deprecated and return the same value as the
#' function that replaces them:
#'
#' - `spec_tramoseats()` returns the same value as `tramoseats_spec()`
#' - `spec_tramo()` returns the same value as `tramo_spec()`
#' - `fast_tramoseats()` returns the same value as `tramoseats_fast()`
#' - `fast_regarima()` returns the same value as `regarima_fast()`
#' - `.jtramoseats()` returns the same value as `jtramoseats()`
#' - `userdefined_variables_tramoseats()` returns the same value as `tramoseats_dictionary()`
#'
#' @name deprecated-rjd3tramoseats
#'
#' @export
fast_tramoseats <- function(
    ts,
    spec = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"),
    context = NULL,
    userdefined = NULL
) {
    .Deprecated("tramoseats_fast")
    tramoseats_fast(ts, spec, context, userdefined)
}

#' @name deprecated-rjd3tramoseats
#' @export
fast_tramo <- function(
    ts,
    spec = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5"),
    context = NULL,
    userdefined = NULL
) {
    .Deprecated("tramo_fast")
    tramo_fast(ts, spec, context, userdefined)
}

#' @name deprecated-rjd3tramoseats
#' @export
spec_tramoseats <- function(
    name = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5")
) {
    .Deprecated("tramoseats_spec")
    tramoseats_spec(name)
}

#' @name deprecated-rjd3tramoseats
#' @export
spec_tramo <- function(
    name = c("trfull", "tr0", "tr1", "tr2", "tr3", "tr4", "tr5")
) {
    .Deprecated("tramo_spec")
    tramo_spec(name)
}

#' @name deprecated-rjd3tramoseats
#' @export
userdefined_variables_tramoseats <- function(x = c("TRAMO-SEATS", "TRAMO")) {
    .Deprecated("tramoseats_dictionary")
    tramoseats_dictionary()
}

#' @export
#' @rdname deprecated-rjd3tramoseats
.jtramoseats <- function(
    ts,
    spec = c("rsafull", "rsa0", "rsa1", "rsa2", "rsa3", "rsa4", "rsa5"),
    context = NULL,
    userdefined = NULL
) {
    .Deprecated("jtramoseats")
    jtramoseats(ts, spec, context, userdefined)
}
