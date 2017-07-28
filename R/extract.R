#' Extract or Replace Parts of an Object
#'
#' @param x        A \code{simList} object from which to extract element(s) or
#'                 in which to replace element(s).
#' @param name     A literal character string or a \code{\link{name}}.
#' @param value    Any R object.
#'
#' @export
#' @include classes.R
#' @name $
#' @aliases $,goc-method
#' @rdname extract
setMethod(
  "$",
  signature(x = "goc"),
  definition = function(x, name) {
    return(slot(x, name))
})

#' @export
#' @name $<-
#' @aliases $<-,goc-method
#' @rdname extract
setReplaceMethod(
  "$",
  signature(x = "goc", value = "ANY"),
  definition = function(x, name, value) {
    slot(x, name) <- value
    return(x)
})

#' @export
#' @name $
#' @aliases $,mpg-method
#' @rdname extract
setMethod(
  "$",
  signature(x = "mpg"),
  definition = function(x, name) {
    return(slot(x, name))
})

#' @export
#' @name $<-
#' @aliases $<-,mpg-method
#' @rdname extract
setReplaceMethod(
  "$",
  signature(x = "mpg", value = "ANY"),
  definition = function(x, name, value) {
    slot(x, name) <- value
    return(x)
})
