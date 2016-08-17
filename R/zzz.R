#' @importFrom methods loadMethod
.onLoad <- function(libname, pkgname) {
}

.onUnload <- function(libpath) {
  library.dynam.unload("grainscape2", libpath)
}
