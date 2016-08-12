#' @importFrom methods loadMethod
.onLoad <- function(libname, pkgname) {
  options(gs.fpthresh = 1e-4)
}

.onUnload <- function(libpath) {
  options(gs.fpthresh = NULL)
  library.dynam.unload("grainscape2", libpath)
}
