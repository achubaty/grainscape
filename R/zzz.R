.onLoad <- function(libname, pkgname) {
}

.onUnload <- function(libpath) {
  library.dynam.unload("grainscape", libpath)
}
