# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Habitat connectivity engine (C++)
#'
#' Internal function, not intended to be called directly.
#'
#' @param cost     Numeric vector of habitat cost (resistance) values
#'                 extracted from a cost raster map.
#'
#' @param patches  Numeric vector that have binary values (\code{0} and \code{1}) where
#'                 ones corresponds to patch cells and zeroes to non-habitat (i.e., matrix) cells.
#'
#' @param nrow     Number of rows in both the cost and patch raster maps.
#'
#' @param ncol     Number of columns in the cost and patch raster maps.
#'
#' @author Sam Doctolero
#' @keywords internal
#' @rdname habConnRcpp
.habConnRcpp <- function(cost, patches, ncol, nrow) {
    .Call(`_grainscape_habConnRcpp`, cost, patches, ncol, nrow)
}

