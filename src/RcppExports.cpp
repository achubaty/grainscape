// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// habConnRcpp
List habConnRcpp(NumericVector cost, int nrow, int ncol, double hab, double no_data, double increment);
RcppExport SEXP grainscape2_habConnRcpp(SEXP costSEXP, SEXP nrowSEXP, SEXP ncolSEXP, SEXP habSEXP, SEXP no_dataSEXP, SEXP incrementSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type cost(costSEXP);
    Rcpp::traits::input_parameter< int >::type nrow(nrowSEXP);
    Rcpp::traits::input_parameter< int >::type ncol(ncolSEXP);
    Rcpp::traits::input_parameter< double >::type hab(habSEXP);
    Rcpp::traits::input_parameter< double >::type no_data(no_dataSEXP);
    Rcpp::traits::input_parameter< double >::type increment(incrementSEXP);
    __result = Rcpp::wrap(habConnRcpp(cost, nrow, ncol, hab, no_data, increment));
    return __result;
END_RCPP
}
