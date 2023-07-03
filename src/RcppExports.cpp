// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// roll_time_threshold
IntegerVector roll_time_threshold(NumericVector x, double threshold, bool switch_on_boundary);
RcppExport SEXP _timeplyr_roll_time_threshold(SEXP xSEXP, SEXP thresholdSEXP, SEXP switch_on_boundarySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< bool >::type switch_on_boundary(switch_on_boundarySEXP);
    rcpp_result_gen = Rcpp::wrap(roll_time_threshold(x, threshold, switch_on_boundary));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_timeplyr_roll_time_threshold", (DL_FUNC) &_timeplyr_roll_time_threshold, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_timeplyr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
