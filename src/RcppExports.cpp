// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cpp_double_equal_vectorised
SEXP cpp_double_equal_vectorised(SEXP x, SEXP y, SEXP tolerance);
RcppExport SEXP _timeplyr_cpp_double_equal_vectorised(SEXP xSEXP, SEXP ySEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    Rcpp::traits::input_parameter< SEXP >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_double_equal_vectorised(x, y, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// cpp_double_gt_vectorised
SEXP cpp_double_gt_vectorised(SEXP x, SEXP y, SEXP tolerance);
RcppExport SEXP _timeplyr_cpp_double_gt_vectorised(SEXP xSEXP, SEXP ySEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    Rcpp::traits::input_parameter< SEXP >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_double_gt_vectorised(x, y, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// cpp_double_gte_vectorised
SEXP cpp_double_gte_vectorised(SEXP x, SEXP y, SEXP tolerance);
RcppExport SEXP _timeplyr_cpp_double_gte_vectorised(SEXP xSEXP, SEXP ySEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    Rcpp::traits::input_parameter< SEXP >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_double_gte_vectorised(x, y, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// cpp_double_lt_vectorised
SEXP cpp_double_lt_vectorised(SEXP x, SEXP y, SEXP tolerance);
RcppExport SEXP _timeplyr_cpp_double_lt_vectorised(SEXP xSEXP, SEXP ySEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    Rcpp::traits::input_parameter< SEXP >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_double_lt_vectorised(x, y, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// cpp_double_lte_vectorised
SEXP cpp_double_lte_vectorised(SEXP x, SEXP y, SEXP tolerance);
RcppExport SEXP _timeplyr_cpp_double_lte_vectorised(SEXP xSEXP, SEXP ySEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    Rcpp::traits::input_parameter< SEXP >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_double_lte_vectorised(x, y, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// cpp_is_whole_num
SEXP cpp_is_whole_num(SEXP x, SEXP tol, bool na_rm);
RcppExport SEXP _timeplyr_cpp_is_whole_num(SEXP xSEXP, SEXP tolSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_is_whole_num(x, tol, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// cpp_df_group_indices
SEXP cpp_df_group_indices(SEXP rows, int size);
RcppExport SEXP _timeplyr_cpp_df_group_indices(SEXP rowsSEXP, SEXP sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type rows(rowsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_df_group_indices(rows, size));
    return rcpp_result_gen;
END_RCPP
}
// list_rm_null
List list_rm_null(List l);
RcppExport SEXP _timeplyr_list_rm_null(SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< List >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(list_rm_null(l));
    return rcpp_result_gen;
END_RCPP
}
// list_has_interval
bool list_has_interval(List l);
RcppExport SEXP _timeplyr_list_has_interval(SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< List >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(list_has_interval(l));
    return rcpp_result_gen;
END_RCPP
}
// list_item_is_interval
SEXP list_item_is_interval(List l);
RcppExport SEXP _timeplyr_list_item_is_interval(SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< List >::type l(lSEXP);
    rcpp_result_gen = Rcpp::wrap(list_item_is_interval(l));
    return rcpp_result_gen;
END_RCPP
}
// test_long_vector_support
bool test_long_vector_support();
RcppExport SEXP _timeplyr_test_long_vector_support() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(test_long_vector_support());
    return rcpp_result_gen;
END_RCPP
}
// cpp_num_na
SEXP cpp_num_na(SEXP x);
RcppExport SEXP _timeplyr_cpp_num_na(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_num_na(x));
    return rcpp_result_gen;
END_RCPP
}
// roll_time_threshold
IntegerVector roll_time_threshold(SEXP x, double threshold, bool switch_on_boundary);
RcppExport SEXP _timeplyr_roll_time_threshold(SEXP xSEXP, SEXP thresholdSEXP, SEXP switch_on_boundarySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< bool >::type switch_on_boundary(switch_on_boundarySEXP);
    rcpp_result_gen = Rcpp::wrap(roll_time_threshold(x, threshold, switch_on_boundary));
    return rcpp_result_gen;
END_RCPP
}
// before_sequence
IntegerVector before_sequence(IntegerVector size, double k);
RcppExport SEXP _timeplyr_before_sequence(SEXP sizeSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(before_sequence(size, k));
    return rcpp_result_gen;
END_RCPP
}
// after_sequence
Rcpp::IntegerVector after_sequence(Rcpp::IntegerVector size, double k);
RcppExport SEXP _timeplyr_after_sequence(SEXP sizeSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(after_sequence(size, k));
    return rcpp_result_gen;
END_RCPP
}
// cpp_dbl_sequence
SEXP cpp_dbl_sequence(SEXP size, SEXP from, SEXP by);
RcppExport SEXP _timeplyr_cpp_dbl_sequence(SEXP sizeSEXP, SEXP fromSEXP, SEXP bySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< SEXP >::type from(fromSEXP);
    Rcpp::traits::input_parameter< SEXP >::type by(bySEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_dbl_sequence(size, from, by));
    return rcpp_result_gen;
END_RCPP
}
// window_sequence
IntegerVector window_sequence(IntegerVector size, double k, bool partial, bool ascending);
RcppExport SEXP _timeplyr_window_sequence(SEXP sizeSEXP, SEXP kSEXP, SEXP partialSEXP, SEXP ascendingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    Rcpp::traits::input_parameter< bool >::type partial(partialSEXP);
    Rcpp::traits::input_parameter< bool >::type ascending(ascendingSEXP);
    rcpp_result_gen = Rcpp::wrap(window_sequence(size, k, partial, ascending));
    return rcpp_result_gen;
END_RCPP
}
// lag_sequence
IntegerVector lag_sequence(IntegerVector size, double k);
RcppExport SEXP _timeplyr_lag_sequence(SEXP sizeSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(lag_sequence(size, k));
    return rcpp_result_gen;
END_RCPP
}
// lead_sequence
IntegerVector lead_sequence(IntegerVector size, double k);
RcppExport SEXP _timeplyr_lead_sequence(SEXP sizeSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(lead_sequence(size, k));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_timeplyr_cpp_double_equal_vectorised", (DL_FUNC) &_timeplyr_cpp_double_equal_vectorised, 3},
    {"_timeplyr_cpp_double_gt_vectorised", (DL_FUNC) &_timeplyr_cpp_double_gt_vectorised, 3},
    {"_timeplyr_cpp_double_gte_vectorised", (DL_FUNC) &_timeplyr_cpp_double_gte_vectorised, 3},
    {"_timeplyr_cpp_double_lt_vectorised", (DL_FUNC) &_timeplyr_cpp_double_lt_vectorised, 3},
    {"_timeplyr_cpp_double_lte_vectorised", (DL_FUNC) &_timeplyr_cpp_double_lte_vectorised, 3},
    {"_timeplyr_cpp_is_whole_num", (DL_FUNC) &_timeplyr_cpp_is_whole_num, 3},
    {"_timeplyr_cpp_df_group_indices", (DL_FUNC) &_timeplyr_cpp_df_group_indices, 2},
    {"_timeplyr_list_rm_null", (DL_FUNC) &_timeplyr_list_rm_null, 1},
    {"_timeplyr_list_has_interval", (DL_FUNC) &_timeplyr_list_has_interval, 1},
    {"_timeplyr_list_item_is_interval", (DL_FUNC) &_timeplyr_list_item_is_interval, 1},
    {"_timeplyr_test_long_vector_support", (DL_FUNC) &_timeplyr_test_long_vector_support, 0},
    {"_timeplyr_cpp_num_na", (DL_FUNC) &_timeplyr_cpp_num_na, 1},
    {"_timeplyr_roll_time_threshold", (DL_FUNC) &_timeplyr_roll_time_threshold, 3},
    {"_timeplyr_before_sequence", (DL_FUNC) &_timeplyr_before_sequence, 2},
    {"_timeplyr_after_sequence", (DL_FUNC) &_timeplyr_after_sequence, 2},
    {"_timeplyr_cpp_dbl_sequence", (DL_FUNC) &_timeplyr_cpp_dbl_sequence, 3},
    {"_timeplyr_window_sequence", (DL_FUNC) &_timeplyr_window_sequence, 4},
    {"_timeplyr_lag_sequence", (DL_FUNC) &_timeplyr_lag_sequence, 2},
    {"_timeplyr_lead_sequence", (DL_FUNC) &_timeplyr_lead_sequence, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_timeplyr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
