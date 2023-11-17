// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// gcd.cpp
double cpp_gcd2(double x, double y, double tol, bool na_rm);
extern "C" SEXP _timeplyr_cpp_gcd2(SEXP x, SEXP y, SEXP tol, SEXP na_rm) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_gcd2(cpp11::as_cpp<cpp11::decay_t<double>>(x), cpp11::as_cpp<cpp11::decay_t<double>>(y), cpp11::as_cpp<cpp11::decay_t<double>>(tol), cpp11::as_cpp<cpp11::decay_t<bool>>(na_rm)));
  END_CPP11
}
// gcd.cpp
double cpp_lcm2(double x, double y, double tol, bool na_rm);
extern "C" SEXP _timeplyr_cpp_lcm2(SEXP x, SEXP y, SEXP tol, SEXP na_rm) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_lcm2(cpp11::as_cpp<cpp11::decay_t<double>>(x), cpp11::as_cpp<cpp11::decay_t<double>>(y), cpp11::as_cpp<cpp11::decay_t<double>>(tol), cpp11::as_cpp<cpp11::decay_t<bool>>(na_rm)));
  END_CPP11
}
// gcd.cpp
SEXP cpp_gcd(SEXP x, double tol, bool na_rm, int start, bool break_early, bool round);
extern "C" SEXP _timeplyr_cpp_gcd(SEXP x, SEXP tol, SEXP na_rm, SEXP start, SEXP break_early, SEXP round) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_gcd(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<double>>(tol), cpp11::as_cpp<cpp11::decay_t<bool>>(na_rm), cpp11::as_cpp<cpp11::decay_t<int>>(start), cpp11::as_cpp<cpp11::decay_t<bool>>(break_early), cpp11::as_cpp<cpp11::decay_t<bool>>(round)));
  END_CPP11
}
// gcd.cpp
SEXP cpp_lcm(SEXP x, double tol, bool na_rm, bool round);
extern "C" SEXP _timeplyr_cpp_lcm(SEXP x, SEXP tol, SEXP na_rm, SEXP round) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_lcm(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<double>>(tol), cpp11::as_cpp<cpp11::decay_t<bool>>(na_rm), cpp11::as_cpp<cpp11::decay_t<bool>>(round)));
  END_CPP11
}
// is_whole_num.cpp
SEXP cpp_is_whole_num(SEXP x, double tol, bool na_rm);
extern "C" SEXP _timeplyr_cpp_is_whole_num(SEXP x, SEXP tol, SEXP na_rm) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_is_whole_num(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<double>>(tol), cpp11::as_cpp<cpp11::decay_t<bool>>(na_rm)));
  END_CPP11
}
// lag.cpp
SEXP cpp_roll_lag(SEXP x, int k, SEXP fill);
extern "C" SEXP _timeplyr_cpp_roll_lag(SEXP x, SEXP k, SEXP fill) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_roll_lag(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<int>>(k), cpp11::as_cpp<cpp11::decay_t<SEXP>>(fill)));
  END_CPP11
}
// lag.cpp
SEXP cpp_roll_lead(SEXP x, int k, SEXP fill);
extern "C" SEXP _timeplyr_cpp_roll_lead(SEXP x, SEXP k, SEXP fill) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_roll_lead(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<int>>(k), cpp11::as_cpp<cpp11::decay_t<SEXP>>(fill)));
  END_CPP11
}
// lag.cpp
SEXP cpp_roll_lag_grouped(SEXP x, int k, SEXP o, SEXP sizes, SEXP fill);
extern "C" SEXP _timeplyr_cpp_roll_lag_grouped(SEXP x, SEXP k, SEXP o, SEXP sizes, SEXP fill) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_roll_lag_grouped(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<int>>(k), cpp11::as_cpp<cpp11::decay_t<SEXP>>(o), cpp11::as_cpp<cpp11::decay_t<SEXP>>(sizes), cpp11::as_cpp<cpp11::decay_t<SEXP>>(fill)));
  END_CPP11
}
// lag.cpp
SEXP cpp_roll_lead_grouped(SEXP x, int k, SEXP o, SEXP sizes, SEXP fill);
extern "C" SEXP _timeplyr_cpp_roll_lead_grouped(SEXP x, SEXP k, SEXP o, SEXP sizes, SEXP fill) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_roll_lead_grouped(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<int>>(k), cpp11::as_cpp<cpp11::decay_t<SEXP>>(o), cpp11::as_cpp<cpp11::decay_t<SEXP>>(sizes), cpp11::as_cpp<cpp11::decay_t<SEXP>>(fill)));
  END_CPP11
}
// lag.cpp
SEXP cpp_roll_diff(SEXP x, int k, SEXP fill);
extern "C" SEXP _timeplyr_cpp_roll_diff(SEXP x, SEXP k, SEXP fill) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_roll_diff(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<int>>(k), cpp11::as_cpp<cpp11::decay_t<SEXP>>(fill)));
  END_CPP11
}
// lag.cpp
SEXP cpp_roll_diff_grouped(SEXP x, int k, SEXP o, SEXP sizes, SEXP fill);
extern "C" SEXP _timeplyr_cpp_roll_diff_grouped(SEXP x, SEXP k, SEXP o, SEXP sizes, SEXP fill) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_roll_diff_grouped(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<int>>(k), cpp11::as_cpp<cpp11::decay_t<SEXP>>(o), cpp11::as_cpp<cpp11::decay_t<SEXP>>(sizes), cpp11::as_cpp<cpp11::decay_t<SEXP>>(fill)));
  END_CPP11
}
// na_fill.cpp
SEXP cpp_roll_na_fill(SEXP x, double fill_limit);
extern "C" SEXP _timeplyr_cpp_roll_na_fill(SEXP x, SEXP fill_limit) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_roll_na_fill(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<double>>(fill_limit)));
  END_CPP11
}
// na_fill.cpp
SEXP cpp_roll_na_fill_grouped(SEXP x, SEXP o, SEXP sizes, double fill_limit);
extern "C" SEXP _timeplyr_cpp_roll_na_fill_grouped(SEXP x, SEXP o, SEXP sizes, SEXP fill_limit) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_roll_na_fill_grouped(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<SEXP>>(o), cpp11::as_cpp<cpp11::decay_t<SEXP>>(sizes), cpp11::as_cpp<cpp11::decay_t<double>>(fill_limit)));
  END_CPP11
}
// num_na.cpp
SEXP cpp_num_na(SEXP x);
extern "C" SEXP _timeplyr_cpp_num_na(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_num_na(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// row_id.cpp
SEXP cpp_row_id(SEXP order, SEXP group_sizes, bool ascending);
extern "C" SEXP _timeplyr_cpp_row_id(SEXP order, SEXP group_sizes, SEXP ascending) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_row_id(cpp11::as_cpp<cpp11::decay_t<SEXP>>(order), cpp11::as_cpp<cpp11::decay_t<SEXP>>(group_sizes), cpp11::as_cpp<cpp11::decay_t<bool>>(ascending)));
  END_CPP11
}
// sequence.cpp
SEXP before_sequence(SEXP size, double k);
extern "C" SEXP _timeplyr_before_sequence(SEXP size, SEXP k) {
  BEGIN_CPP11
    return cpp11::as_sexp(before_sequence(cpp11::as_cpp<cpp11::decay_t<SEXP>>(size), cpp11::as_cpp<cpp11::decay_t<double>>(k)));
  END_CPP11
}
// sequence.cpp
SEXP after_sequence(SEXP size, double k);
extern "C" SEXP _timeplyr_after_sequence(SEXP size, SEXP k) {
  BEGIN_CPP11
    return cpp11::as_sexp(after_sequence(cpp11::as_cpp<cpp11::decay_t<SEXP>>(size), cpp11::as_cpp<cpp11::decay_t<double>>(k)));
  END_CPP11
}
// sequence.cpp
SEXP cpp_int_sequence(SEXP size, SEXP from, SEXP by);
extern "C" SEXP _timeplyr_cpp_int_sequence(SEXP size, SEXP from, SEXP by) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_int_sequence(cpp11::as_cpp<cpp11::decay_t<SEXP>>(size), cpp11::as_cpp<cpp11::decay_t<SEXP>>(from), cpp11::as_cpp<cpp11::decay_t<SEXP>>(by)));
  END_CPP11
}
// sequence.cpp
SEXP cpp_dbl_sequence(SEXP size, SEXP from, SEXP by);
extern "C" SEXP _timeplyr_cpp_dbl_sequence(SEXP size, SEXP from, SEXP by) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_dbl_sequence(cpp11::as_cpp<cpp11::decay_t<SEXP>>(size), cpp11::as_cpp<cpp11::decay_t<SEXP>>(from), cpp11::as_cpp<cpp11::decay_t<SEXP>>(by)));
  END_CPP11
}
// sequence.cpp
SEXP cpp_window_sequence(SEXP size, double k, bool partial, bool ascending);
extern "C" SEXP _timeplyr_cpp_window_sequence(SEXP size, SEXP k, SEXP partial, SEXP ascending) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_window_sequence(cpp11::as_cpp<cpp11::decay_t<SEXP>>(size), cpp11::as_cpp<cpp11::decay_t<double>>(k), cpp11::as_cpp<cpp11::decay_t<bool>>(partial), cpp11::as_cpp<cpp11::decay_t<bool>>(ascending)));
  END_CPP11
}
// sequence.cpp
SEXP cpp_lag_sequence(SEXP size, double k, bool partial);
extern "C" SEXP _timeplyr_cpp_lag_sequence(SEXP size, SEXP k, SEXP partial) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_lag_sequence(cpp11::as_cpp<cpp11::decay_t<SEXP>>(size), cpp11::as_cpp<cpp11::decay_t<double>>(k), cpp11::as_cpp<cpp11::decay_t<bool>>(partial)));
  END_CPP11
}
// sequence.cpp
SEXP cpp_lead_sequence(SEXP size, double k, bool partial);
extern "C" SEXP _timeplyr_cpp_lead_sequence(SEXP size, SEXP k, SEXP partial) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_lead_sequence(cpp11::as_cpp<cpp11::decay_t<SEXP>>(size), cpp11::as_cpp<cpp11::decay_t<double>>(k), cpp11::as_cpp<cpp11::decay_t<bool>>(partial)));
  END_CPP11
}
// utils.cpp
SEXP cpp_list_which_not_null(SEXP l);
extern "C" SEXP _timeplyr_cpp_list_which_not_null(SEXP l) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_list_which_not_null(cpp11::as_cpp<cpp11::decay_t<SEXP>>(l)));
  END_CPP11
}
// utils.cpp
bool list_has_interval(SEXP l);
extern "C" SEXP _timeplyr_list_has_interval(SEXP l) {
  BEGIN_CPP11
    return cpp11::as_sexp(list_has_interval(cpp11::as_cpp<cpp11::decay_t<SEXP>>(l)));
  END_CPP11
}
// utils.cpp
SEXP list_item_is_interval(SEXP l);
extern "C" SEXP _timeplyr_list_item_is_interval(SEXP l) {
  BEGIN_CPP11
    return cpp11::as_sexp(list_item_is_interval(cpp11::as_cpp<cpp11::decay_t<SEXP>>(l)));
  END_CPP11
}
// utils.cpp
SEXP cpp_sorted_group_starts(SEXP group_sizes);
extern "C" SEXP _timeplyr_cpp_sorted_group_starts(SEXP group_sizes) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_sorted_group_starts(cpp11::as_cpp<cpp11::decay_t<SEXP>>(group_sizes)));
  END_CPP11
}
// utils.cpp
SEXP roll_time_threshold(SEXP x, double threshold, bool switch_on_boundary);
extern "C" SEXP _timeplyr_roll_time_threshold(SEXP x, SEXP threshold, SEXP switch_on_boundary) {
  BEGIN_CPP11
    return cpp11::as_sexp(roll_time_threshold(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<double>>(threshold), cpp11::as_cpp<cpp11::decay_t<bool>>(switch_on_boundary)));
  END_CPP11
}
// utils.cpp
SEXP cpp_df_group_indices(SEXP rows, int size);
extern "C" SEXP _timeplyr_cpp_df_group_indices(SEXP rows, SEXP size) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_df_group_indices(cpp11::as_cpp<cpp11::decay_t<SEXP>>(rows), cpp11::as_cpp<cpp11::decay_t<int>>(size)));
  END_CPP11
}
// which.cpp
SEXP cpp_which_(SEXP x, bool invert);
extern "C" SEXP _timeplyr_cpp_which_(SEXP x, SEXP invert) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_which_(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<bool>>(invert)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_timeplyr_after_sequence",           (DL_FUNC) &_timeplyr_after_sequence,           2},
    {"_timeplyr_before_sequence",          (DL_FUNC) &_timeplyr_before_sequence,          2},
    {"_timeplyr_cpp_dbl_sequence",         (DL_FUNC) &_timeplyr_cpp_dbl_sequence,         3},
    {"_timeplyr_cpp_df_group_indices",     (DL_FUNC) &_timeplyr_cpp_df_group_indices,     2},
    {"_timeplyr_cpp_gcd",                  (DL_FUNC) &_timeplyr_cpp_gcd,                  6},
    {"_timeplyr_cpp_gcd2",                 (DL_FUNC) &_timeplyr_cpp_gcd2,                 4},
    {"_timeplyr_cpp_int_sequence",         (DL_FUNC) &_timeplyr_cpp_int_sequence,         3},
    {"_timeplyr_cpp_is_whole_num",         (DL_FUNC) &_timeplyr_cpp_is_whole_num,         3},
    {"_timeplyr_cpp_lag_sequence",         (DL_FUNC) &_timeplyr_cpp_lag_sequence,         3},
    {"_timeplyr_cpp_lcm",                  (DL_FUNC) &_timeplyr_cpp_lcm,                  4},
    {"_timeplyr_cpp_lcm2",                 (DL_FUNC) &_timeplyr_cpp_lcm2,                 4},
    {"_timeplyr_cpp_lead_sequence",        (DL_FUNC) &_timeplyr_cpp_lead_sequence,        3},
    {"_timeplyr_cpp_list_which_not_null",  (DL_FUNC) &_timeplyr_cpp_list_which_not_null,  1},
    {"_timeplyr_cpp_num_na",               (DL_FUNC) &_timeplyr_cpp_num_na,               1},
    {"_timeplyr_cpp_roll_diff",            (DL_FUNC) &_timeplyr_cpp_roll_diff,            3},
    {"_timeplyr_cpp_roll_diff_grouped",    (DL_FUNC) &_timeplyr_cpp_roll_diff_grouped,    5},
    {"_timeplyr_cpp_roll_lag",             (DL_FUNC) &_timeplyr_cpp_roll_lag,             3},
    {"_timeplyr_cpp_roll_lag_grouped",     (DL_FUNC) &_timeplyr_cpp_roll_lag_grouped,     5},
    {"_timeplyr_cpp_roll_lead",            (DL_FUNC) &_timeplyr_cpp_roll_lead,            3},
    {"_timeplyr_cpp_roll_lead_grouped",    (DL_FUNC) &_timeplyr_cpp_roll_lead_grouped,    5},
    {"_timeplyr_cpp_roll_na_fill",         (DL_FUNC) &_timeplyr_cpp_roll_na_fill,         2},
    {"_timeplyr_cpp_roll_na_fill_grouped", (DL_FUNC) &_timeplyr_cpp_roll_na_fill_grouped, 4},
    {"_timeplyr_cpp_row_id",               (DL_FUNC) &_timeplyr_cpp_row_id,               3},
    {"_timeplyr_cpp_sorted_group_starts",  (DL_FUNC) &_timeplyr_cpp_sorted_group_starts,  1},
    {"_timeplyr_cpp_which_",               (DL_FUNC) &_timeplyr_cpp_which_,               2},
    {"_timeplyr_cpp_window_sequence",      (DL_FUNC) &_timeplyr_cpp_window_sequence,      4},
    {"_timeplyr_list_has_interval",        (DL_FUNC) &_timeplyr_list_has_interval,        1},
    {"_timeplyr_list_item_is_interval",    (DL_FUNC) &_timeplyr_list_item_is_interval,    1},
    {"_timeplyr_roll_time_threshold",      (DL_FUNC) &_timeplyr_roll_time_threshold,      3},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_timeplyr(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
