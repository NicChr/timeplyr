// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// diff.cpp
SEXP cpp_diff(SEXP x, SEXP lag, SEXP order, SEXP run_lengths, SEXP fill, int differences);
extern "C" SEXP _timeplyr_cpp_diff(SEXP x, SEXP lag, SEXP order, SEXP run_lengths, SEXP fill, SEXP differences) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_diff(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<SEXP>>(lag), cpp11::as_cpp<cpp11::decay_t<SEXP>>(order), cpp11::as_cpp<cpp11::decay_t<SEXP>>(run_lengths), cpp11::as_cpp<cpp11::decay_t<SEXP>>(fill), cpp11::as_cpp<cpp11::decay_t<int>>(differences)));
  END_CPP11
}
// is_whole_num.cpp
SEXP cpp_is_whole_num(SEXP x, double tol, bool na_rm);
extern "C" SEXP _timeplyr_cpp_is_whole_num(SEXP x, SEXP tol, SEXP na_rm) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_is_whole_num(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<double>>(tol), cpp11::as_cpp<cpp11::decay_t<bool>>(na_rm)));
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
// roll.cpp
SEXP cpp_roll_count_na(SEXP x, double window, bool invert, bool partial);
extern "C" SEXP _timeplyr_cpp_roll_count_na(SEXP x, SEXP window, SEXP invert, SEXP partial) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_roll_count_na(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<double>>(window), cpp11::as_cpp<cpp11::decay_t<bool>>(invert), cpp11::as_cpp<cpp11::decay_t<bool>>(partial)));
  END_CPP11
}
// roll.cpp
SEXP cpp_roll_growth_rate(SEXP x, SEXP lag, bool log);
extern "C" SEXP _timeplyr_cpp_roll_growth_rate(SEXP x, SEXP lag, SEXP log) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_roll_growth_rate(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<SEXP>>(lag), cpp11::as_cpp<cpp11::decay_t<bool>>(log)));
  END_CPP11
}
// utils.cpp
R_xlen_t cpp_vector_size(SEXP x);
extern "C" SEXP _timeplyr_cpp_vector_size(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_vector_size(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
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
SEXP cpp_consecutive_na_id(SEXP x, bool left_to_right);
extern "C" SEXP _timeplyr_cpp_consecutive_na_id(SEXP x, SEXP left_to_right) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_consecutive_na_id(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<bool>>(left_to_right)));
  END_CPP11
}
// utils.cpp
SEXP cpp_which_first_gap(SEXP x, int increment, bool left_to_right);
extern "C" SEXP _timeplyr_cpp_which_first_gap(SEXP x, SEXP increment, SEXP left_to_right) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_which_first_gap(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<int>>(increment), cpp11::as_cpp<cpp11::decay_t<bool>>(left_to_right)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_timeplyr_cpp_consecutive_na_id",    (DL_FUNC) &_timeplyr_cpp_consecutive_na_id,    2},
    {"_timeplyr_cpp_diff",                 (DL_FUNC) &_timeplyr_cpp_diff,                 6},
    {"_timeplyr_cpp_is_whole_num",         (DL_FUNC) &_timeplyr_cpp_is_whole_num,         3},
    {"_timeplyr_cpp_roll_count_na",        (DL_FUNC) &_timeplyr_cpp_roll_count_na,        4},
    {"_timeplyr_cpp_roll_growth_rate",     (DL_FUNC) &_timeplyr_cpp_roll_growth_rate,     3},
    {"_timeplyr_cpp_roll_na_fill",         (DL_FUNC) &_timeplyr_cpp_roll_na_fill,         2},
    {"_timeplyr_cpp_roll_na_fill_grouped", (DL_FUNC) &_timeplyr_cpp_roll_na_fill_grouped, 4},
    {"_timeplyr_cpp_vector_size",          (DL_FUNC) &_timeplyr_cpp_vector_size,          1},
    {"_timeplyr_cpp_which_first_gap",      (DL_FUNC) &_timeplyr_cpp_which_first_gap,      3},
    {"_timeplyr_roll_time_threshold",      (DL_FUNC) &_timeplyr_roll_time_threshold,      3},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_timeplyr(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
