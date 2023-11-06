#include <Rcpp.h>
#include <Rinternals.h>
using namespace Rcpp;

#define R_NO_REMAP
#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))

// [[Rcpp::export]]
bool test_long_vector_support() {
#ifdef RCPP_HAS_LONG_LONG_TYPES
  return true;
#else
  return false;
#endif
}

// [[Rcpp::export(rng = false)]]
SEXP cpp_num_na(SEXP x){
  R_xlen_t n = Rf_xlength(x);
  R_xlen_t count = 0;
  // This nicely handles NULL and avoids loop too
  if (n == 0){
    SEXP out = PROTECT(Rf_allocVector(INTSXP, 1));
    int *p_out = INTEGER(out);
    int n_na = 0;
    p_out[0] = n_na;
    UNPROTECT(1);
    return out;
  }
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    for (R_xlen_t i = 0; i < n; i++){
      count += (p_x[i] == NA_INTEGER);
    }
    break;
  }
  case REALSXP: {
    double *p_x = REAL(x);
    for (R_xlen_t i = 0; i < n; i++){
      // Because NaN == NaN is false
      count += !(p_x[i] == p_x[i]);
    }
    break;
  }
  case STRSXP: {
    SEXP *p_x = STRING_PTR(x);
    for (R_xlen_t i = 0; i < n; i++){
      count += (p_x[i] == NA_STRING);
    }
    break;
  }
  case RAWSXP: {
    break;
  }
  case CPLXSXP: {
    Rcpp::ComplexVector xv = Rcpp::as<Rcpp::ComplexVector>(x);
    for (R_xlen_t i = 0; i < n; i++){
      count += Rcpp::ComplexVector::is_na(xv[i]);
    }
    break;
  }
  default: {
    Rf_error("num_na cannot handle the supplied SEXP");
    break;
  }
  }
  if (count <= std::numeric_limits<int>::max()){
    SEXP out = PROTECT(Rf_allocVector(INTSXP, 1));
    int *p_out = INTEGER(out);
    p_out[0] = int(count);
    UNPROTECT(1);
    return out;
  } else {
    SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));
    double *p_out = REAL(out);
    p_out[0] = double(count);
    UNPROTECT(1);
    return out;
  }
}

// [[Rcpp::export(rng = false)]]
List list_rm_null(List l) {
  int n = l.size();
  LogicalVector keep(n);
  for (int i = 0; i < n; ++i) {
    keep[i] = !Rf_isNull(l[i]);
  }
  return l[keep];
}

bool is_interval(SEXP x){
  return (Rf_isS4(x) && Rf_inherits(x, "Interval"));
}

// [[Rcpp::export(rng = false)]]
bool list_has_interval( SEXP l ) {
  SEXP L = PROTECT(Rf_coerceVector(l, VECSXP));
  const SEXP *p_l = VECTOR_PTR_RO(L);
  bool out = false;
  int n = Rf_length(l);
  for (int i = 0; i < n; ++i) {
    if (is_interval(p_l[i])){
      out = true;
      // n = i;
      break;
    }
  }
  UNPROTECT(1);
  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP list_item_is_interval( List l ) {
  int n = l.size();
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *p_out = LOGICAL(out);
  for (int i = 0; i < n; ++i) {
    p_out[i] = is_interval(l[i]);
  }
  UNPROTECT(1);
  return out;
}

// Take a vector of group sizes (sorted by group)
// And this will return a vector of the start indices of each group (in sorted order)

// [[Rcpp::export(rng = false)]]
SEXP cpp_sorted_group_starts(SEXP group_sizes){
  int *p_gsizes = INTEGER(group_sizes);
  int n = Rf_length(group_sizes);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  int init = 1;
  p_out[0] = 1;
  for (int i = 0; i < (n - 1); i++){
    init += p_gsizes[i];
    p_out[i + 1] = init;
  }
  UNPROTECT(1);
  return out;
}

// Internal function
// x must be a numeric or integer
// threshold must be a numeric or integer of length 1
// This basically takes as input a cumulatively increasing time vector,
// and once a threshold is reached
// This is flagged a 1, the threshold is reset at that time value, and
// a new flag will occur once this new threshold is reached, and so on.
// Again, x must be a cumulatively increasing vector.

// [[Rcpp::export(rng = false)]]
SEXP roll_time_threshold(SEXP x, double threshold = 1, bool switch_on_boundary = true) {
  int n = Rf_length(x);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  double init_threshold = 0;
  init_threshold = init_threshold + threshold;
  switch( TYPEOF(x) ) {
  case REALSXP: {
    double *p_x = REAL(x);
    double tol = sqrt(std::numeric_limits<double>::epsilon());
    if (switch_on_boundary){
      tol = -tol;
    }
    for (int i = 0; i < n; ++i) {
      if (!(p_x[i] == p_x[i])){
        p_out[i] = NA_INTEGER;
      } else if ((p_x[i] - threshold) > tol) {
        p_out[i] = 1;
        threshold = init_threshold + p_x[i];
      } else {
        p_out[i] = 0;
      }
    }
    break;
  }
  case INTSXP: {
    int *p_x = INTEGER(x);
    if (switch_on_boundary == true){
      for (int i = 0; i < n; ++i) {
        if (p_x[i] == NA_INTEGER){
          p_out[i] = NA_INTEGER;
        } else if (p_x[i] >= threshold) {
          p_out[i] = 1;
          threshold = init_threshold + p_x[i];
        } else {
          p_out[i] = 0;
        }
      }
    } else {
      for (int i = 0; i < n; ++i) {
        if (p_x[i] == NA_INTEGER){
          p_out[i] = NA_INTEGER;
        } else if (p_x[i] > threshold) {
          p_out[i] = 1;
          threshold = init_threshold + p_x[i];
        } else {
          p_out[i] = 0;
        }
      }
    }
    break;
  }
  default: {
    UNPROTECT(1);
    Rf_error("roll_time_threshold only supports integer and numeric vectors");
  }
  }
  UNPROTECT(1);
  return out;
}

// Taken from dplyr::group_indices,
// All credits go to dplyr
// [[Rcpp::export(rng = false)]]
SEXP cpp_df_group_indices(SEXP rows, int size) {
  SEXP indices = PROTECT(Rf_allocVector(INTSXP, size));
  int *p_indices = INTEGER(indices);
  R_xlen_t ng = XLENGTH(rows);
  const SEXP* p_rows = VECTOR_PTR_RO(rows);

  for (R_xlen_t i = 0; i < ng; i++) {
    SEXP rows_i = p_rows[i];
    R_xlen_t n_i = XLENGTH(rows_i);
    int *p_rows_i = INTEGER(rows_i);
    for (R_xlen_t j = 0; j < n_i; j++, ++p_rows_i) {
      p_indices[*p_rows_i - 1] = i + 1;
    }
  }
  UNPROTECT(1);
  return indices;
}

// Taken from stackoverflow.com/questions/48118248
// NumericVector Rcpp_sort(NumericVector x, NumericVector y) {
//   // Order the elements of x by sorting y
//   // First create a vector of indices
//   IntegerVector idx = seq_along(x) - 1;
//   // Then sort that vector by the values of y
//   std::sort(idx.begin(), idx.end(), [&](int i, int j){return y[i] < y[j];});
//   // And return x in that order
//   return x[idx];
// }

SEXP rcpp_sort_in_place(SEXP x) {
  // Order the elements of x by sorting y
  // Then sort that vector by the values of y
  // std::sort(std::begin(idx), std::end(idx),
  //           [&](int i, int j){
  //             if (y[i] == NA_INTEGER){
  //               return false;
  //             } else if (y[j] == NA_INTEGER){
  //               return true;
  //             } else {
  //               return y[i] < y[j];
  //             }
  //             });
  // std::sort(std::begin(y), std::end(y),
  //           [](double i, double j){
  //             if (!(i == i)){
  //               return false;
  //             }
  //             if (!(j == j)){
  //               return true;
  //             }
  //             return i < j;
  //           });
  switch(TYPEOF(x)){
  case LGLSXP: {
    Rcpp::as<LogicalVector>(x).sort();
    break;
  }
  case INTSXP: {
    Rcpp::as<IntegerVector>(x).sort();
   break;
  }
  case REALSXP: {
    Rcpp::as<NumericVector>(x).sort();
   break;
  }
  case STRSXP: {
    Rcpp::as<CharacterVector>(x).sort();
   break;
  }
  default: {
    Rf_error("Cannot sort the supplied SEXP");
  }
  }
  return x;
}

bool r_is_sorted(SEXP x) {
  Rcpp::Function r_is_unsorted = Rcpp::Environment::base_env()["is.unsorted"];
  SEXP is_unsorted = PROTECT(r_is_unsorted(x));
  int *p_out = LOGICAL(is_unsorted);
  bool out = p_out[0];
  out = (out != NA_LOGICAL) && !out;
  UNPROTECT(1);
  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP cpp_roll_na_fill_grouped(SEXP x, SEXP g, double fill_limit,
                              bool check_sorted) {
  R_xlen_t size = Rf_xlength(x);
  R_xlen_t g_size = Rf_xlength(g);
  bool has_groups = g_size > 0;
  if (has_groups && g_size != size){
    Rf_error("x and g must both be the same length");
  }
  SEXP groups = PROTECT(Rf_coerceVector(g, INTSXP));
  // This will always evaluate to TRUE when g contains NA
  if (check_sorted && !r_is_sorted(groups)){
    UNPROTECT(1);
    Rf_error("g must be a sorted integer vector");
  }
  int *p_groups = INTEGER(groups);
  fill_limit = std::fmax(fill_limit, 0);
  bool first_non_na = false;
  bool is_na;
  bool prev_is_not_na = false;
  R_xlen_t fill_count = 0;

  switch(TYPEOF(x)){
  case LGLSXP:
  case INTSXP: {
    int fill;
    SEXP out = PROTECT(Rf_duplicate(x));
    int *p_out = INTEGER(out);
    for (R_xlen_t i = 0; i < size; ++i) {
      // Start of new group?
      if (has_groups && i > 0 && p_groups[i] != p_groups[i - 1]){
        first_non_na = false;
        fill_count = 0;
      }
      is_na = (p_out[i] == NA_INTEGER);
      if (!first_non_na && !is_na){
        first_non_na = true;
      }
      // Resetting fill value
      // Are we in new NA run?
      if (is_na && first_non_na && prev_is_not_na){
        fill_count = 0;
        fill = p_out[i - 1];
      }
      // Should we fill this NA value?
      if (is_na && first_non_na && fill_count < fill_limit){
        p_out[i] = fill;
        fill_count += 1;
      }
      prev_is_not_na = !is_na;
    }
    UNPROTECT(2);
    return out;
  }
  case REALSXP: {
    double fill;
    SEXP out = PROTECT(Rf_duplicate(x));
    double *p_out = REAL(out);
    for (R_xlen_t i = 0; i < size; ++i) {
      // Start of new group?
      if (has_groups && i > 0 && p_groups[i] != p_groups[i - 1]){
        first_non_na = false;
        fill_count = 0;
      }
      is_na = !(p_out[i] == p_out[i]);
      if (!first_non_na && !is_na){
        first_non_na = true;
      }
      // Resetting fill value
      // Are we in new NA run?
      if (is_na && first_non_na && prev_is_not_na){
        fill_count = 0;
        fill = p_out[i - 1];
      }
      // Should we fill this NA value?
      if (is_na && first_non_na && fill_count < fill_limit){
        p_out[i] = fill;
        fill_count += 1;
      }
      prev_is_not_na = !is_na;
    }
    UNPROTECT(2);
    return out;
  }
  case STRSXP: {
    CharacterVector fill(1);
    CharacterVector out = Rcpp::as<CharacterVector>(x);
    out = Rcpp::clone(out);
    for (R_xlen_t i = 0; i < size; ++i) {
      // Start of new group?
      if (has_groups && i > 0 && p_groups[i] != p_groups[i - 1]){
        first_non_na = false;
        fill_count = 0;
      }
      is_na = Rcpp::CharacterVector::is_na(out[i]);
      if (!first_non_na && !is_na){
        first_non_na = true;
      }
      // Resetting fill value
      // Are we in new NA run?
      if (is_na && first_non_na && prev_is_not_na){
        fill_count = 0;
        fill[0] = out[i - 1];
      }
      // Should we fill this NA value?
      if (is_na && first_non_na && fill_count < fill_limit){
        out[i] = fill[0];
        fill_count += 1;
      }
      prev_is_not_na = !is_na;
    }
    UNPROTECT(1);
    return out;
  }
  default: {
    UNPROTECT(1);
    Rf_error("cpp_roll_na_fill_grouped cannot handle the supplied SEXP");
    break;
  }
  }
}

// SEXP pmax2(NumericVector x, NumericVector y){
//   R_xlen_t n1 = Rf_xlength(x);
//   R_xlen_t n2 = Rf_xlength(y);
//   R_xlen_t n = std::max(n1, n2);
//   if (n1 <= 0 || n2 <= 0){
//     n = 0;
//   }
//   SEXP maxes = PROTECT(Rf_allocVector(REALSXP, n));
//   double *p_maxes = REAL(maxes);
//   R_xlen_t xi;
//   R_xlen_t yi;
//   for (R_xlen_t i = 0; i < n; ++i){
//     xi = (i % n1);
//     yi = (i % n2);
//     p_maxes[i] = std::fmax(x[xi], y[yi]);
//   }
//   UNPROTECT(1);
//   return maxes;
// }
