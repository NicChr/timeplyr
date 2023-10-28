#include <Rcpp.h>
#include <Rinternals.h>
using namespace Rcpp;
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
    Rcpp::stop("num_na cannot handle the supplied SEXP");
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
// [[Rcpp::export(rng = false)]]
bool list_has_interval( List l ) {
  bool out = false;
  int n = l.length();
  for (int i = 0; i < n; ++i) {
    if (Rf_isReal(l[i]) && Rf_inherits(l[i], "Interval")){
      out = true;
      break;
    }
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP list_item_is_interval( List l ) {
  int n = l.size();
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *p_out = LOGICAL(out);
  for (int i = 0; i < n; ++i) {
    p_out[i] = Rf_isReal(l[i]) && Rf_inherits(l[i], "Interval");
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
    Rcpp::stop("roll_time_threshold only supports integer and numeric vectors");
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
