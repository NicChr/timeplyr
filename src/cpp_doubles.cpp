#include <Rcpp.h>

// Below are a complete set of C++ functions for comparing doubles
// mimicing the ==, <=, <, > and >= operators with a tolerance

// Relative differences are used except when either x or y is very close to zero
// in which case absolute differences are used

// Recycle an index using the length of the vector being indexed
// This in theory allows flexible vector recycling within a loop without allocation

// int recycle_index(int i, int n){
//   if (n == 0){
//     Rf_error("Cannot recycle index i for a zero-length vector");
//   }
//   return (i % n);
// }

bool cpp_double_is_na(double x){
  return !(x == x);
}
bool cpp_double_both_same_inf(double x, double y){
  return (x == R_PosInf && y == R_PosInf) || (x == R_NegInf && y == R_NegInf);
}
bool cpp_double_any_inf(double x, double y){
  return x == R_PosInf || y == R_PosInf || x == R_NegInf || y == R_NegInf;
}
double cpp_double_abs_diff(double x, double y){
  return std::fabs(x - y);
}
double cpp_double_rel_diff(double x, double y){
  return cpp_double_abs_diff(x, y) / std::fmax(std::fabs(x), std::fabs(y));
}
bool cpp_double_any_zero(double x, double y, double tolerance){
  return ( std::fabs(x) < tolerance ) || ( std::fabs(y) < tolerance );
}

// Testing equality

bool cpp_double_equal(double x, double y, double tolerance){
  double ax = std::fabs(x);
  double ay = std::fabs(y);
  bool any_zero = ( ax < tolerance ) || ( ay < tolerance );
  double adiff = std::fabs(x - y);
  double rdiff = adiff / std::fmax(ax, ay);
  double out;
  if (any_zero){
    out = (adiff < tolerance);
  } else {
    out = cpp_double_both_same_inf(x, y) || (rdiff < tolerance);
  }
  return out;
}
bool cpp_double_equal_abs(double x, double y, double tolerance){
  double adiff = std::fabs(x - y);
  return cpp_double_both_same_inf(x, y) || (adiff < tolerance);
}
bool cpp_double_equal_strict(double x, double y, double tolerance){
  return cpp_double_equal_abs(x, y, tolerance) &&
    cpp_double_equal(x, y, tolerance);
}

// Testing >, >=, < and <=
bool cpp_double_gt(double x, double y, double tolerance){
  double out;
  double diff = (x - y);
  bool any_zero = cpp_double_any_zero(x, y, tolerance);
  if (any_zero || cpp_double_any_inf(x, y)){
    out = diff > tolerance;
  } else {
    out = (diff / std::fmax(std::fabs(x), std::fabs(y))) > tolerance;
  }
  return out;
}
bool cpp_double_lt(double x, double y, double tolerance){
  double out;
  double diff = (x - y);
  bool any_zero = cpp_double_any_zero(x, y, tolerance);
  if (any_zero || cpp_double_any_inf(x, y)){
    out = diff < -tolerance;
  } else {
    out = (diff / std::fmax(std::fabs(x), std::fabs(y))) < -tolerance;
  }
  return out;
}
bool cpp_double_gte(double x, double y, double tolerance){
  return cpp_double_gt(x, y, tolerance) || cpp_double_equal(x, y, tolerance);
}
bool cpp_double_lte(double x, double y, double tolerance){
  return cpp_double_lt(x, y, tolerance) || cpp_double_equal(x, y, tolerance);
}


// [[Rcpp::export(rng = false)]]
SEXP cpp_double_equal_vectorised(SEXP x, SEXP y, SEXP tolerance) {
  // double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
  R_xlen_t x_len = Rf_xlength(x);
  R_xlen_t y_len = Rf_xlength(y);
  R_xlen_t tol_len = Rf_xlength(tolerance);
  R_xlen_t n = std::fmax(x_len, y_len);
  n = std::fmax(n, tol_len);
  if (x_len <= 0 || y_len <= 0 || tol_len <= 0){
    // Avoid loop if any are length zero vectors
    n = 0;
  }
  double *p_x = REAL(x);
  double *p_y = REAL(y);
  double *p_t = REAL(tolerance);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *p_out = LOGICAL(out);
  R_xlen_t xi;
  R_xlen_t yi;
  R_xlen_t ti;
  for (R_xlen_t i = 0; i < n; ++i) {
    xi = i % x_len;
    yi = i % y_len;
    ti = i % tol_len;
    p_out[i] = cpp_double_equal(p_x[xi], p_y[yi], p_t[ti]);
    // If either is NA, out is NA
    if ( cpp_double_is_na(p_x[xi]) || cpp_double_is_na(p_y[yi]) ){
      p_out[i] = NA_LOGICAL;
    }
  }
  UNPROTECT(1);
  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP cpp_double_gt_vectorised(SEXP x, SEXP y, SEXP tolerance) {
  // double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
  R_xlen_t x_len = Rf_xlength(x);
  R_xlen_t y_len = Rf_xlength(y);
  R_xlen_t tol_len = Rf_xlength(tolerance);
  R_xlen_t n = std::fmax(x_len, y_len);
  n = std::fmax(n, tol_len);
  if (x_len <= 0 || y_len <= 0 || tol_len <= 0){
    // Avoid loop if any are length zero vectors
    n = 0;
  }
  double *p_x = REAL(x);
  double *p_y = REAL(y);
  double *p_t = REAL(tolerance);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *p_out = LOGICAL(out);
  R_xlen_t xi;
  R_xlen_t yi;
  R_xlen_t ti;
  for (R_xlen_t i = 0; i < n; ++i) {
    xi = i % x_len;
    yi = i % y_len;
    ti = i % tol_len;
    p_out[i] = cpp_double_gt(p_x[xi], p_y[yi], p_t[ti]);
    // If either is NA, out is NA
    if ( cpp_double_is_na(p_x[xi]) || cpp_double_is_na(p_y[yi]) ){
      p_out[i] = NA_LOGICAL;
    }
  }
  UNPROTECT(1);
  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP cpp_double_gte_vectorised(SEXP x, SEXP y, SEXP tolerance) {
  // double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
  R_xlen_t x_len = Rf_xlength(x);
  R_xlen_t y_len = Rf_xlength(y);
  R_xlen_t tol_len = Rf_xlength(tolerance);
  R_xlen_t n = std::fmax(x_len, y_len);
  n = std::fmax(n, tol_len);
  if (x_len <= 0 || y_len <= 0 || tol_len <= 0){
    // Avoid loop if any are length zero vectors
    n = 0;
  }
  double *p_x = REAL(x);
  double *p_y = REAL(y);
  double *p_t = REAL(tolerance);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *p_out = LOGICAL(out);
  R_xlen_t xi;
  R_xlen_t yi;
  R_xlen_t ti;
  for (R_xlen_t i = 0; i < n; ++i) {
    xi = i % x_len;
    yi = i % y_len;
    ti = i % tol_len;
    p_out[i] = cpp_double_gte(p_x[xi], p_y[yi], p_t[ti]);
    // If either is NA, out is NA
    if ( cpp_double_is_na(p_x[xi]) || cpp_double_is_na(p_y[yi]) ){
      p_out[i] = NA_LOGICAL;
    }
  }
  UNPROTECT(1);
  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP cpp_double_lt_vectorised(SEXP x, SEXP y, SEXP tolerance) {
  // double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
  R_xlen_t x_len = Rf_xlength(x);
  R_xlen_t y_len = Rf_xlength(y);
  R_xlen_t tol_len = Rf_xlength(tolerance);
  R_xlen_t n = std::fmax(x_len, y_len);
  n = std::fmax(n, tol_len);
  if (x_len <= 0 || y_len <= 0 || tol_len <= 0){
    // Avoid loop if any are length zero vectors
    n = 0;
  }
  double *p_x = REAL(x);
  double *p_y = REAL(y);
  double *p_t = REAL(tolerance);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *p_out = LOGICAL(out);
  R_xlen_t xi;
  R_xlen_t yi;
  R_xlen_t ti;
  for (R_xlen_t i = 0; i < n; ++i) {
    xi = i % x_len;
    yi = i % y_len;
    ti = i % tol_len;
    p_out[i] = cpp_double_lt(p_x[xi], p_y[yi], p_t[ti]);
    // If either is NA, out is NA
    if ( cpp_double_is_na(p_x[xi]) || cpp_double_is_na(p_y[yi]) ){
      p_out[i] = NA_LOGICAL;
    }
  }
  UNPROTECT(1);
  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP cpp_double_lte_vectorised(SEXP x, SEXP y, SEXP tolerance) {
  // double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
  R_xlen_t x_len = Rf_xlength(x);
  R_xlen_t y_len = Rf_xlength(y);
  R_xlen_t tol_len = Rf_xlength(tolerance);
  R_xlen_t n = std::fmax(x_len, y_len);
  n = std::fmax(n, tol_len);
  if (x_len <= 0 || y_len <= 0 || tol_len <= 0){
    // Avoid loop if any are length zero vectors
    n = 0;
  }
  double *p_x = REAL(x);
  double *p_y = REAL(y);
  double *p_t = REAL(tolerance);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *p_out = LOGICAL(out);
  R_xlen_t xi;
  R_xlen_t yi;
  R_xlen_t ti;
  for (R_xlen_t i = 0; i < n; ++i) {
    xi = i % x_len;
    yi = i % y_len;
    ti = i % tol_len;
    p_out[i] = cpp_double_lte(p_x[xi], p_y[yi], p_t[ti]);
    // If either is NA, out is NA
    if ( cpp_double_is_na(p_x[xi]) || cpp_double_is_na(p_y[yi]) ){
      p_out[i] = NA_LOGICAL;
    }
  }
  UNPROTECT(1);
  return out;
}

// Whole number check

// [[Rcpp::export(rng = false)]]
SEXP cpp_is_whole_num(SEXP x, double tol, bool na_rm = true) {
  R_xlen_t n = Rf_xlength(x);
  bool is_whole;
  double adiff;
  R_xlen_t n_na = 0;
  bool is_na;
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, 1));
  int *p_out = LOGICAL(out);
  p_out[0] = false;
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    p_out[0] = true;
    break;
  }
  case REALSXP: {
    // Re-initialise so that we can break when we find non-whole num
    p_out[0] = true;
    double *p_x = REAL(x);
    for (R_xlen_t i = 0; i < n; ++i) {
      adiff = std::fabs(p_x[i] - std::round(p_x[i]));
      is_whole = (adiff < tol);
      is_na = !(p_x[i] == p_x[i]);
      n_na += is_na;
      if (!is_whole && !is_na){
        p_out[0] = false;
        break;
      }
    }
    if (!na_rm && n_na > 0){
      p_out[0] = NA_LOGICAL;
      break;
    }
    break;
  }
  }
  UNPROTECT(1);
  return out;
}

// SEXP cpp_whole_num(SEXP x, SEXP tol) {
//   R_xlen_t x_len = Rf_xlength(x);
//   R_xlen_t tol_len = Rf_xlength(tol);
//   R_xlen_t n = std::max(x_len, tol_len);
//   if (x_len <= 0 || tol_len <= 0){
//     n = 0;
//   }
//   double adiff;
//   bool is_na;
//   SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
//   int *p_out = LOGICAL(out);
//   switch ( TYPEOF(x) ){
//   case LGLSXP:
//   case INTSXP: {
//     for (R_xlen_t i = 0; i < n; ++i){
//     p_out[i] = true;
//   }
//     break;
//   }
//   case REALSXP: {
//     R_xlen_t xi;
//     R_xlen_t ti;
//     double *p_x = REAL(x);
//     double *p_t = REAL(PROTECT(Rf_coerceVector(tol, REALSXP)));
//     for (R_xlen_t i = 0; i < n; ++i) {
//       xi = i % x_len;
//       ti = i % tol_len;
//       adiff = std::fabs(p_x[xi] - std::round(p_x[xi]));
//       is_na = !(p_x[xi] == p_x[xi]) || !(p_t[ti] == p_t[ti]);
//       p_out[i] = (adiff < p_t[ti]);
//       if (is_na){
//         p_out[i] = NA_REAL;
//       }
//     }
//     UNPROTECT(1);
//     break;
//   }
//   default: {
//     for (R_xlen_t i = 0; i < n; ++i){
//     p_out[i] = false;
//   }
//     break;
//   }
//   }
//   UNPROTECT(1);
//   return out;
// }
