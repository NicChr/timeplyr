#include <Rcpp.h>
using namespace Rcpp;

bool double_is_na(double x){
  return !(x == x);
}
bool double_both_inf(double x, double y){
  return (x == R_PosInf && y == R_PosInf) || (x == R_NegInf && y == R_NegInf);
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
    out = double_both_inf(x, y) || (rdiff < tolerance);
  }
  return out;
}
bool cpp_double_equal_abs(double x, double y, double tolerance){
  double adiff = std::fabs(x - y);
  return double_both_inf(x, y) || (adiff < tolerance);
}
bool cpp_double_equal_strict(double x, double y, double tolerance){
  return cpp_double_equal_abs(x, y, tolerance) &&
    cpp_double_equal(x, y, tolerance);
}

// [[Rcpp::export(rng = false)]]
SEXP cpp_double_equal_vectorised(SEXP x, SEXP y, double tolerance) {
  // double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
  int n = Rf_length(x);
  if (Rf_length(y) != n){
    stop("x must be the same length as y");
  }
  double *p_x = REAL(x);
  double *p_y = REAL(y);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int *p_out = LOGICAL(out);
  for (int i = 0; i < n; ++i) {
    p_out[i] = cpp_double_equal(p_x[i], p_y[i], tolerance);
    // If either is NA, out is NA
    if ( double_is_na(p_x[i]) || double_is_na(p_y[i]) ){
      p_out[i] = NA_LOGICAL;
    }
  }
  UNPROTECT(1);
  return out;
}
