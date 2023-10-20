#include <Rcpp.h>
using namespace Rcpp;

bool double_is_na(double x){
  return !(x == x);
}
bool double_both_inf(double x, double y){
  return (x == R_PosInf && y == R_PosInf) || (x == R_NegInf && y == R_NegInf);
}
bool cpp_double_equal_rel(double x, double y, double tolerance){
  double ax = std::fabs(x);
  double ay = std::fabs(y);
  bool both_zero = ( ax < tolerance ) && ( ay < tolerance );
  double adiff = std::fabs(x - y);
  double rdiff = adiff / std::max(ax, ay);
  return double_both_inf(x, y) || both_zero || (rdiff < tolerance);
}
bool cpp_double_equal_abs(double x, double y, double tolerance){
  double adiff = std::fabs(x - y);
  return double_both_inf(x, y) || (adiff < tolerance);
}
bool cpp_double_equal_strict(double x, double y, double tolerance){
  double ax = std::fabs(x);
  double ay = std::fabs(y);
  bool both_zero = ( ax < tolerance ) && ( ay < tolerance );
  double adiff = std::fabs(x - y);
  double rdiff = adiff / std::max(ax, ay);
  return (
      both_zero ||
        double_both_inf(x, y) || (
          (adiff < tolerance) && (rdiff < tolerance)
      )
  );
}

// [[Rcpp::export(rng = false)]]
LogicalVector cpp_double_equal_vectorised(SEXP x, SEXP y, Nullable<NumericVector> tol) {
  double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
  if (tol.isNotNull()){
    tolerance = Rcpp::as<double>(tol);
  }
  int n = Rf_length(x);
  if (Rf_length(y) != n){
    stop("x must be the same length as y");
  }
  double *p_x = REAL(x);
  double *p_y = REAL(y);
  LogicalVector out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = cpp_double_equal_rel(p_x[i], p_y[i], tolerance);
    // If either is NA, out is NA
    if ( double_is_na(p_x[i]) || double_is_na(p_y[i]) ){
      out[i] = NA_LOGICAL;
    }
  }
  return out;
}
// LogicalVector cpp_double_equal(NumericVector x, NumericVector y, Nullable<NumericVector> tol) {
//   double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
//   if (tol.isNotNull()){
//     tolerance = Rcpp::as<double>(tol);
//   }
//   double rdiff;
//   double adiff;
//   double ax;
//   double ay;
//   // bool any_zero;
//   bool both_zero;
//   int n = x.length();
//   if (y.length() != n){
//     stop("x must be the same length as y");
//   }
//   LogicalVector out(n);
//   for (int i = 0; i < n; ++i) {
//     ax = std::fabs(x[i]);
//     ay = std::fabs(y[i]);
//     both_zero = ( ax < tolerance ) && ( ay < tolerance );
//     adiff = std::fabs(x[i] - y[i]);
//     rdiff = adiff / std::max(ax, ay);
//     out[i] = both_zero || (
//       // (any_zero && adiff < tolerance) || (
//           (adiff < tolerance) && (rdiff < tolerance)
//       );
//     if (Rcpp::NumericVector::is_na(x[i]) ||
//         Rcpp::NumericVector::is_na(y[i])){
//       out[i] = NA_REAL;
//     }
//   }
//   return out;
// }
// Working good version
// LogicalVector cpp_double_equal(SEXP x, SEXP y, Nullable<NumericVector> tol) {
//   double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
//   if (tol.isNotNull()){
//     tolerance = Rcpp::as<double>(tol);
//   }
//   double rdiff;
//   double adiff;
//   double ax;
//   double ay;
//   // bool any_zero;
//   bool both_zero;
//   int n = Rf_length(x);
//   double *p_x = REAL(x);
//   double *p_y = REAL(y);
//   if (Rf_length(y) != n){
//     stop("x must be the same length as y");
//   }
//   LogicalVector out(n);
//   for (int i = 0; i < n; ++i) {
//     ax = std::fabs(p_x[i]);
//     ay = std::fabs(p_y[i]);
//     both_zero = ( ax < tolerance ) && ( ay < tolerance );
//     adiff = std::fabs(p_x[i] - p_y[i]);
//     rdiff = adiff / std::max(ax, ay);
//     out[i] = both_zero || (
//       (adiff < tolerance) && (rdiff < tolerance)
//     );
//     // If either is NA, out is NA
//     if ( ( p_x[i] != p_x[i] ) || ( p_y[i] != p_y[i] ) ){
//       out[i] = NA_REAL;
//     }
//   }
//   return out;
// }
