#include <Rcpp.h>

// Whole number check
// NA values are ignored by default

// [[Rcpp::export(rng = false)]]
SEXP cpp_is_whole_num(SEXP x, SEXP tol, bool na_rm = true) {
  R_xlen_t n = Rf_xlength(x);
  int tol_len = Rf_length(tol);
  if (tol_len > 1){
    Rcpp::stop("tol must be of length <= 1");
  }
  if (tol_len == 0){
    n = 0;
  }
  bool is_whole;
  double adiff;
  R_xlen_t n_na = 0;
  bool is_na;
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, 1));
  int *p_out = INTEGER(out);
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
    double *p_t = REAL(tol);
    for (R_xlen_t i = 0; i < n; ++i) {
      adiff = std::fabs(p_x[i] - std::round(p_x[i]));
      is_whole = (adiff < p_t[0]);
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

// bool is_whole_num(SEXP x, Nullable<NumericVector> tol) {
//   double tolerance = std::sqrt(std::numeric_limits<double>::epsilon());
//   if (tol.isNotNull()){
//     tolerance = Rcpp::as<double>(tol);
//   }
//   bool is_whole = true;
//   double rdiff;
//   double adiff;
//   double rounded;
//   bool out = false;
//   double diff_zero;
//   bool is_zero;
//   switch ( TYPEOF(x) ){
//   case LGLSXP:
//   case INTSXP: {
//     out = true;
//     break;
//   }
//   case REALSXP: {
//     // Re-initialise so that we can break when we find non-whole num
//     out = true;
//     R_xlen_t n = Rf_xlength(x);
//     double *p_x = REAL(x);
//     for (R_xlen_t i = 0; i < n; ++i) {
//       diff_zero = std::fabs(p_x[i]);
//       is_zero = diff_zero < tolerance;
//       rounded = std::round(p_x[i]);
//       adiff = std::fabs(rounded - p_x[i]);
//       rdiff = adiff / std::fmax(std::fabs(p_x[i]), std::fabs(rounded));
//       is_whole = is_zero || ( (adiff < tolerance) && (rdiff < tolerance) );
//       if (!is_whole && !(p_x[i] != p_x[i])){
//         out = false;
//         break;
//       }
//     }
//     break;
//   }
//   }
//   return out;
// }

// Vectorised version
// SEXP cpp_is_whole_num(SEXP x, SEXP tol, bool na_rm = true) {
//   R_xlen_t x_len = Rf_xlength(x);
//   R_xlen_t tol_len = Rf_xlength(tol);
//   R_xlen_t n = std::max(x_len, tol_len);
//   if (x_len <= 0 || tol_len <= 0){
//     n = 0;
//   }
//   bool is_whole;
//   double adiff;
//   R_xlen_t n_na = 0;
//   bool is_na;
//   SEXP out = PROTECT(Rf_allocVector(LGLSXP, 1));
//   int *p_out = INTEGER(out);
//   p_out[0] = false;
//   switch ( TYPEOF(x) ){
//   case LGLSXP:
//   case INTSXP: {
//     p_out[0] = true;
//     break;
//   }
//   case REALSXP: {
//     // Re-initialise so that we can break when we find non-whole num
//     p_out[0] = true;
//     double *p_x = REAL(x);
//     double *p_t = REAL(tol);
//     R_xlen_t xi;
//     R_xlen_t ti;
//     for (R_xlen_t i = 0; i < n; ++i) {
//       xi = (i % x_len);
//       ti = (i % tol_len);
//       // adiff = std::remainder(p_x[xi], 1); // Alternative method
//       adiff = std::fabs(p_x[xi] - std::round(p_x[xi]));
//       is_whole = (adiff < p_t[ti]);
//       is_na = !(p_x[xi] == p_x[xi]);
//       n_na += is_na;
//       if (!is_whole && !is_na){
//         p_out[0] = false;
//         break;
//       }
//     }
//     if (!na_rm && n_na > 0){
//       p_out[0] = NA_LOGICAL;
//       break;
//     }
//     break;
//   }
//   }
//   UNPROTECT(1);
//   return out;
// }

// Keeping this as a pedagogical example of how to use R functions in cpp
// bool is_whole_num(NumericVector x) {
//   Function get_tol("sqrt_double_eps");
//   NumericVector tol_val = get_tol();
//   double tol = Rcpp::as<double>(tol_val);
//   bool has_decimal;
//   double diff;
//   bool out = true;
//   for (int i = 0; i < x.length(); ++i) {
//     diff = abs(round(x[i]) - x[i]);
//     has_decimal = !(diff < tol);
//     if (has_decimal && !NumericVector::is_na(x[i])){
//       out = false;
//       break;
//     }
//   }
//   return out;
// }
