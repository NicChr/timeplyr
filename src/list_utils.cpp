#include <Rcpp.h>
using namespace Rcpp;

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
  for( List::iterator it = l.begin(); it != l.end(); ++it ) {
    switch( TYPEOF(*it) ) {
    case REALSXP: {
      if (Rf_inherits(*it, "Interval")){
        out = true;
      }
      break;
    }
    }
    if (out){
      break;
    }
  }
  return out;
}
// [[Rcpp::export(rng = false)]]
LogicalVector list_item_is_interval( List l ) {
  int n = l.size();
  LogicalVector out(n);
  for (int i = 0; i < n; ++i) {
    switch( TYPEOF(l[i]) ) {
    case REALSXP: {
      out[i] = Rf_inherits(l[i], "Interval");
      break;
    }
    }
  }
  return out;
}
// SEXP pmax2(SEXP x, SEXP y){
//   int n1 = Rf_length(x);
//   int n2 = Rf_length(y);
//   int n = std::max(n1, n2);
//   if (n1 <= 0 || n2 <= 0){
//     n = 0;
//   }
//   double *p_x = REAL(x);
//   double *p_y = REAL(y);
//   SEXP maxes = PROTECT(Rf_allocVector(REALSXP, n));
//   double *p_maxes = REAL(maxes);
//   int xi;
//   int yi;
//   for (int i = 0; i < n1; ++i){
//     xi = (i % n1);
//     yi = (i % n2);
//     p_maxes[i] = std::fmax(p_x[xi], p_y[yi]);
//   }
//   UNPROTECT(1);
//   return maxes;
// }
// NumericVector pmax3(SEXP x, SEXP y){
//   int n1 = Rf_length(x);
//   int n2 = Rf_length(y);
//   if (n1 != n2){
//     stop("x and y must be of equal length");
//   }
//   double *p_x = REAL(x);
//   double *p_y = REAL(y);
//   NumericVector out(n1);
//   for (int i = 0; i < n1; ++i){
//     out[i] = std::fmax(p_x[i], p_y[i]);
//   }
//   return out;
// }
