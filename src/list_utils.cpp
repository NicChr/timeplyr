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
SEXP list_item_is_interval( List l ) {
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
