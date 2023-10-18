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
