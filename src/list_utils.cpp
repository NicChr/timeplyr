#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List list_rm_null(List l) {
  int n = l.size();
  LogicalVector keep(n);
  for (int i = 0; i < n; ++i) {
    keep[i] = !Rf_isNull(l[i]);
  }
  return l[keep];
}
// LogicalVector list_item_is_null(List l) {
//   int n = l.size();
//   LogicalVector out(n);
//   for (int i = 0; i < n; ++i) {
//     out[i] = Rf_isNull(l[i]);
//   }
//   return out;
// }

// [[Rcpp::export]]
CharacterVector numeric_class(NumericVector x){
  CharacterVector out(1);
  if (x.hasAttribute("class")){
    out = wrap(x.attr("class"));
  }
  return out;
}

// [[Rcpp::export]]
bool list_has_interval( List l ) {
  bool out = false;
  std::string int_str = "Interval";
  for( List::iterator it = l.begin(); it != l.end(); ++it ) {
    switch( TYPEOF(*it) ) {
    case REALSXP: {
      NumericVector tmp = Rcpp::as<NumericVector>(*it);
      CharacterVector x_class = numeric_class(tmp);
      if (x_class[0] == int_str){
        out = true;
        break;
      }
    }
    }
    if (out){
      break;
    }
  }
  return out;
}
// [[Rcpp::export]]
LogicalVector list_item_is_interval( List l ) {
  int n = l.size();
  LogicalVector out(n);
  std::string int_str = "Interval";
  for (int i = 0; i < n; ++i) {
    switch( TYPEOF(l[i]) ) {
    case REALSXP: {
      NumericVector tmp = Rcpp::as<NumericVector>(l[i]);
      CharacterVector x_class = numeric_class(tmp);
      out[i] = (x_class[0] == int_str);
    }
    }
  }
  return out;
}
// LogicalVector list_item_is_interval( List l ) {
//   int n = l.size();
//   LogicalVector out(n);
//   std::string int_str = "Interval";
//   int i;
//   bool tmp_bool;
//   for( List::iterator it = l.begin(); it != l.end(); ++it ) {
//     switch( TYPEOF(*it) ) {
//     case REALSXP: {
//       NumericVector tmp = Rcpp::as<NumericVector>(*it);
//       CharacterVector x_class = numeric_class(tmp);
//       tmp_bool = (x_class[0] == int_str);
//       i = it;
//       out[i] = tmp_bool;
//     }
//     }
//   }
//   return out;
// }
