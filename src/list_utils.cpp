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
