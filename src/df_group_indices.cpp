#include <Rcpp.h>
using namespace Rcpp;
#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))

// My slower version, not sure why so slow..
// IntegerVector cpp_df_group_indices(List rows, int size) {
//   int n_groups = rows.size();
//   IntegerVector out(size);
//   for (int i = 0; i < n_groups; i++){
//     IntegerVector ind = Rcpp::as<IntegerVector>(rows[i]);
//     out[ind - 1] = i + 1;
//   }
//   return out;
// }

// Taken from dplyr::group_indices,
// All credits go to dplyr
// [[Rcpp::export]]
SEXP cpp_df_group_indices(SEXP rows, int size) {
  SEXP indices = PROTECT(Rf_allocVector(INTSXP, size));
  int* p_indices = INTEGER(indices);
  R_xlen_t ng = XLENGTH(rows);
  const SEXP* p_rows = VECTOR_PTR_RO(rows);

  for (R_xlen_t i = 0; i < ng; i++) {
    SEXP rows_i = p_rows[i];
    R_xlen_t n_i = XLENGTH(rows_i);
    int* p_rows_i = INTEGER(rows_i);
    for (R_xlen_t j = 0; j < n_i; j++, ++p_rows_i) {
      p_indices[*p_rows_i - 1] = i + 1;
    }
  }
  UNPROTECT(1);
  return indices;
}

