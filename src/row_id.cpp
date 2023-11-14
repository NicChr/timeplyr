#include <cpp11.hpp>
#include <Rinternals.h>

#define R_NO_REMAP


// Row numbers by group using order of groups and group sizes
[[cpp11::register]]
SEXP cpp_row_id(SEXP order, SEXP group_sizes, bool ascending){
  int n = Rf_length(order);
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  int *p_o = INTEGER(order);
  int *p_group_sizes = INTEGER(group_sizes);
  int j = 0;
  int running_group_size;
  if (Rf_length(group_sizes) == 0){
    running_group_size = n;
  } else {
    running_group_size = p_group_sizes[0];
  }
  if (ascending){
    int init = 0;
    for (int i = 0; i < n; i++){
      if (i > (running_group_size - 1)){
        ++j;
        init = 0;
        running_group_size += p_group_sizes[j];
      }
      ++init;
      p_out[p_o[i] - 1] = init;
    }
  } else {
    int init = running_group_size + 1;
    for (int i = 0; i < n; i++){
      if (i > (running_group_size - 1)){
        ++j;
        init = p_group_sizes[j] + 1;
        running_group_size += p_group_sizes[j];
      }
      --init;
      p_out[p_o[i] - 1] = init;
    }
  }
  Rf_unprotect(1);
  return out;
}

// WORKING METHOD THAT USES GROUP START LOCATIONS (SORTED)
// SEXP cpp_row_id2(SEXP order, SEXP sorted_group_starts){
//     R_xlen_t n = Rf_xlength(order);
//     R_xlen_t starts_size = Rf_xlength(sorted_group_starts);
//     bool is_long;
//     is_long = (n > std::numeric_limits<int>::max());
//     if (is_long){
//         R_xlen_t oi;
//         SEXP out = Rf_protect(Rf_allocVector(REALSXP, n));
//         double *p_out = REAL(out);
//         double *p_o = REAL(order);
//         double *p_group_starts = REAL(sorted_group_starts);
//         R_xlen_t init = 0;
//         R_xlen_t j = 0;
//         for (R_xlen_t i = 0; i < n; i++){
//             if (j < starts_size && i >= (p_group_starts[j] - 1)){
//                 ++j;
//                 init = 0;
//             }
//             ++init;
//             oi = p_o[i] - 1;
//             p_out[oi] = init;
//         }
//         Rf_unprotect(1);
//         return out;
//     } else {
//         SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
//         int *p_out = INTEGER(out);
//         int *p_o = INTEGER(order);
//         int *p_group_starts = INTEGER(sorted_group_starts);
//         int init = 0;
//         int j = 0;
//         for (int i = 0; i < n; i++){
//             if (j < starts_size && i >= (p_group_starts[j] - 1)){
//                 ++j;
//                 init = 0;
//             }
//             ++init;
//             p_out[p_o[i] - 1] = init;
//         }
//         Rf_unprotect(1);
//         return out;
//     }
// }
// SEXP cpp_row_id(SEXP order, SEXP group_sizes){
//     int n = Rf_length(order);
//     SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
//     int *p_out = INTEGER(out);
//     int *p_o = INTEGER(order);
//     int *p_group_sizes = INTEGER(group_sizes);
//     int init = 0;
//     int j = 0;
//     int running_group_size = p_group_sizes[0];
//     for (int i = 0; i < n; i++){
//         ++init;
//         p_out[p_o[i] - 1] = init;
//         if (i >= (running_group_size - 1)){
//             ++j;
//             init = 0;
//             running_group_size += p_group_sizes[j];
//         }
//     }
//     Rf_unprotect(1);
//     return out;
// }
