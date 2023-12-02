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
