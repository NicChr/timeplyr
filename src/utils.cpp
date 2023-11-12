#include <cpp11.hpp>
#include <Rinternals.h>

#define R_NO_REMAP
#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))

[[cpp11::register]]
bool test_long_vector_support() {
#ifdef RCPP_HAS_LONG_LONG_TYPES
  return true;
#else
  return false;
#endif
}


[[cpp11::register]]
SEXP cpp_list_which_not_null(SEXP l) {
  // Coerce l to list
  Rf_protect(l = Rf_coerceVector(l, VECSXP));
  const SEXP *p_l = VECTOR_PTR_RO(l);
  int n = Rf_length(l);
  // Create logical vector
  SEXP keep = Rf_protect(Rf_allocVector(LGLSXP, n));
  int *p_keep = LOGICAL(keep);
  int size = 0;
  bool not_null;
  for (int i = 0; i < n; ++i) {
    not_null = !Rf_isNull(p_l[i]);
    p_keep[i] = not_null;
    // Keeping track of number of true values
    size += not_null;
  }
  int whichi = 0;
  int i = 0;
  // The below is essentially which(keep)
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, size));
  int *p_out = INTEGER(out);
  while (whichi < size){
    p_out[whichi] = i + 1;
    whichi += (p_keep[i] == TRUE);
    ++i;
  }
  Rf_unprotect(3);
  return out;
}

bool is_interval(SEXP x){
  return (Rf_isS4(x) && Rf_inherits(x, "Interval"));
}

[[cpp11::register]]
bool list_has_interval( SEXP l ) {
  Rf_protect(l = Rf_coerceVector(l, VECSXP));
  const SEXP *p_l = VECTOR_PTR_RO(l);
  bool out = false;
  int n = Rf_length(l);
  for (int i = 0; i < n; ++i) {
    if (is_interval(p_l[i])){
      out = true;
      // n = i;
      break;
    }
  }
  Rf_unprotect(1);
  return out;
}

[[cpp11::register]]
SEXP list_item_is_interval( SEXP l ) {
  Rf_protect(l = Rf_coerceVector(l, VECSXP));
  const SEXP *p_l = VECTOR_PTR_RO(l);
  int n = Rf_length(l);
  SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n));
  int *p_out = LOGICAL(out);
  for (int i = 0; i < n; ++i) {
    p_out[i] = is_interval(p_l[i]);
  }
  Rf_unprotect(2);
  return out;
}

// Take a vector of group sizes (sorted by group)
// And this will return a vector of the start indices of each group (in sorted order)

[[cpp11::register]]
SEXP cpp_sorted_group_starts(SEXP group_sizes){
  int *p_gsizes = INTEGER(group_sizes);
  int n = Rf_length(group_sizes);
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  int init = 1;
  p_out[0] = 1;
  // cumsum over group_sizes[-length(group_sizes)]
  for (int i = 0; i < (n - 1); i++){
    init += p_gsizes[i];
    p_out[i + 1] = init;
  }
  Rf_unprotect(1);
  return out;
}

// Internal function
// x must be a numeric or integer
// threshold must be a numeric or integer of length 1
// This basically takes as input a cumulatively increasing time vector,
// and once a threshold is reached
// This is flagged a 1, the threshold is reset at that time value, and
// a new flag will occur once this new threshold is reached, and so on.
// Again, x must be a cumulatively increasing vector.

[[cpp11::register]]
SEXP roll_time_threshold(SEXP x, double threshold, bool switch_on_boundary) {
  int n = Rf_length(x);
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  double init_threshold = 0;
  init_threshold = init_threshold + threshold;
  switch( TYPEOF(x) ) {
  case REALSXP: {
    double *p_x = REAL(x);
    double tol = sqrt(std::numeric_limits<double>::epsilon());
    if (switch_on_boundary){
      tol = -tol;
    }
    for (int i = 0; i < n; ++i) {
      if (!(p_x[i] == p_x[i])){
        p_out[i] = NA_INTEGER;
      } else if ((p_x[i] - threshold) > tol) {
        p_out[i] = 1;
        threshold = init_threshold + p_x[i];
      } else {
        p_out[i] = 0;
      }
    }
    break;
  }
  case INTSXP: {
    int *p_x = INTEGER(x);
    if (switch_on_boundary == true){
      for (int i = 0; i < n; ++i) {
        if (p_x[i] == NA_INTEGER){
          p_out[i] = NA_INTEGER;
        } else if (p_x[i] >= threshold) {
          p_out[i] = 1;
          threshold = init_threshold + p_x[i];
        } else {
          p_out[i] = 0;
        }
      }
    } else {
      for (int i = 0; i < n; ++i) {
        if (p_x[i] == NA_INTEGER){
          p_out[i] = NA_INTEGER;
        } else if (p_x[i] > threshold) {
          p_out[i] = 1;
          threshold = init_threshold + p_x[i];
        } else {
          p_out[i] = 0;
        }
      }
    }
    break;
  }
  default: {
    Rf_unprotect(1);
    Rf_error("roll_time_threshold only supports integer and numeric vectors");
  }
  }
  Rf_unprotect(1);
  return out;
}

// Taken from dplyr::group_indices,
// All credits go to dplyr

[[cpp11::register]]
SEXP cpp_df_group_indices(SEXP rows, int size) {
  SEXP indices = Rf_protect(Rf_allocVector(INTSXP, size));
  int *p_indices = INTEGER(indices);
  R_xlen_t ng = XLENGTH(rows);
  const SEXP* p_rows = VECTOR_PTR_RO(rows);

  for (R_xlen_t i = 0; i < ng; i++) {
    SEXP rows_i = p_rows[i];
    R_xlen_t n_i = XLENGTH(rows_i);
    int *p_rows_i = INTEGER(rows_i);
    for (R_xlen_t j = 0; j < n_i; j++, ++p_rows_i) {
      p_indices[*p_rows_i - 1] = i + 1;
    }
  }
  Rf_unprotect(1);
  return indices;
}

// Taken from stackoverflow.com/questions/48118248
// NumericVector Rcpp_sort(NumericVector x, NumericVector y) {
//   // Order the elements of x by sorting y
//   // First create a vector of indices
//   IntegerVector idx = seq_along(x) - 1;
//   // Then sort that vector by the values of y
//   std::sort(idx.begin(), idx.end(), [&](int i, int j){return y[i] < y[j];});
//   // And return x in that order
//   return x[idx];
// }

// SEXP rcpp_sort_in_place(SEXP x) {
//   // Order the elements of x by sorting y
//   // Then sort that vector by the values of y
//   // std::sort(std::begin(idx), std::end(idx),
//   //           [&](int i, int j){
//   //             if (y[i] == NA_INTEGER){
//   //               return false;
//   //             } else if (y[j] == NA_INTEGER){
//   //               return true;
//   //             } else {
//   //               return y[i] < y[j];
//   //             }
//   //             });
//   // std::sort(std::begin(y), std::end(y),
//   //           [](double i, double j){
//   //             if (!(i == i)){
//   //               return false;
//   //             }
//   //             if (!(j == j)){
//   //               return true;
//   //             }
//   //             return i < j;
//   //           });
//   switch(TYPEOF(x)){
//   case LGLSXP: {
//     Rcpp::as<LogicalVector>(x).sort();
//     break;
//   }
//   case INTSXP: {
//     Rcpp::as<IntegerVector>(x).sort();
//    break;
//   }
//   case REALSXP: {
//     Rcpp::as<NumericVector>(x).sort();
//    break;
//   }
//   case STRSXP: {
//     Rcpp::as<CharacterVector>(x).sort();
//    break;
//   }
//   default: {
//     Rf_error("Cannot sort the supplied SEXP");
//   }
//   }
//   return x;
// }
