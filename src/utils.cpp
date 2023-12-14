#include <cpp11.hpp>
#include <Rinternals.h>

#define R_NO_REMAP
#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))

// bool test_long_vector_support() {
// #ifdef LONG_VECTOR_SUPPORT
//   return true;
// #else
//   return false;
// #endif
// }


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
  return (Rf_isS4(x) && Rf_inherits(x, "Interval")) || Rf_inherits(x, "time_interval");
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
  SEXP list_sexp = Rf_protect(Rf_coerceVector(l, VECSXP));
  const SEXP *p_l = VECTOR_PTR_RO(list_sexp);
  int n = Rf_length(list_sexp);
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
    double tol = std::sqrt(std::numeric_limits<double>::epsilon());
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
  R_xlen_t ng = Rf_xlength(rows);
  const SEXP* p_rows = VECTOR_PTR_RO(rows);

  for (R_xlen_t i = 0; i < ng; i++) {
    SEXP rows_i = p_rows[i];
    R_xlen_t n_i = Rf_xlength(rows_i);
    int *p_rows_i = INTEGER(rows_i);
    for (R_xlen_t j = 0; j < n_i; j++, ++p_rows_i) {
      p_indices[*p_rows_i - 1] = i + 1;
    }
  }
  Rf_unprotect(1);
  return indices;
}


// Address of R object

[[cpp11::register]]
SEXP cpp_r_obj_address(SEXP x) {
  static char buf[1000];
  snprintf(buf, 1000, "%p", (void*) x);
  SEXP out = Rf_protect(Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(out, 0, Rf_mkChar(buf));
  Rf_unprotect(1);
  return out;
}

// Quick check to see that addresses between 2 equal length lists are the same

[[cpp11::register]]
bool cpp_any_address_changed(SEXP x, SEXP y) {
  const SEXP* p_x = VECTOR_PTR_RO(x);
  const SEXP* p_y = VECTOR_PTR_RO(y);
  bool out = false;
  int n1 = Rf_length(x);
  int n2 = Rf_length(y);
  if (n1 != n2){
    Rf_error("x and y must be of the same length");
  }
  for (int i = 0; i < n1; ++i) {
    if (STRING_ELT(cpp_r_obj_address(p_x[i]), 0) != STRING_ELT(cpp_r_obj_address(p_y[i]), 0)){
      out = true;
      break;
    }
  }
  return out;
}

[[cpp11::register]]
SEXP cpp_lengths(SEXP x) {
  Rf_protect(x = Rf_coerceVector(x, VECSXP));
  int n = Rf_length(x);
  const SEXP* p_x = VECTOR_PTR_RO(x);
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  for (int i = 0; i < n; ++i) {
    p_out[i] = Rf_length(p_x[i]);
  }
  Rf_unprotect(2);
  return out;
}


// Credits to R authors
// Re-purposed .bincode
// The main difference is that codes or breaks can be returned efficiently
// Values outside the (right or left) intervals can be included too

[[cpp11::register]]
SEXP cpp_bin(SEXP x, SEXP breaks, bool codes, bool right, bool include_lowest,
             bool include_oob){
  int n = Rf_length(x);
  int lo;
  int hi;
  int nb = Rf_length(breaks);
  int nb1 = nb - 1;
  int cutpoint;
  bool left = !right;
  bool include_border = include_lowest;
  switch(TYPEOF(x)){
  case INTSXP: {
    if (codes){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
    Rf_protect(breaks = Rf_coerceVector(breaks, REALSXP));
    int *p_x = INTEGER(x);
    double *p_b = REAL(breaks);
    int *p_out = INTEGER(out);
    for (R_xlen_t i = 0; i < n; i++) {
      p_out[i] = NA_INTEGER;
      // If not NA
      if (p_x[i] != NA_INTEGER) {
        lo = 0;
        hi = nb1;
        if ( (include_oob && !include_border && (left ? p_x[i] == p_b[hi] : p_x[i] == p_b[lo])) ||
             ((include_oob && (left ? p_x[i] > p_b[hi] : p_x[i] < p_b[lo])))){
          p_out[i] = (left ? hi : lo) + 1;
        }
        else if (!(p_x[i] < p_b[lo] || p_x[i] > p_b[hi] ||
            (p_x[i] == p_b[left ? hi : lo] && !include_border))){
          while (hi - lo >= 2) {
            cutpoint = (hi + lo)/2;
            if (p_x[i] > p_b[cutpoint] || (left && p_x[i] == p_b[cutpoint]))
              lo = cutpoint;
            else
              hi = cutpoint;
          }
          p_out[i] = lo + 1;
        }
      }
    }
    Rf_unprotect(2);
    return out;
  } else {
    SEXP out = Rf_protect(Rf_duplicate(x));
    Rf_protect(breaks = Rf_coerceVector(breaks, REALSXP));
    int *p_x = INTEGER(x);
    double *p_b = REAL(breaks);
    int *p_out = INTEGER(out);
    for (R_xlen_t i = 0; i < n; i++) {
      p_out[i] = NA_INTEGER;
      // If not NA
      if (p_x[i] != NA_INTEGER) {
        lo = 0;
        hi = nb1;
        if ( (include_oob && !include_border && (left ? p_x[i] == p_b[hi] : p_x[i] == p_b[lo])) ||
             ((include_oob && (left ? p_x[i] > p_b[hi] : p_x[i] < p_b[lo])))){
          p_out[i] = p_b[(left ? hi : lo)];
        }
        else if (!(p_x[i] < p_b[lo] || p_x[i] > p_b[hi] ||
          (p_x[i] == p_b[left ? hi : lo] && !include_border))){
          while (hi - lo >= 2) {
            cutpoint = (hi + lo)/2;
            if (p_x[i] > p_b[cutpoint] || (left && p_x[i] == p_b[cutpoint]))
              lo = cutpoint;
            else
              hi = cutpoint;
          }
          p_out[i] = p_b[lo];
        }
      }
    }
    Rf_unprotect(2);
    return out;
  }
  }
  default: {
    if (codes){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
    Rf_protect(breaks = Rf_coerceVector(breaks, REALSXP));
    double *p_x = REAL(x);
    double *p_b = REAL(breaks);
    int *p_out = INTEGER(out);
    for (R_xlen_t i = 0; i < n; i++) {
      p_out[i] = NA_INTEGER;
      // If not NA
      if (p_x[i] == p_x[i]) {
        lo = 0;
        hi = nb1;
        if ( (include_oob && !include_border && (left ? p_x[i] == p_b[hi] : p_x[i] == p_b[lo])) ||
             ((include_oob && (left ? p_x[i] > p_b[hi] : p_x[i] < p_b[lo])))){
          p_out[i] = (left ? hi : lo) + 1;
        }
        else if (!(p_x[i] < p_b[lo] || p_x[i] > p_b[hi] ||
          (p_x[i] == p_b[left ? hi : lo] && !include_border))){
          while (hi - lo >= 2) {
            cutpoint = (hi + lo)/2;
            if (p_x[i] > p_b[cutpoint] || (left && p_x[i] == p_b[cutpoint]))
              lo = cutpoint;
            else
              hi = cutpoint;
          }
          p_out[i] = lo + 1;
        }
      }
    }
    Rf_unprotect(2);
    return out;
  } else {
    SEXP out = Rf_protect(Rf_duplicate(x));
    Rf_protect(breaks = Rf_coerceVector(breaks, REALSXP));
    double *p_x = REAL(x);
    double *p_b = REAL(breaks);
    double *p_out = REAL(out);
    for (R_xlen_t i = 0; i < n; i++) {
      p_out[i] = NA_REAL;
      // If not NA
      if (p_x[i] == p_x[i]) {
        lo = 0;
        hi = nb1;
        if ( (include_oob && !include_border && (left ? p_x[i] == p_b[hi] : p_x[i] == p_b[lo])) ||
             ((include_oob && (left ? p_x[i] > p_b[hi] : p_x[i] < p_b[lo])))){
          p_out[i] = p_b[(left ? hi : lo)];
        }
        else if (!(p_x[i] < p_b[lo] || p_x[i] > p_b[hi] ||
          (p_x[i] == p_b[left ? hi : lo] && !include_border))){
          while (hi - lo >= 2) {
            cutpoint = (hi + lo)/2;
            if (p_x[i] > p_b[cutpoint] || (left && p_x[i] == p_b[cutpoint]))
              lo = cutpoint;
            else
              hi = cutpoint;
          }
          p_out[i] = p_b[lo];
        }
      }
    }
    Rf_unprotect(2);
    return out;
  }
  }
  }
}

// This takes 2 lists, x containing a numeric vector
// And y containing the sorted breaks

[[cpp11::register]]
SEXP cpp_bin_grouped(SEXP x, SEXP y, bool codes, bool right, bool include_lowest,
                     bool include_oob) {
  Rf_protect(x = Rf_coerceVector(x, VECSXP));
  Rf_protect(y = Rf_coerceVector(y, VECSXP));
  const SEXP* p_x = VECTOR_PTR_RO(x);
  const SEXP* p_y = VECTOR_PTR_RO(y);
  int n1 = Rf_length(x);
  int n2 = Rf_length(y);
  if (n1 != n2){
    Rf_unprotect(2);
    Rf_error("x and y must be of the same length");
  }
  SEXP out = Rf_protect(Rf_allocVector(VECSXP, n1));
  for (int i = 0; i < n1; ++i) {
    SET_VECTOR_ELT(out, i, cpp_bin(p_x[i], p_y[i], codes, right,
                                   include_lowest,
                                   include_oob));
  }
  Rf_unprotect(3);
  return out;
}

// Subset one element from each list element
// List must contain elements of one type, e.g integer

[[cpp11::register]]
SEXP cpp_list_subset(SEXP x, SEXP ptype, SEXP i, SEXP default_value) {
  Rf_protect(x = Rf_coerceVector(x, VECSXP));
  Rf_protect(i = Rf_coerceVector(i, INTSXP));
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int n = Rf_length(x);
  int i_n = Rf_length(i);
  int k;
  if (n == 0){
    Rf_unprotect(2);
    return ptype;
  }
  if (Rf_length(ptype) > 0){
    Rf_unprotect(2);
    Rf_error("ptype must be a zero-length vector");
  }
  if (!(i_n == 1 || (n > 0 && i_n == n))){
    Rf_unprotect(2);
    Rf_error("i must be an integer vector of length 1 or of length(x)");
  }
  int *p_i = INTEGER(i);
  switch (TYPEOF(ptype)){
  case LGLSXP: {
    bool replace = Rf_asLogical(default_value);
    SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n));
    int *p_out = LOGICAL(out);
    for (int j = 0; j < n; ++j) {
      p_out[j] = replace;
      k = (i_n == 1 ? p_i[0] : p_i[j]);
      if (k <= Rf_length(p_x[j]) && k > 0){
        p_out[j] = LOGICAL(p_x[j])[k - 1];
      }
    }
    Rf_unprotect(3);
    return out;
  }
  case INTSXP: {
    int replace = Rf_asInteger(default_value);
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
    int *p_out = INTEGER(out);
    for (int j = 0; j < n; ++j) {
      p_out[j] = replace;
      k = (i_n == 1 ? p_i[0] : p_i[j]);
      if (k <= Rf_length(p_x[j]) && k > 0){
        p_out[j] = INTEGER(p_x[j])[k - 1];
      }
    }
    Rf_unprotect(3);
    return out;
  }
  case REALSXP: {
    double replace = Rf_asReal(default_value);
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, n));
    double *p_out = REAL(out);
    for (int j = 0; j < n; ++j) {
      p_out[j] = replace;
      k = (i_n == 1 ? p_i[0] : p_i[j]);
      if (k <= Rf_length(p_x[j]) && k > 0){
        p_out[j] = REAL(p_x[j])[k - 1];
      }
    }
    Rf_unprotect(3);
    return out;
  }
  case STRSXP: {
    SEXP replace = Rf_asChar(default_value);
    SEXP out = Rf_protect(Rf_allocVector(STRSXP, n));
    for (int j = 0; j < n; ++j) {
      SET_STRING_ELT(out, j, replace);
      k = (i_n == 1 ? p_i[0] : p_i[j]);
      if (k <= Rf_length(p_x[j]) && k > 0){
        SET_STRING_ELT(out, j, STRING_ELT(p_x[j], k - 1));
      }
    }
    Rf_unprotect(3);
    return out;
  }
  default: {
    Rf_unprotect(2);
    Rf_error("cpp_list_subset cannot handle supplied SEXP");
  }
  }
}

// Create a new list with a default value

[[cpp11::register]]
SEXP cpp_new_list(R_xlen_t size, SEXP default_value) {
  SEXP out = Rf_protect(Rf_allocVector(VECSXP, size));
  for (R_xlen_t i = 0; i < size; ++i) {
    SET_VECTOR_ELT(out, i, default_value);
  }
  Rf_unprotect(1);
  return out;
}

// Checks that all row indices of 2 grouped data frames are the same

// bool cpp_group_data_rows_equal(SEXP rows1, SEXP rows2) {
//   bool out = true;
//   int ng1 = Rf_length(rows1);
//   int ng2 = Rf_length(rows2);
//   if (ng1 != ng2){
//     return false;
//   }
//   const SEXP *p_rows1 = VECTOR_PTR_RO(rows1);
//   const SEXP *p_rows2 = VECTOR_PTR_RO(rows2);
//   for (int i = 0; i < ng1; ++i){
//     SEXP rows1_i = p_rows1[i];
//     SEXP rows2_i = p_rows2[i];
//     int n1 = Rf_length(rows1_i);
//     int n2 = Rf_length(rows2_i);
//     if (n1 != n2){
//       out = false;
//     }
//     if (out == false){
//       break;
//     }
//     int *p_rows1_i = INTEGER(rows1_i);
//     int *p_rows2_i = INTEGER(rows2_i);
//     for (int j = 0; j < n1; ++j){
//       if (p_rows1_i[j] != p_rows2_i[j]){
//         out = false;
//         break;
//       }
//     }
//   }
//   return out;
// }
