#include "timeplyr_cpp.h"
#include <cpp11.hpp>
#include <Rinternals.h>

[[cpp11::register]]
SEXP cpp_na_col_counts(SEXP x){
  if (!Rf_isVectorList(x)){
    Rf_error("x must be a data frame");
  }
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int num_col = Rf_length(x);
  int n_protections = 0;
  R_xlen_t num_row = cpp_vector_size(x);
  SEXP n_empty = Rf_protect(Rf_allocVector(INTSXP, num_row));
  ++n_protections;
  int *p_n_empty = INTEGER(n_empty);
  memset(p_n_empty, 0, num_row * sizeof(int));
  int do_parallel = num_row >= 100000;
  int n_cores = do_parallel ? num_cores() : 1;
#pragma omp parallel num_threads(n_cores) if(do_parallel)
  for (int j = 0; j < num_col; ++j){
    switch ( TYPEOF(p_x[j]) ){
    case LGLSXP:
    case INTSXP: {
      int *p_xj = INTEGER(p_x[j]);
#pragma omp for simd
      for (R_xlen_t i = 0; i < num_row; ++i){
        p_n_empty[i] += (p_xj[i] == NA_INTEGER);
      }
      break;
    }
    case REALSXP: {
      double *p_xj = REAL(p_x[j]);
#pragma omp for simd
      for (R_xlen_t i = 0; i < num_row; ++i){
        p_n_empty[i] += (p_xj[i] != p_xj[i]);
      }
      break;
    }
    case STRSXP: {
      SEXP *p_xj = STRING_PTR(p_x[j]);
#pragma omp for simd
      for (R_xlen_t i = 0; i < num_row; ++i){
        p_n_empty[i] += (p_xj[i] == NA_STRING);
      }

      break;
    }
    case RAWSXP: {
      break;
    }
    case CPLXSXP: {
      Rcomplex *p_xj = COMPLEX(p_x[j]);
#pragma omp for simd
      for (R_xlen_t i = 0; i < num_row; ++i){
        p_n_empty[i] += (p_xj[i]).r != (p_xj[i]).r || (p_xj[i]).i != (p_xj[i]).i;
      }
      break;
    }
    case VECSXP: {
      SEXP n_empty_nested = Rf_protect(cpp_na_col_counts(VECTOR_ELT(x, j)));
      ++n_protections;
      int *p_n_empty_nested = INTEGER(n_empty_nested);
      for (R_xlen_t j = 0; j < num_row; ++j){
        p_n_empty[j] += (p_n_empty_nested[j] >= 1);
      }
      break;
    }
    default: {
      Rf_unprotect(n_protections);
      Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(p_x[j])));
    }
    }
  }
  Rf_unprotect(n_protections);
  return n_empty;
}

// Are rows empty for at least ncol (>= threshold)?

[[cpp11::register]]
SEXP cpp_missing_row(SEXP x, int threshold){
  if (!Rf_isVectorList(x)){
    Rf_error("x must be a data frame");
  }
  if (threshold == NA_INTEGER){
    Rf_error("threshold cannot be NA");
  }
  int n_cols = cpp_vector_width(x);
  int over_threshold;
  R_xlen_t n_rows = cpp_vector_size(x);
  if (threshold < 0){
    threshold = 0;
  }
  if (threshold > n_cols){
    threshold = n_cols;
  }
  // Special case when there are 0 cols
  if (n_cols == 0){
    SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n_rows));
    int *p_out = INTEGER(out);
    memset(p_out, 0, n_rows * sizeof(int));
    Rf_unprotect(1);
    return out;
    // All rows always have >= 0 empty values
  } else if (threshold <= 0){
    SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n_rows));
    int *p_out = INTEGER(out);
#pragma omp for simd
    for (R_xlen_t i = 0; i < n_rows; ++i){
      p_out[i] = 1;
    }
    Rf_unprotect(1);
    return out;
  } else {
    SEXP out = Rf_protect(cpp_na_col_counts(x));
    int *p_out = INTEGER(out);
#pragma omp for simd
    for (R_xlen_t i = 0; i < n_rows; ++i){
      over_threshold = (p_out[i] - threshold + 1) >= 1;
      p_out[i] = over_threshold;
    }
    SET_TYPEOF(out, LGLSXP);
    Rf_unprotect(1);
    return out;
  }
}
// SEXP cpp_missing_row(SEXP x, double prop){
//   if (prop != prop){
//     Rf_error("prop cannot be NA");
//   }
//   int n_cols = cpp_vector_width(x);
//   int over_threshold;
//   R_xlen_t n_rows = cpp_vector_size(x);
//   int threshold;
//   if (prop <= 0){
//     threshold = 0;
//   } else if (prop >= 1){
//     threshold = n_cols;
//   } else {
//     threshold = std::floor( (prop * n_cols) + 0.0000000001);
//   }
//   // Special case when there are 0 cols
//   if (n_cols == 0){
//     SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n_rows));
//     int *p_out = INTEGER(out);
//     memset(p_out, 0, n_rows * sizeof(int));
//     Rf_unprotect(1);
//     return out;
//     // All rows always have >= 0 empty values
//   } else if (threshold == 0.0 || prop == 0.0){
//     SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n_rows));
//     int *p_out = INTEGER(out);
// #pragma omp for simd
//     for (R_xlen_t i = 0; i < n_rows; ++i){
//       p_out[i] = true;
//     }
//     Rf_unprotect(1);
//     return out;
//   } else {
//     SEXP out = Rf_protect(cpp_na_col_counts(x));
//     int *p_out = INTEGER(out);
// #pragma omp for simd
//     for (R_xlen_t i = 0; i < n_rows; ++i){
//       over_threshold = p_out[i] / threshold;
//       p_out[i] = over_threshold;
//     }
//     // SET_TYPEOF(out, LGLSXP);
//     Rf_unprotect(1);
//     return out;
//   }
// }
// SEXP cpp_missing_row(SEXP x, double prop){
//   // if (!(prop >= 0.0 && prop <= 1.0)){
//   //   Rf_error("prop must be in the range [0,1]");
//   // }
//   if (prop != prop){
//     Rf_error("prop cannot be NA");
//   }
//   int n_cols = cpp_vector_width(x);
//   R_xlen_t n_rows = cpp_vector_size(x);
//   int threshold;
//   if (prop <= 0){
//     threshold = 0;
//   } else if (prop >= 1){
//     threshold = n_cols;
//   } else {
//     threshold = std::floor(prop * n_cols);
//   }
//   if (n_cols == 0){
//     SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n_rows));
//     int *p_out = INTEGER(out);
//     memset(p_out, 0, n_rows * sizeof(int));
//     Rf_unprotect(1);
//     return out;
//   } else if (threshold == n_cols){
//     return cpp_empty_row(x);
//   } else {
//     SEXP counts = Rf_protect(cpp_na_col_counts(x));
//     int *p_counts = INTEGER(counts);
//     SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n_rows));
//     int *p_out = LOGICAL(out);
// #pragma omp parallel num_threads(num_cores()) if(n_rows >= 100000)
//     for (R_xlen_t i = 0; i < n_rows; ++i){
//       p_out[i] = p_counts[i] >= threshold;
//     }
//     Rf_unprotect(2);
//     return out;
//   }
// }
// SEXP cpp_missing_row(SEXP x, int n_cols){
//   if (n_cols == Rf_length(x)){
//     return cpp_empty_row(x);
//   } else {
//     R_xlen_t n = cpp_vector_size(x);
//     SEXP counts = Rf_protect(cpp_na_col_counts(x));
//     int *p_counts = INTEGER(counts);
//     SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n));
//     int *p_out = LOGICAL(out);
// #pragma omp parallel num_threads(num_cores()) if(n >= 100000)
//     for (R_xlen_t i = 0; i < n; ++i){
//       p_out[i] = p_counts[i] >= n_cols;
//     }
//     Rf_unprotect(2);
//     return out;
//   }
// }

[[cpp11::register]]
SEXP cpp_num_na(SEXP x){
  R_xlen_t n = Rf_xlength(x);
  R_xlen_t count = 0;
  int n_protections = 0;
  // This nicely handles NULL and avoids loop too
  if (n == 0){
    return Rf_ScalarInteger(count);
  }
  bool do_parallel = n >= 100000;
  int n_cores = do_parallel ? num_cores() : 1;
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    if (do_parallel){
      #pragma omp parallel for simd num_threads(n_cores) reduction(+:count)
      // #pragma omp parallel for simd num_threads(Rf_asInteger(cpp11::package("base")["getOption"]("timeplyr.cores", 1))) reduction(+:count)
      for (R_xlen_t i = 0; i < n; ++i){
        count = count + (p_x[i] == NA_INTEGER);
      }
    } else {
      for (R_xlen_t i = 0; i < n; ++i){
        count = count + (p_x[i] == NA_INTEGER);
      }
    }
    break;
  }
  case REALSXP: {
    double *p_x = REAL(x);
    if (do_parallel){
      #pragma omp parallel for simd num_threads(n_cores) reduction(+:count)
      for (R_xlen_t i = 0; i < n; ++i){
        count = count + (p_x[i] != p_x[i]);
      }
    } else {
      for (R_xlen_t i = 0; i < n; ++i){
        count = count + (p_x[i] != p_x[i]);
      }
    }
    break;
  }
  case STRSXP: {
    SEXP *p_x = STRING_PTR(x);
    if (do_parallel){
      #pragma omp parallel for simd num_threads(n_cores) reduction(+:count)
      for (R_xlen_t i = 0; i < n; ++i){
        count = count + (p_x[i] == NA_STRING);
      }
    } else {
      for (R_xlen_t i = 0; i < n; ++i){
        count = count + (p_x[i] == NA_STRING);
      }
    }
    break;
  }
  case RAWSXP: {
    break;
  }
  case CPLXSXP: {
    Rcomplex *p_x = COMPLEX(x);
    if (do_parallel){
      #pragma omp parallel for simd num_threads(n_cores) reduction(+:count)
      for (R_xlen_t i = 0; i < n; ++i){
        count = count + ( ((p_x[i]).r != (p_x[i]).r) || ((p_x[i]).i != (p_x[i]).i) );
      }
    } else {
      for (R_xlen_t i = 0; i < n; ++i){
        count = count + ( ((p_x[i]).r != (p_x[i]).r) || ((p_x[i]).i != (p_x[i]).i) );
      }
    }
    break;
  }
  case VECSXP: {
    R_xlen_t num_row = cpp_vector_size(x);
    SEXP is_empty = Rf_protect(cpp_missing_row(x, cpp_vector_width(x)));
    // SEXP is_empty = Rf_protect(cpp_empty_row(x));
    ++n_protections;
    int *p_is_empty = LOGICAL(is_empty);
    count = count_true(p_is_empty, num_row);
//     SEXP empty_col_counts = Rf_protect(cpp_na_col_counts(x));
//     int *p_empty_col_counts = INTEGER(empty_col_counts);
//     ++n_protections;
// #pragma omp parallel for simd if(do_parallel) num_threads(n_cores)
//     for (R_xlen_t i = 0; i < num_row; ++i){
//       count += p_empty_col_counts[i] >= n_cols;
//     }
    break;
  }
  default: {
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
    break;
  }
  }
  if (count <= std::numeric_limits<int>::max()){
    Rf_unprotect(n_protections);
    return Rf_ScalarInteger(count);
  } else {
    Rf_unprotect(n_protections);
    return Rf_ScalarReal(count);
  }
}

// Memory-efficient which(is.na(x))

[[cpp11::register]]
SEXP cpp_which_na(SEXP x){
  R_xlen_t n = cpp_vector_size(x);
  bool is_short = (n <= std::numeric_limits<int>::max());
  if (n == 0){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 0));
    Rf_unprotect(1);
    return out;
  }
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    R_xlen_t count = Rf_asReal(Rf_protect(cpp_num_na(x)));
    int *p_x = INTEGER(x);
    if (is_short){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, count));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == NA_INTEGER);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, count));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == NA_INTEGER);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  case REALSXP: {
    R_xlen_t count = Rf_asReal(Rf_protect(cpp_num_na(x)));
    double *p_x = REAL(x);
    if (is_short){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, count));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] != p_x[i]);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, count));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] != p_x[i]);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  case STRSXP: {
    R_xlen_t count = Rf_asReal(Rf_protect(cpp_num_na(x)));
    SEXP *p_x = STRING_PTR(x);
    if (is_short){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, count));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == NA_STRING);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, count));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == NA_STRING);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  case RAWSXP: {
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 1));
    Rf_unprotect(1);
    return out;
  }
  case CPLXSXP: {
    R_xlen_t count = Rf_asReal(Rf_protect(cpp_num_na(x)));
    Rcomplex *p_x = COMPLEX(x);
    if (is_short){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, count));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += ( ( !((p_x[i]).r == (p_x[i]).r) ) || ( !((p_x[i]).i == (p_x[i]).i) ) );
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, count));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += ( ( !((p_x[i]).r == (p_x[i]).r) ) || ( !((p_x[i]).i == (p_x[i]).i) ) );
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  case VECSXP: {
    SEXP is_empty = Rf_protect(cpp_missing_row(x, cpp_vector_width(x)));
    // SEXP is_empty = Rf_protect(cpp_empty_row(x));
    SEXP out = Rf_protect(cpp_which_(is_empty, false));
    Rf_unprotect(2);
    return out;
  }
  default: {
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
    break;
  }
  }
}

[[cpp11::register]]
SEXP cpp_which_not_na(SEXP x){
  R_xlen_t n = cpp_vector_size(x);
  bool is_short = (n <= std::numeric_limits<int>::max());
  if (n == 0){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 0));
    Rf_unprotect(1);
    return out;
  }
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    R_xlen_t count = Rf_asReal(Rf_protect(cpp_num_na(x)));
    int out_size = n - count;
    int *p_x = INTEGER(x);
    if (is_short){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, out_size));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < out_size){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] != NA_INTEGER);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, out_size));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < out_size){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] != NA_INTEGER);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  case REALSXP: {
    R_xlen_t count = Rf_asReal(Rf_protect(cpp_num_na(x)));
    int out_size = n - count;
    double *p_x = REAL(x);
    if (is_short){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, out_size));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < out_size){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == p_x[i]);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, out_size));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < out_size){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == p_x[i]);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  case STRSXP: {
    R_xlen_t count = Rf_asReal(Rf_protect(cpp_num_na(x)));
    int out_size = n - count;
    SEXP *p_x = STRING_PTR(x);
    if (is_short){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, out_size));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < out_size){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] != NA_STRING);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, out_size));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < out_size){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] != NA_STRING);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  case RAWSXP: {
    if (is_short){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
    int *p_out = INTEGER(out);
    for (int i = 0; i < n; ++i){
      p_out[i] = i + 1;
    }
    Rf_unprotect(1);
    return out;
  } else {
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, n));
    double *p_out = REAL(out);
    for (R_xlen_t i = 0; i < n; ++i){
      p_out[i] = i + 1;
    }
    Rf_unprotect(1);
    return out;
  }
  }
  case CPLXSXP: {
    R_xlen_t count = Rf_asReal(Rf_protect(cpp_num_na(x)));
    int out_size = n - count;
    Rcomplex *p_x = COMPLEX(x);
    if (is_short){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, out_size));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < out_size){
        p_out[whichi] = i + 1;
        whichi += ( ( ((p_x[i]).r == (p_x[i]).r) ) && ( ((p_x[i]).i == (p_x[i]).i) ) );
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, out_size));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < out_size){
        p_out[whichi] = i + 1;
        whichi += ( ( ((p_x[i]).r == (p_x[i]).r) ) && ( ((p_x[i]).i == (p_x[i]).i) ) );
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  case VECSXP: {
    SEXP is_empty = Rf_protect(cpp_missing_row(x, cpp_vector_width(x)));
    // SEXP is_empty = Rf_protect(cpp_empty_row(x));
    SEXP out = Rf_protect(cpp_which_(is_empty, true));
    Rf_unprotect(2);
    return out;
  }
  default: {
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
    break;
  }
  }
}
