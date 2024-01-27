#include "timeplyr_cpp.h"
#include <cpp11.hpp>
#include <Rinternals.h>

SEXP cpp_empty_row(SEXP x){
  if (!Rf_isVectorList(x)){
    Rf_error("x must be a data frame");
  }
  const SEXP *p_x = VECTOR_PTR_RO(x);
  int num_col = Rf_length(x);
  int n_protections = 0;
  R_xlen_t num_row = cpp_vector_size(x);
  // Check that list elements are all equal in length
  // and all atomic vectors and not NULL
  for (int j = 0; j < num_col; ++j){
    if (!Rf_isVectorAtomic(p_x[j])){
      Rf_error("All list elements must be atomic vectors");
    }
  }
  SEXP is_empty = Rf_protect(Rf_allocVector(LGLSXP, num_row));
  ++n_protections;
  int *p_is_empty = LOGICAL(is_empty);
  memset(p_is_empty, 0, num_row * sizeof(int));
  for (int j = 0; j < num_col; ++j){
    switch ( TYPEOF(p_x[j]) ){
    case LGLSXP:
    case INTSXP: {
      int *p_xj = INTEGER(p_x[j]);
      if (j == 0){
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_is_empty[i] = (p_xj[i] == NA_INTEGER);
        }
      } else {
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_is_empty[i] = p_is_empty[i] && (p_xj[i] == NA_INTEGER);
        }
      }
      break;
    }
    case REALSXP: {
      double *p_xj = REAL(p_x[j]);
      if (j == 0){
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_is_empty[i] = (p_xj[i] != p_xj[i]);
        }
      } else {
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_is_empty[i] = p_is_empty[i] && (p_xj[i] != p_xj[i]);
        }
      }
      break;
    }
    case STRSXP: {
      SEXP *p_xj = STRING_PTR(p_x[j]);
      if (j == 0){
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_is_empty[i] = (p_xj[i] == NA_STRING);
        }
      } else {
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_is_empty[i] = p_is_empty[i] && (p_xj[i] == NA_STRING);
        }
      }

      break;
    }
    case RAWSXP: {
      break;
    }
    case CPLXSXP: {
      Rcomplex *p_xj = COMPLEX(p_x[j]);
      if (j == 0){
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_is_empty[i] = (
            ( ((p_xj[i]).r != (p_xj[i]).r) ) ||
              ( ((p_xj[i]).i != (p_xj[i]).i) )
          );
        }
      } else {
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_is_empty[i] = p_is_empty[i] && (
            ( ((p_xj[i]).r != (p_xj[i]).r) ) ||
              ( ((p_xj[i]).i != (p_xj[i]).i) )
          );
        }
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
  return is_empty;
}

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
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    if (do_parallel){
      #pragma omp parallel for simd num_threads(num_cores()) reduction(+:count)
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
      #pragma omp parallel for simd num_threads(num_cores()) reduction(+:count)
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
      #pragma omp parallel for simd num_threads(num_cores()) reduction(+:count)
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
      #pragma omp parallel for simd num_threads(num_cores()) reduction(+:count)
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
    // It would be simpler to use sum(cpp_empty_row())
    // As shown below..
    // Though we can optimise on this counting the number of empty rows
    // at the same time as checking the last col for NAs
    // Start
    // R_xlen_t num_row = cpp_vector_size(x);
    // SEXP is_empty = Rf_protect(cpp_empty_row(x));
    // int *p_is_empty = LOGICAL(is_empty);
    // ++n_protections;
    // count = count_true(p_is_empty, num_row);
    // End
    const SEXP *p_x = VECTOR_PTR_RO(x);
    int num_col = Rf_length(x);
    R_xlen_t num_row = cpp_vector_size(x);
    // R_xlen_t num_row = 0;
    // if (num_col > 0){
    //   num_row = Rf_xlength(p_x[0]);
    // }
    // Check that list elements are all equal in length
    // and all atomic vectors and not NULL
    for (int j = 0; j < num_col; ++j){
      if (!Rf_isVectorAtomic(p_x[j])){
        Rf_error("All list elements must be atomic vectors");
      }
      // if (Rf_xlength(p_x[j]) != num_row){
      //   Rf_error("All list elements must be of equal length");
      // }
    }
    SEXP n_nas = Rf_protect(Rf_allocVector(INTSXP, num_row));
    ++n_protections;
    int *p_n_nas = INTEGER(n_nas);
    memset(p_n_nas, 0, num_row * sizeof(int));
    for (int j = 0; j < (num_col - 1) ; ++j){
      switch ( TYPEOF(p_x[j]) ){
      case LGLSXP:
      case INTSXP: {
        int *p_xj = INTEGER(p_x[j]);
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_INTEGER);
        }
        break;
      }
      case REALSXP: {
        double *p_xj = REAL(p_x[j]);
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_n_nas[i] = p_n_nas[i] + !(p_xj[i] == p_xj[i]);
        }
        break;
      }
      case STRSXP: {
        SEXP *p_xj = STRING_PTR(p_x[j]);
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_STRING);
        }
        break;
      }
      case RAWSXP: {
        break;
      }
      case CPLXSXP: {
        Rcomplex *p_xj = COMPLEX(p_x[j]);
#pragma omp parallel for simd num_threads(num_cores())
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_n_nas[i] = p_n_nas[i] + ( ((p_xj[i]).r != (p_xj[i]).r) || ((p_xj[i]).i != (p_xj[i]).i) );
        }
        break;
      }
      }
    }
    switch ( TYPEOF(p_x[num_col - 1]) ){
    case LGLSXP:
    case INTSXP: {
      int *p_xj = INTEGER(p_x[num_col - 1]);
#pragma omp parallel for simd num_threads(num_cores()) reduction(+:count)
      for (R_xlen_t i = 0; i < num_row; ++i){
        p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_INTEGER);
        count = count + (p_n_nas[i] == num_col);
      }
      break;
    }
    case REALSXP: {
      double *p_xj = REAL(p_x[num_col - 1]);
#pragma omp parallel for simd num_threads(num_cores()) reduction(+:count)
      for (R_xlen_t i = 0; i < num_row; ++i){
        p_n_nas[i] = p_n_nas[i] + !(p_xj[i] == p_xj[i]);
        count = count + (p_n_nas[i] == num_col);
      }
      break;
    }
    case STRSXP: {
      SEXP *p_xj = STRING_PTR(p_x[num_col - 1]);
#pragma omp parallel for simd num_threads(num_cores()) reduction(+:count)
      for (R_xlen_t i = 0; i < num_row; ++i){
        p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_STRING);
        count = count + (p_n_nas[i] == num_col);
      }
      break;
    }
    case RAWSXP: {
      break;
    }
    case CPLXSXP: {
      Rcomplex *p_xj = COMPLEX(p_x[num_col - 1]);
#pragma omp parallel for simd num_threads(num_cores()) reduction(+:count)
      for (R_xlen_t i = 0; i < num_row; ++i){
        p_n_nas[i] = p_n_nas[i] + ( ((p_xj[i]).r != (p_xj[i]).r) || ((p_xj[i]).i != (p_xj[i]).i) );
        count = count + (p_n_nas[i] == num_col);
      }
      break;
    }
    }
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
  R_xlen_t n = Rf_xlength(x);
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
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 0));
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
    SEXP is_empty = Rf_protect(cpp_empty_row(x));
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
    SEXP is_empty = Rf_protect(cpp_empty_row(x));
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
