#include "timeplyr_cpp.h"
#include <cpp11.hpp>
#include <Rinternals.h>

// A more memory-efficient which()
// Author: Nick Christofides

// Count the number of true values

R_xlen_t count_true(int *px, R_xlen_t n){
  if (n > std::numeric_limits<int>::max()){
    R_xlen_t size = 0;
    R_xlen_t N = n;
#pragma omp parallel for simd num_threads(num_cores()) reduction(+:size)
    for (R_xlen_t j = 0; j < N; ++j) size += (px[j] == TRUE);
    return size;
  } else if (n < 100000){
    int size = 0;
    int N = n;
    for (int j = 0; j < N; ++j) size += (px[j] == TRUE);
    return size;
  } else {
    int size = 0;
    int N = n;
#pragma omp parallel for simd num_threads(num_cores()) reduction(+:size)
    for (int j = 0; j < N; ++j) size += (px[j] == TRUE);
    return size;
  }
}

[[cpp11::register]]
SEXP cpp_which_(SEXP x, bool invert){
  R_xlen_t n = Rf_xlength(x);
  int *p_x = LOGICAL(x);
  bool is_long = (n > std::numeric_limits<int>::max());
  if (invert){
    if (is_long){
      R_xlen_t size = count_true(p_x, n);
      R_xlen_t out_size = n - size;
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, out_size));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < out_size){
        p_out[whichi] = i + 1;
        whichi += !(p_x[i] == TRUE);
        ++i;
      }
      Rf_unprotect(1);
      return out;
    } else {
      int size = count_true(p_x, n);
      int out_size = n - size;
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, out_size));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < out_size){
        p_out[whichi] = i + 1;
        whichi += !(p_x[i] == TRUE);
        ++i;
      }
      Rf_unprotect(1);
      return out;
    }
  } else {
    if (is_long){
      R_xlen_t size = count_true(p_x, n);
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, size));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < size){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == TRUE);
        ++i;
      }
      Rf_unprotect(1);
      return out;
    } else {
      int size = count_true(p_x, n);
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, size));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < size){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == TRUE);
        ++i;
      }
      Rf_unprotect(1);
      return out;
    }
  }
}
