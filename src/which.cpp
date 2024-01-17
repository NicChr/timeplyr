// #ifdef _OPENMP
//   #include <omp.h>
//   #define OMP_NUM_PROCS omp_get_num_procs()
//   #define OMP_THREAD_LIMIT omp_get_thread_limit()
//   #define OMP_MAX_THREADS omp_get_max_threads()
// #else
//   #define OMP_NUM_PROCS 1
//   #define OMP_THREAD_LIMIT 1
//   #define OMP_MAX_THREADS 1
// #endif

#include <cpp11.hpp>
#include <Rinternals.h>

#define R_NO_REMAP

// bool openmp_support() {
// #ifdef _OPENMP
//   return true;
// #else
//   return false;
// #endif
// }

// A simple and more efficient which()

[[cpp11::register]]
SEXP cpp_which_(SEXP x, bool invert) {
  R_xlen_t n = Rf_xlength(x);
  int *p_x = LOGICAL(x);
  bool is_long = (n > std::numeric_limits<int>::max());
  if (invert){
    if (is_long){
      R_xlen_t size = 0;
      for (R_xlen_t j = 0; j < n; ++j) size += (p_x[j] == TRUE);
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
      int size = 0;
      for (int j = 0; j < n; ++j) size += (p_x[j] == TRUE);
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
      R_xlen_t size = 0;
      for (R_xlen_t j = 0; j < n; ++j) size += (p_x[j] == TRUE);
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
      int size = 0;
      for (int j = 0; j < n; ++j) size += (p_x[j] == TRUE);
      // if (n < 10000){
      //   for (int j = 0; j < n; ++j) size += (p_x[j] == TRUE);
      // } else {
      //   // #pragma omp simd reduction(+:size)
      //   #pragma omp parallel for simd num_threads(openmp_cores()) reduction(+:size)
      //   for (int j = 0; j < n; ++j) size += (p_x[j] == TRUE);
      // }
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

