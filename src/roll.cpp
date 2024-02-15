#include "timeplyr_cpp.h"
#include <cpp11.hpp>
#include <Rinternals.h>

[[cpp11::register]]
SEXP cpp_roll_growth_rate(SEXP x, SEXP lag, bool log){
  R_xlen_t n = Rf_xlength(x);
  R_xlen_t lag_n = Rf_xlength(lag);
  if (n != lag_n){
    Rf_error("length of lag must be equal to length of x in %s", __func__);
  }
  int *p_lag = INTEGER(lag);
  Rf_protect(x = Rf_coerceVector(x, REALSXP));
  SEXP out = Rf_protect(Rf_allocVector(REALSXP, n));
  double *p_x = REAL(x);
  double *p_out = REAL(out);
  int do_parallel = n >= 100000;
// #pragma omp parallel for num_threads(num_cores()) if(do_parallel)
// #pragma omp parallel for simd if(do_parallel) num_threads(num_cores())
  // Here we perform the growth rate either on log(x) or on x
  if (log){
#pragma omp parallel for simd if(do_parallel) num_threads(num_cores())
    for (R_xlen_t i = 0; i < n; ++i){
      if (p_lag[i] == NA_INTEGER){
        p_out[i] = NA_REAL;
      } else if (p_lag[i] == 0.0){
        p_out[i] = 1.0;
      } else {
        p_out[i] = std::exp( ( std::log(p_x[i]) - std::log(p_x[i - p_lag[i]]) ) / p_lag[i]);
      }
    }
  } else {
#pragma omp parallel for simd if(do_parallel) num_threads(num_cores())
    for (R_xlen_t i = 0; i < n; ++i){
      if (p_lag[i] == NA_INTEGER){
        p_out[i] = NA_REAL;
      } else if (p_x[i] == 0.0 && p_x[i - p_lag[i]] == 0.0){
        p_out[i] = 1.0;
      } else {
        p_out[i] = std::pow((p_x[i] / p_x[i - p_lag[i]]), (1.0 / p_lag[i]));
      }
    }
  }
  Rf_unprotect(2);
  return out;
}
