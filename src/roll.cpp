#include "timeplyr.h"

[[cpp11::register]]
SEXP cpp_roll_count_na(SEXP x, double window, bool invert, bool partial){
  R_xlen_t n = Rf_xlength(x);
  if (!(window >= 1)){
    Rf_error("window must be >= 1");
  }
  int k = window > n ? n : window;
  int count = 0;
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  switch ( TYPEOF(x) ){
  case NILSXP: {
    break;
  }
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    if (partial) {
      for (int i = 0; i < k - 1; ++i){
        count += (p_x[i] == NA_INTEGER);
        p_out[i] = invert ? i - count + 1 : count;
      }
    } else {
      for (int i = 0; i < k - 1; ++i){
        count += (p_x[i] == NA_INTEGER);
        p_out[i] = NA_INTEGER;
      }
    }
    count += (p_x[k - 1] == NA_INTEGER);
    p_out[k - 1] = invert ? k - count : count;
    for (R_xlen_t i = k; i < n; ++i){
      count -= (p_x[i - k] == NA_INTEGER);
      count += (p_x[i] == NA_INTEGER);
      p_out[i] = invert ? k - count : count;
    }
    break;
  }
  case REALSXP: {
    double *p_x = REAL(x);
    if (partial) {
      for (int i = 0; i < k - 1; ++i){
        count += (p_x[i] != p_x[i]);
        p_out[i] = invert ? i - count + 1 : count;
      }
    } else {
      for (int i = 0; i < k - 1; ++i){
        count += (p_x[i] != p_x[i]);
        p_out[i] = NA_INTEGER;
      }
    }
    count += (p_x[k - 1] != p_x[k - 1]);
    p_out[k - 1] = invert ? k - count : count;
    for (R_xlen_t i = k; i < n; ++i){
      count -= (p_x[i - k] != p_x[i - k]);
      count += (p_x[i] != p_x[i]);
      p_out[i] = invert ? k - count : count;
    }
    break;
  }
  case STRSXP: {
    const SEXP *p_x = STRING_PTR_RO(x);
    if (partial){
      for (int i = 0; i < k - 1; ++i){
        count += (p_x[i] == NA_STRING);
        p_out[i] = invert ? i - count + 1 : count;
      }
    } else {
      for (int i = 0; i < k - 1; ++i){
        count += (p_x[i] == NA_STRING);
        p_out[i] = NA_INTEGER;
      }
    }
    count += (p_x[k - 1] == NA_STRING);
    p_out[k - 1] = invert ? k - count : count;
    for (R_xlen_t i = k; i < n; ++i){
      count -= (p_x[i - k] == NA_STRING);
      count += (p_x[i] == NA_STRING);
      p_out[i] = invert ? k - count : count;
    }
    break;
  }
  case RAWSXP: {
    memset(p_out, 0, n * sizeof(int));
    break;
  }
  case CPLXSXP: {
    Rcomplex *p_x = COMPLEX(x);
    if (partial){
      for (int i = 0; i < k - 1; ++i){
        count += ( (p_x[i].r != p_x[i].r) || (p_x[i].i != p_x[i].i));
        p_out[i] = invert ? i - count + 1 : count;
      }
    } else {
      for (int i = 0; i < k - 1; ++i){
        count += ( (p_x[i].r != p_x[i].r) || (p_x[i].i != p_x[i].i) );
        p_out[i] = NA_INTEGER;
      }
    }
    count += ( (p_x[k - 1].r != p_x[k - 1].r) || (p_x[k - 1].i != p_x[k - 1].i) );
    p_out[k - 1] = invert ? k - count : count;
    for (R_xlen_t i = k; i < n; ++i){
      count -= ((p_x[i - k].r != p_x[i - k].r) || (p_x[i - k].i != p_x[i - k].i));
      count += ((p_x[i].r != p_x[i].r) || (p_x[i].i != p_x[i].i));
      p_out[i] = invert ? k - count : count;
    }
    break;
  }
  default: {
    Rf_unprotect(1);
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
  }
  }
  Rf_unprotect(1);
  return out;
}

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
  // int do_parallel = n >= 100000;
  // Here we perform the growth rate either on log(x) or on x
  if (log){
// #pragma omp parallel for simd if(do_parallel) num_threads(num_cores())
    for (R_xlen_t i = 0; i < n; ++i){
      if (p_lag[i] == NA_INTEGER){
        p_out[i] = NA_REAL;
      } else if (p_lag[i] == 0){
        p_out[i] = 1.0;
      } else {
        p_out[i] = std::exp( ( std::log(p_x[i]) - std::log(p_x[i - p_lag[i]]) ) / p_lag[i]);
      }
    }
  } else {
// #pragma omp parallel for simd if(do_parallel) num_threads(num_cores())
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

// Internal function
// x must be a numeric or integer
// threshold must be a numeric or integer of length 1
// This basically takes as input a cumulatively increasing time vector,
// and once a threshold is reached
// This is flagged a 1, the threshold is reset at that time value, and
// a new flag will occur once this new threshold is reached, and so on.
// Again, x must be a cumulatively increasing vector.

[[cpp11::register]]
SEXP cpp_roll_time_threshold(SEXP x, double threshold, bool switch_on_boundary) {
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

[[cpp11::register]]
SEXP cpp_which_first_gap(SEXP x, int increment, bool left_to_right) {
  int n = Rf_length(x);
  int *p_x = INTEGER(x);
  bool no_gap = true;
  int gap_loc;
  if (left_to_right){
    for (int i = 0; i < (n - 1); ++i){
      if ((p_x[i + 1] - p_x[i]) > increment){
        gap_loc = i + 1;
        no_gap = false;
        break;
      }
    }
  } else {
    for (int i = (n - 1); i > 0; --i){
      if ((p_x[i] - p_x[i - 1]) > increment){
        gap_loc = i + 1;
        no_gap = false;
        break;
      }
    }
  }
  if (no_gap){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 0));
    Rf_unprotect(1);
    return out;
  } else {
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 1));
    int *p_out = INTEGER(out);
    p_out[0] = gap_loc;
    Rf_unprotect(1);
    return out;
  }
}
