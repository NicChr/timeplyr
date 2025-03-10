#include "timeplyr.h"

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
