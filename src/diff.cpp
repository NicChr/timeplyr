#include "timeplyr_cpp.h"

// Integer difference accounting for overflow
int int_diff(int x, int y){
  int out;
  if (x == NA_INTEGER || y == NA_INTEGER){
    out = NA_INTEGER;
  } else if (x >= 0 && (y < (r_integer_min_ + x))){
    out = NA_INTEGER;
  } else if (x < 0 && (y > (r_integer_max_ + x))){
    out = NA_INTEGER;
  } else {
    out = y - x;
  }
  return out;
}

int lgl_diff(int x, int y){
  if (x == NA_INTEGER || y == NA_INTEGER){
    return NA_INTEGER;
  } else {
    return y - x;
  }
}

[[cpp11::register]]
SEXP cpp_diff(SEXP x, SEXP lag, SEXP order, SEXP run_lengths, SEXP fill, int differences){
  R_xlen_t size = Rf_xlength(x);
  R_xlen_t o_size = Rf_length(order); // order must be int vector
  R_xlen_t rl_size = Rf_xlength(run_lengths);
  R_xlen_t lag_size = Rf_xlength(lag);
  R_xlen_t fill_size = Rf_xlength(fill);
  int n_prot = 0;
  if (fill_size > 1){
    Rf_error("fill size must be NULL or length 1");
  }
  if (lag_size < 1){
    Rf_error("lag must be a non-zero length integer vector");
  }
  if (differences < 1){
    Rf_error("differences must be a positive integer >= 1");
  }
  bool has_order = !Rf_isNull(order);
  bool has_rl = !Rf_isNull(run_lengths);
  bool recycle_lag = lag_size != 1;

  // When order is NULL we run through x from left to right (as usual)
  // When run_lengths is NULL we run through x without resetting
  // To do this properly we use dummy vectors so that
  // we can statically assign int pointers
  // While keeping order and run_lengths as NULL (if they are NULL)
  // This is mainly done this way because cpp_diff is recursive and
  // hence order/run_lengths should remain NULL throughout each recursion
  // if they are NULL

  SEXP dummy_vec1 = Rf_protect(Rf_allocVector(INTSXP, 0));
  ++n_prot;
  SEXP dummy_vec2 = Rf_protect(Rf_allocVector(INTSXP, 0));
  ++n_prot;
  Rf_protect(lag = Rf_coerceVector(lag, INTSXP));
  ++n_prot;
  Rf_protect(order = has_order ? Rf_coerceVector(order, INTSXP) : R_NilValue);
  ++n_prot;
  Rf_protect(run_lengths = has_rl ? Rf_coerceVector(run_lengths, INTSXP) : R_NilValue);
  ++n_prot;

  int *p_o = INTEGER(has_order ? order : dummy_vec1);
  int *p_rl = INTEGER(has_rl ? run_lengths : dummy_vec2);
  int *p_lag = INTEGER(lag);
  R_xlen_t rl; // Run-length
  R_xlen_t run_start = 0; // Start index of current run
  R_xlen_t run_end = 0; // End index of current run
  R_xlen_t oi; // Indices (specified by order vector) to lag
  R_xlen_t k; // Lag
  R_xlen_t lag1 = p_lag[0];
  // Manually set run rl size to 1 if run_lengths = NULL (checked prior)
  if (!has_rl) rl_size = 1;
  // Initialise range of possible order values to fast check
  // user-supplied order values
  unsigned int o_rng = o_size - 1;
  SEXP out;
  switch(TYPEOF(x)){
  case NILSXP: {
    out = Rf_protect(R_NilValue);
    ++n_prot;
    break;
  }
  case LGLSXP: {
    if (has_order && (size != o_size)){
    Rf_error("length(order) must equal length(x)");
  }
    int *p_x = LOGICAL(x);
    int fill_value = NA_INTEGER;
    if (fill_size >= 1){
      fill_value = Rf_asInteger(fill);
    }
    out = Rf_protect(Rf_allocVector(INTSXP, size));
    ++n_prot;
    int *p_out = INTEGER(out);
    for (R_xlen_t i = 0; i != rl_size; ++i){
      run_start = run_end; // Start at the end of the previous run
      // Manually set run rl if order = NULL
      rl = has_rl ? p_rl[i] : size; // Current run-length
      run_end += rl; // Cumulative run-length

      // If any run-lengths are negative (or NA) we stop
      if (rl < 0){
        Rf_unprotect(n_prot);
        Rf_error("All run lengths must be non-NA and >= 0");
      }

      // If the cumulative run-length exceeds length(x) we stop
      if (run_end > size){
        Rf_unprotect(n_prot);
        Rf_error("sum(run_lengths) must be equal to length(x)");
      }
      // Loop starting from the end of the previous run-length
      for (R_xlen_t j = run_start; j != run_end; ++j){
        // Check that order value is valid
        if (has_order){
          oi = p_o[j] - 1;
          if (!( ( (unsigned) oi ) <= o_rng)){
            Rf_unprotect(n_prot);
            Rf_error("order must be an integer vector of unique values between 1 and length(x)");
          }
        } else {
          oi = j;
        }
        // Costly to use % if we don't need to
        k = recycle_lag ? p_lag[j % lag_size] : lag1;
        if ( (k >= 0 && ((j - run_start) >= k) ) ||
             ( k < 0 && ((j - run_end) < k))){
          p_out[oi] = lgl_diff(p_x[has_order ? p_o[j - k] - 1 : j - k], p_x[oi]);
        } else {
          p_out[oi] = fill_value;
        }
      }
    }
    if (run_end != size){
      Rf_unprotect(n_prot);
      Rf_error("sum(run_lengths) must be equal to length(x)");
    }
    if (differences >= 2){
      // Essentially.. If k is a scalar integer and not an integer vector
      // We can do a much more efficient updating by reference algorithm
      // Otherwise we use the less efficient but code-simpler recursion
      if (recycle_lag){
        for (int l = 1; l < differences; ++l){
          out = Rf_protect(cpp_diff(out, lag, order, run_lengths, fill, 1));
          ++n_prot;
        }
      } else {
        k = lag1;
        if (std::abs(k) >= 1){
          R_xlen_t tempi;
          int tempv;
          SEXP lag_temp = Rf_protect(Rf_allocVector(INTSXP, std::abs(k)));
          ++n_prot;
          int* __restrict__ p_lag = INTEGER(lag_temp);
          if (k >= 0){
            for (int l = 1; l < differences; ++l){
              run_end = 0; // Reset run_end;
              for (R_xlen_t i = 0; i != rl_size; ++i){
                run_start = run_end;
                rl = has_rl ? p_rl[i] : size;
                run_end += rl;
                for (R_xlen_t j = run_start; j != run_end; ++j){
                  oi = has_order ? p_o[j] - 1 : j;
                  if ((j - run_start) >= k){
                    tempi = ((j - k - run_start) % k);
                    tempv = p_lag[tempi];
                    p_lag[tempi] = p_out[oi];
                    p_out[oi] = lgl_diff(tempv, p_out[oi]);
                  } else {
                    p_lag[j - run_start] = p_out[oi];
                    p_out[oi] = fill_value;
                  }
                }
              }
            }
          } else {
            for (int l = 1; l < differences; ++l){
              run_end = 0; // Reset run_end;
              for (R_xlen_t i = 0; i != rl_size; ++i){
                run_start = run_end;
                rl = has_rl ? p_rl[i] : size;
                run_end += rl;
                for (R_xlen_t j = run_end - 1; j >= run_end + k; --j){
                  oi = has_order ? p_o[j] - 1 : j;
                  p_lag[run_end - j - 1] = p_out[oi];
                  p_out[oi] = fill_value;
                }
                for (R_xlen_t j = run_end + k - 1; j >= run_start; --j) {
                  oi = has_order ? p_o[j] - 1 : j;
                  tempi = ( (run_end - (j - k) - 1) % k);
                  tempv = p_lag[tempi];
                  p_lag[tempi] = p_out[oi];
                  p_out[oi] = lgl_diff(tempv, p_out[oi]);
                }
              }
            }
          }
        }
      }
    }
    cpp_copy_names(x, out);
    break;
  }
  case INTSXP: {
    if (has_order && (size != o_size)){
    Rf_error("length(order) must equal length(x)");
  }
    int *p_x = INTEGER(x);
    int fill_value = NA_INTEGER;
    if (fill_size >= 1){
      fill_value = Rf_asInteger(fill);
    }
    out = Rf_protect(Rf_allocVector(INTSXP, size));
    ++n_prot;
    int *p_out = INTEGER(out);
    for (R_xlen_t i = 0; i != rl_size; ++i){
      run_start = run_end; // Start at the end of the previous run
      // Manually set run rl if order = NULL
      rl = has_rl ? p_rl[i] : size; // Current run-length
      run_end += rl; // Cumulative run-length

      // If any run-lengths are negative (or NA) we stop
      if (rl < 0){
        Rf_unprotect(n_prot);
        Rf_error("All run lengths must be non-NA and >= 0");
      }

      // If the cumulative run-length exceeds length(x) we stop
      if (run_end > size){
        Rf_unprotect(n_prot);
        Rf_error("sum(run_lengths) must be equal to length(x)");
      }
      // Loop starting from the end of the previous run-length
      for (R_xlen_t j = run_start; j != run_end; ++j){
        // Check that order value is valid
        if (has_order){
          oi = p_o[j] - 1;
          if (!( ( (unsigned) oi ) <= o_rng)){
            Rf_unprotect(n_prot);
            Rf_error("order must be an integer vector of unique values between 1 and length(x)");
          }
        } else {
          oi = j;
        }
        // Costly to use % if we don't need to
        k = recycle_lag ? p_lag[j % lag_size] : lag1;
        if ( (k >= 0 && ((j - run_start) >= k) ) ||
             ( k < 0 && ((j - run_end) < k))){
          p_out[oi] = int_diff(p_x[has_order ? p_o[j - k] - 1 : j - k], p_x[oi]);
        } else {
          p_out[oi] = fill_value;
        }
      }
    }
    if (run_end != size){
      Rf_unprotect(n_prot);
      Rf_error("sum(run_lengths) must be equal to length(x)");
    }
    if (differences >= 2){
      // Essentially.. If k is a scalar integer and not an integer vector
      // We can do a much more efficient updating by reference algorithm
      // Otherwise we use the less efficient but code-simpler recursion
      if (recycle_lag){
        for (int l = 1; l < differences; ++l){
          out = Rf_protect(cpp_diff(out, lag, order, run_lengths, fill, 1));
          ++n_prot;
        }
      } else {
        k = lag1;
        if (std::abs(k) >= 1){
          R_xlen_t tempi;
          int tempv;
          SEXP lag_temp = Rf_protect(Rf_allocVector(INTSXP, std::abs(k)));
          ++n_prot;
          int* __restrict__ p_lag = INTEGER(lag_temp);
          if (k >= 0){
            for (int l = 1; l < differences; ++l){
              run_end = 0; // Reset run_end;
              for (R_xlen_t i = 0; i != rl_size; ++i){
                run_start = run_end;
                rl = has_rl ? p_rl[i] : size;
                run_end += rl;
                for (R_xlen_t j = run_start; j != run_end; ++j){
                  oi = has_order ? p_o[j] - 1 : j;
                  if ((j - run_start) >= k){
                    tempi = ((j - k - run_start) % k);
                    tempv = p_lag[tempi];
                    p_lag[tempi] = p_out[oi];
                    p_out[oi] = int_diff(tempv, p_out[oi]);
                  } else {
                    p_lag[j - run_start] = p_out[oi];
                    p_out[oi] = fill_value;
                  }
                }
              }
            }
          } else {
            for (int l = 1; l < differences; ++l){
              run_end = 0; // Reset run_end;
              for (R_xlen_t i = 0; i != rl_size; ++i){
                run_start = run_end;
                rl = has_rl ? p_rl[i] : size;
                run_end += rl;
                for (R_xlen_t j = run_end - 1; j >= run_end + k; --j){
                  oi = has_order ? p_o[j] - 1 : j;
                  p_lag[run_end - j - 1] = p_out[oi];
                  p_out[oi] = fill_value;
                }
                for (R_xlen_t j = run_end + k - 1; j >= run_start; --j) {
                  oi = has_order ? p_o[j] - 1 : j;
                  tempi = ( (run_end - (j - k) - 1) % k);
                  tempv = p_lag[tempi];
                  p_lag[tempi] = p_out[oi];
                  p_out[oi] = int_diff(tempv, p_out[oi]);
                }
              }
            }
          }
        }
      }
    }
    cpp_copy_names(x, out);
    break;
  }
  case REALSXP: {
    if (has_order && (size != o_size)){
    Rf_error("length(order) must equal length(x)");
  }
    double *p_x = REAL(x);
    double fill_value = NA_REAL;
    if (fill_size >= 1){
      fill_value = Rf_asReal(fill);
    }
    out = Rf_protect(Rf_allocVector(REALSXP, size));
    ++n_prot;
    double *p_out = REAL(out);
    for (R_xlen_t i = 0; i != rl_size; ++i){
      run_start = run_end; // Start at the end of the previous run
      // Manually set run rl if order = NULL
      rl = has_rl ? p_rl[i] : size; // Current run-length
      run_end += rl; // Cumulative run-length

      // If any run-lengths are negative (or NA) we stop
      if (rl < 0){
        Rf_unprotect(n_prot);
        Rf_error("All run lengths must be non-NA and >= 0");
      }

      // If the cumulative run-length exceeds length(x) we stop
      if (run_end > size){
        Rf_unprotect(n_prot);
        Rf_error("sum(run_lengths) must be equal to length(x)");
      }
      // Loop starting from the end of the previous run-length
      for (R_xlen_t j = run_start; j != run_end; ++j){
        // Check that order value is valid
        if (has_order){
          oi = p_o[j] - 1;
          if (!( ( (unsigned) oi ) <= o_rng)){
            Rf_unprotect(n_prot);
            Rf_error("order must be an integer vector of unique values between 1 and length(x)");
          }
        } else {
          oi = j;
        }
        // Costly to use % if we don't need to
        k = recycle_lag ? p_lag[j % lag_size] : lag1;
        if ( (k >= 0 && ((j - run_start) >= k) ) ||
             ( k < 0 && ((j - run_end) < k))){
          p_out[oi] = p_x[oi] - p_x[has_order ? p_o[j - k] - 1 : j - k];
        } else {
          p_out[oi] = fill_value;
        }
      }
    }
    if (run_end != size){
      Rf_unprotect(n_prot);
      Rf_error("sum(run_lengths) must be equal to length(x)");
    }
    if (differences >= 2){
      // Essentially.. If k is a scalar integer and not an integer vector
      // We can do a much more efficient updating by reference algorithm
      // Otherwise we use the less efficient but code-simpler recursion
      if (recycle_lag){
        for (int l = 1; l < differences; ++l){
          out = Rf_protect(cpp_diff(out, lag, order, run_lengths, fill, 1));
          ++n_prot;
        }
      } else {
        k = lag1;
        if (std::abs(k) >= 1){
          R_xlen_t tempi;
          double tempv;
          SEXP lag_temp = Rf_protect(Rf_allocVector(REALSXP, std::abs(k)));
          ++n_prot;
          double* __restrict__ p_lag = REAL(lag_temp);
          if (k >= 0){
            for (int l = 1; l < differences; ++l){
              run_end = 0; // Reset run_end;
              for (R_xlen_t i = 0; i != rl_size; ++i){
                run_start = run_end;
                rl = has_rl ? p_rl[i] : size;
                run_end += rl;
                for (R_xlen_t j = run_start; j != run_end; ++j){
                  oi = has_order ? p_o[j] - 1 : j;
                  if ((j - run_start) >= k){
                    tempi = ((j - k - run_start) % k);
                    tempv = p_lag[tempi];
                    p_lag[tempi] = p_out[oi];
                    p_out[oi] = p_out[oi] - tempv;
                  } else {
                    p_lag[j - run_start] = p_out[oi];
                    p_out[oi] = fill_value;
                  }
                }
              }
            }
          } else {
            for (int l = 1; l < differences; ++l){
              run_end = 0; // Reset run_end;
              for (R_xlen_t i = 0; i != rl_size; ++i){
                run_start = run_end;
                rl = has_rl ? p_rl[i] : size;
                run_end += rl;
                for (R_xlen_t j = run_end - 1; j >= run_end + k; --j){
                  oi = has_order ? p_o[j] - 1 : j;
                  p_lag[run_end - j - 1] = p_out[oi];
                  p_out[oi] = fill_value;
                }
                for (R_xlen_t j = run_end + k - 1; j >= run_start; --j) {
                  oi = has_order ? p_o[j] - 1 : j;
                  tempi = ( (run_end - (j - k) - 1) % k);
                  tempv = p_lag[tempi];
                  p_lag[tempi] = p_out[oi];
                  p_out[oi] = p_out[oi] - tempv;
                }
              }
            }
          }
        }
      }
    }
    cpp_copy_names(x, out);
    break;
  }
  case CPLXSXP: {
    Rcomplex x_val;
    Rcomplex x_lag;
    if (has_order && (size != o_size)){
      Rf_error("length(order) must equal length(x)");
    }
    Rcomplex *p_x = COMPLEX(x);
    SEXP fill_sexp = Rf_protect(Rf_allocVector(CPLXSXP, 1));
    ++n_prot;
    Rcomplex *p_fill = COMPLEX(fill_sexp);
    p_fill[0].i = NA_REAL;
    p_fill[0].r = NA_REAL;
    Rcomplex fill_value = fill_size >= 1 ? Rf_asComplex(fill) : COMPLEX(fill_sexp)[0];
    out = Rf_protect(Rf_allocVector(CPLXSXP, size));
    ++n_prot;
    Rcomplex *p_out = COMPLEX(out);
    for (R_xlen_t i = 0; i != rl_size; ++i){
      run_start = run_end; // Start at the end of the previous run
      // Manually set run rl if order = NULL
      rl = has_rl ? p_rl[i] : size; // Current run-length
      run_end += rl; // Cumulative run-length

      // If any run-lengths are negative (or NA) we stop
      if (rl < 0){
        Rf_unprotect(n_prot);
        Rf_error("All run lengths must be non-NA and >= 0");
      }

      // If the cumulative run-length exceeds length(x) we stop
      if (run_end > size){
        Rf_unprotect(n_prot);
        Rf_error("sum(run_lengths) must be equal to length(x)");
      }
      // Loop starting from the end of the previous run-length
      for (R_xlen_t j = run_start; j != run_end; ++j){
        // Check that order value is valid
        if (has_order){
          oi = p_o[j] - 1;
          if (!( ( (unsigned) oi ) <= o_rng)){
            Rf_unprotect(n_prot);
            Rf_error("order must be an integer vector of unique values between 1 and length(x)");
          }
        } else {
          oi = j;
        }
        // Costly to use % if we don't need to
        k = recycle_lag ? p_lag[j % lag_size] : lag1;
        if (k >= 0){
          if ((j - run_start) >= k){
            x_lag = p_x[has_order ? p_o[j - k] - 1 : j - k];
            x_val = p_x[has_order ? p_o[j] - 1 : j];
            p_out[oi].r = x_val.r - x_lag.r;
            p_out[oi].i = x_val.i - x_lag.i;
          } else {
            SET_COMPLEX_ELT(out, oi, fill_value);
          }
        } else {
          if ((j - run_end) < k){
            x_lag = p_x[has_order ? p_o[j - k] - 1 : j - k];
            x_val = p_x[has_order ? p_o[j] - 1 : j];
            p_out[oi].r = x_val.r - x_lag.r;
            p_out[oi].i = x_val.i - x_lag.i;
          } else {
            SET_COMPLEX_ELT(out, oi, fill_value);
          }
        }
      }
    }
    if (run_end != size){
      Rf_unprotect(n_prot);
      Rf_error("sum(run_lengths) must be equal to length(x)");
    }
    if (differences >= 2){
      for (int l = 1; l < differences; ++l){
        out = Rf_protect(cpp_diff(out, lag, order, run_lengths, fill, 1));
        ++n_prot;
      }
    }
    cpp_copy_names(x, out);
    break;
  }
  case VECSXP: {
    const SEXP *p_x = VECTOR_PTR_RO(x);
    out = Rf_protect(Rf_allocVector(VECSXP, size));
    ++n_prot;
    SHALLOW_DUPLICATE_ATTRIB(out, x);
    for (R_xlen_t i = 0; i < size; ++i){
      SET_VECTOR_ELT(out, i, cpp_diff(p_x[i], lag, order, run_lengths, fill, differences));
    }
    break;
  }
  default: {
    Rf_unprotect(n_prot);
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
  }
  }
  Rf_unprotect(n_prot);
  return out;
}

