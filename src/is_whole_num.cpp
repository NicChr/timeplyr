#include "timeplyr.h"

// Returns true if all numbers are whole numbers
// otherwise false
// Returns NA when na_rm is false and the function can't find any
// non-whole numbers and there is at least 1 NA

[[cpp11::register]]
SEXP cpp_is_whole_num(SEXP x, SEXP tol_, SEXP na_rm_) {
  if (Rf_xlength(tol_) != 1){
    Rf_error("`tol` must be a length 1 double vector");
  }
  if (Rf_xlength(na_rm_) != 1){
    Rf_error("`na.rm` must be a length 1 logical vector");
  }

  Rf_protect(tol_ = Rf_coerceVector(tol_, REALSXP));
  Rf_protect(na_rm_ = Rf_coerceVector(na_rm_, LGLSXP));

  double tol = REAL(tol_)[0];
  bool na_rm = LOGICAL(na_rm_)[0];

  R_xlen_t n = Rf_xlength(x);
  bool is_whole;
  double adiff;
  R_xlen_t n_na = 0;
  bool is_na;
  SEXP out = Rf_protect(Rf_allocVector(LGLSXP, 1));
  LOGICAL(out)[0] = false;
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    LOGICAL(out)[0] = true;
    break;
  }
  case REALSXP: {
    // Re-initialise so that we can break when we find non-whole num
    LOGICAL(out)[0] = true;
    double *p_x = REAL(x);
    for (R_xlen_t i = 0; i < n; ++i) {
      adiff = std::fabs(p_x[i] - std::round(p_x[i]));
      is_whole = (adiff < tol);
      is_na = p_x[i] != p_x[i];
      n_na += is_na;
      if (!is_whole && !is_na){
        LOGICAL(out)[0] = false;
        break;
      }
    }
    if (!na_rm && n_na > 0){
      LOGICAL(out)[0] = NA_LOGICAL;
      break;
    }
    break;
  }
  default: {
    break;
  }
  }
  Rf_unprotect(3);
  return out;
}
