#include <Rcpp.h>

// Whole number check
// NA values are ignored by default

// [[Rcpp::export(rng = false)]]
SEXP cpp_is_whole_num(SEXP x, SEXP tol, bool na_rm = true) {
  R_xlen_t n = Rf_xlength(x);
  int tol_len = Rf_length(tol);
  if (tol_len > 1){
    Rcpp::stop("tol must be of length <= 1");
  }
  if (tol_len == 0){
    n = 0;
  }
  bool is_whole;
  double adiff;
  R_xlen_t n_na = 0;
  bool is_na;
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, 1));
  int *p_out = LOGICAL(out);
  p_out[0] = false;
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    p_out[0] = true;
    break;
  }
  case REALSXP: {
    // Re-initialise so that we can break when we find non-whole num
    p_out[0] = true;
    double *p_x = REAL(x);
    double *p_t = REAL(tol);
    for (R_xlen_t i = 0; i < n; ++i) {
      adiff = std::fabs(p_x[i] - std::round(p_x[i]));
      is_whole = (adiff < p_t[0]);
      is_na = !(p_x[i] == p_x[i]);
      n_na += is_na;
      if (!is_whole && !is_na){
        p_out[0] = false;
        break;
      }
    }
    if (!na_rm && n_na > 0){
      p_out[0] = NA_LOGICAL;
      break;
    }
    break;
  }
  }
  UNPROTECT(1);
  return out;
}

