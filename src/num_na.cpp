#include <cpp11.hpp>
#include <Rinternals.h>

#define R_NO_REMAP

[[cpp11::register]]
SEXP cpp_num_na(SEXP x){
  R_xlen_t n = Rf_xlength(x);
  R_xlen_t count = 0;
  // This nicely handles NULL and avoids loop too
  if (n == 0){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 1));
    int *p_out = INTEGER(out);
    int n_na = 0;
    p_out[0] = n_na;
    Rf_unprotect(1);
    return out;
  }
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    for (R_xlen_t i = 0; i < n; i++){
      count += (p_x[i] == NA_INTEGER);
    }
    break;
  }
  case REALSXP: {
    double *p_x = REAL(x);
    for (R_xlen_t i = 0; i < n; i++){
      // Because NaN == NaN is false
      count += !(p_x[i] == p_x[i]);
    }
    break;
  }
  case STRSXP: {
    SEXP *p_x = STRING_PTR(x);
    for (R_xlen_t i = 0; i < n; i++){
      count += (p_x[i] == NA_STRING);
    }
    break;
  }
  case RAWSXP: {
    break;
  }
  case CPLXSXP: {
    Rcomplex *p_x = COMPLEX(x);
    for (R_xlen_t i = 0; i < n; i++){
      count +=
        ( !((p_x[i]).r == (p_x[i]).r) ) ||
        ( !((p_x[i]).i == (p_x[i]).i) );
  }
    break;
  }
  default: {
    Rf_error("cpp_num_na cannot handle the supplied SEXP");
    break;
  }
  }
  if (count <= std::numeric_limits<int>::max()){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 1));
    int *p_out = INTEGER(out);
    p_out[0] = int(count);
    Rf_unprotect(1);
    return out;
  } else {
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, 1));
    double *p_out = REAL(out);
    p_out[0] = double(count);
    Rf_unprotect(1);
    return out;
  }
}
