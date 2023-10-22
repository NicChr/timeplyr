#include <Rcpp.h>
#include <Rinternals.h>
// int test_long_vector_support() {
// #ifdef RCPP_HAS_LONG_LONG_TYPES
//   return 1;
// #else
//   return 0;
// #endif
// }

// [[Rcpp::export(rng = false)]]
SEXP cpp_num_na(SEXP x){
  R_xlen_t n = Rf_xlength(x);
  R_xlen_t count = 0;
  // This nicely handles NULL and avoids loop too
  if (n == 0){
    SEXP out = PROTECT(Rf_allocVector(INTSXP, 1));
    int *p_out = INTEGER(out);
    int n_na = 0;
    p_out[0] = n_na;
    UNPROTECT(1);
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
    Rcpp::ComplexVector xv = Rcpp::as<Rcpp::ComplexVector>(x);
    for (R_xlen_t i = 0; i < n; i++){
      count += Rcpp::ComplexVector::is_na(xv[i]);
    }
    break;
  }
  default: {
    Rcpp::stop("num_na cannot handle the supplied SEXP");
  }
  }
  if (count <= std::numeric_limits<int>::max()){
    SEXP out = PROTECT(Rf_allocVector(INTSXP, 1));
    int *p_out = INTEGER(out);
    p_out[0] = int(count);
    UNPROTECT(1);
    return out;
  } else {
    SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));
    double *p_out = REAL(out);
    p_out[0] = double(count);
    UNPROTECT(1);
    return out;
  }
}
