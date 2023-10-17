#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
int cpp_num_na(SEXP x){
  int count = 0;
  int n = Rf_length(x);
  // This nicely handles NULL and avoids loop too
  if (n == 0){
    return count;
  }
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    int *p_count = INTEGER(x);
    for (int i = 0; i < n; i++){
      count += (p_count[i] == NA_INTEGER);
    }
    break;
  }
  case REALSXP: {
    double *p_count = REAL(x);
    for (int i = 0; i < n; i++){
      // Because NaN == NaN is false
      count += !(p_count[i] == p_count[i]);
    }
    break;
  }
  case STRSXP: {
    SEXP *p_count = STRING_PTR(x);
    for (int i = 0; i < n; i++){
      count += (p_count[i] == NA_STRING);
    }
    break;
  }
  case RAWSXP: {
    Rcpp::RawVector xv = Rcpp::as<Rcpp::RawVector>(x);
    for (int i = 0; i < n; i++){
      count += RawVector::is_na(xv[i]);
    }
    break;
  }
  case CPLXSXP: {
    Rcpp::ComplexVector xv = Rcpp::as<Rcpp::ComplexVector>(x);
    for (int i = 0; i < n; i++){
      count += ComplexVector::is_na(xv[i]);
    }
    break;
  }
  default: {
    stop("num_na cannot handle the supplied SEXP");
  }
  }
  return count;
}
