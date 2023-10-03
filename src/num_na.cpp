#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int num_na(SEXP x){
  int count = 0;
  switch ( TYPEOF(x) ){
  case LGLSXP: {
    Rcpp::LogicalVector xv = Rcpp::as<Rcpp::LogicalVector>(x);
    for (int i = 0; i < xv.size(); i++){
      count += LogicalVector::is_na(xv[i]);
    }
    break;
  }
  case INTSXP: {
    Rcpp::IntegerVector xv = Rcpp::as<Rcpp::IntegerVector>(x);
    for (int i = 0; i < xv.size(); i++){
      count += IntegerVector::is_na(xv[i]);
    }
    break;
  }
  case REALSXP: {
    Rcpp::NumericVector xv = Rcpp::as<Rcpp::NumericVector>(x);
    for (int i = 0; i < xv.size(); i++){
      count += NumericVector::is_na(xv[i]);
    }
    break;
  }
  case STRSXP: {
    Rcpp::CharacterVector xv = Rcpp::as<Rcpp::CharacterVector>(x);
    for (int i = 0; i < xv.size(); i++){
      count += CharacterVector::is_na(xv[i]);
    }
    break;
  }
  case RAWSXP: {
    Rcpp::RawVector xv = Rcpp::as<Rcpp::RawVector>(x);
    for (int i = 0; i < xv.size(); i++){
      count += RawVector::is_na(xv[i]);
    }
    break;
  }
  case CPLXSXP: {
    Rcpp::ComplexVector xv = Rcpp::as<Rcpp::ComplexVector>(x);
    for (int i = 0; i < xv.size(); i++){
      count += ComplexVector::is_na(xv[i]);
    }
    break;
  }
  default: {
    stop("Unknown type");
  }
  }
  return count;
}
