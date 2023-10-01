#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List list_rm_null(List l) {
  int n = l.size();
  LogicalVector keep(n);
  for (int i = 0; i < n; ++i) {
    keep[i] = !Rf_isNull(l[i]);
  }
  return l[keep];
}

// [[Rcpp::export]]
CharacterVector numeric_class(NumericVector x){
  CharacterVector out(1);
  if (x.hasAttribute("class")){
    out = wrap(x.attr("class"));
  }
  return out;
}

// [[Rcpp::export]]
bool list_has_interval( List l ) {
  bool out = false;
  std::string int_str = "Interval";
  for( List::iterator it = l.begin(); it != l.end(); ++it ) {
    switch( TYPEOF(*it) ) {
    case REALSXP: {
      NumericVector tmp = Rcpp::as<NumericVector>(*it);
      CharacterVector x_class = numeric_class(tmp);
      if (x_class[0] == int_str){
        out = true;
      }
      break;
    }
    }
    if (out){
      break;
    }
  }
  return out;
}
// [[Rcpp::export]]
LogicalVector list_item_is_interval( List l ) {
  int n = l.size();
  LogicalVector out(n);
  std::string int_str = "Interval";
  for (int i = 0; i < n; ++i) {
    switch( TYPEOF(l[i]) ) {
    case REALSXP: {
      NumericVector tmp = Rcpp::as<NumericVector>(l[i]);
      CharacterVector x_class = numeric_class(tmp);
      out[i] = (x_class[0] == int_str);
      break;
    }
    }
  }
  return out;
}

// template <class T>
// T my_rep(T &x, int times){
//   return Rcpp::rep(x, times);
// }
//
// SEXP my_rep (SEXP &x, int times)
// {
//   switch (TYPEOF (x))
//   {
//   case INTSXP: {
//     Rcpp::IntegerVector iv = Rcpp::as <Rcpp::IntegerVector> (x);
//     return my_rep(iv, times);
//   }
//   case REALSXP: {
//     Rcpp::NumericVector nv = Rcpp::as <Rcpp::NumericVector> (x);
//     return my_rep(nv, times);
//   }
//   default: {
//     Rcpp::stop("error");
//   }
//   }
//   return x; // this never happens
// }
// SEXP rcpp_rep(SEXP &x, int times){
//   return my_rep(x, times);
// }

// [[Rcpp::export]]
List rcpp_cj( List X ) {
  int nargs = X.size();
  if (nargs <= 1){
    return X;
  }
  Rcpp::Environment base("package:base");
  Rcpp::Function r_lengths = base["lengths"];
  // Rcpp::Function r_prod = base["prod"];
  Rcpp::Function r_list_of_empty_vectors("list_of_empty_vectors");
  IntegerVector d = r_lengths(X);
  int nx;
  double orep = 1;
  for (int j = 0; j < nargs; j++){
    orep = orep * d[j];
  }
  // double orep = Rcpp::as<double>(r_prod(d));
  if (orep == 0){
    return(r_list_of_empty_vectors(X));
  }
  List out(nargs);
  int rep_fac = 1;
  for (int i = nargs - 1; i >= 0; i--){
      switch( TYPEOF(X[i]) ) {
      case REALSXP: {
        NumericVector x = Rcpp::as<Rcpp::NumericVector>(X[i]);
        nx = d[i];
        orep = orep / nx;
        x = x[Rcpp::rep(Rcpp::rep_each(Rcpp::seq_len(nx), rep_fac), orep) - 1];
        rep_fac = rep_fac * nx;
        out[i] = x;
        break;
      }
      case INTSXP: {
        IntegerVector x = Rcpp::as<Rcpp::IntegerVector>(X[i]);
        nx = d[i];
        orep = orep / nx;
        x = x[Rcpp::rep(Rcpp::rep_each(Rcpp::seq_len(nx), rep_fac), orep) - 1];
        rep_fac = rep_fac * nx;
        out[i] = x;
        break;
      }
      case STRSXP: {
        CharacterVector x = Rcpp::as<Rcpp::CharacterVector>(X[i]);
        nx = d[i];
        orep = orep / nx;
        x = x[Rcpp::rep(Rcpp::rep_each(Rcpp::seq_len(nx), rep_fac), orep) - 1];
        rep_fac = rep_fac * nx;
        out[i] = x;
        break;
      }
      default: {
        Rcpp::stop("Can only cross-join integer, numeric and character vectors");
      }
      }
    }
  return out;
}
