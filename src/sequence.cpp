#include <Rcpp.h>
using namespace Rcpp;

// R sum but result is always a double
double r_sum(SEXP x){
  double out = 0;
  // Rcpp::Environment base("package:base");
  // Rcpp::Function r_sum = base["sum"];
  Rcpp::Environment base(Rcpp::Environment::base_env());
  Rcpp::Function r_sum = base["sum"];
  SEXP size = PROTECT(r_sum(x));
  switch (TYPEOF(size)){
  case INTSXP: {
    int *p_size = INTEGER(size);
    if (Rf_length(size) > 0){
      out = p_size[0];
    }
    break;
  }
  case REALSXP: {
    double *p_size = REAL(size);
    if (Rf_length(size) > 0){
      out = p_size[0];
    }
    break;
  }
  default: {
    Rcpp::stop("r_sum not implemented for supplied SEXP");
  }
  }
  // NumericVector outv = Rcpp::as<Rcpp::NumericVector>(size);
  // Being extra careful, prob unnecessary
  // if (outv.length() > 0){
  //   out = outv[0];
  // }
  UNPROTECT(1);
  return (out);
}

// [[Rcpp::export(rng = false)]]
IntegerVector before_sequence(IntegerVector size, int k) {
  if (Rcpp::min(size) < 0){
    stop("size must be a vector of non-negative integers");
  }
  int size_n = size.length();
  k = std::max(k, 0);
  IntegerVector out(sum(size), k);
  int index = 0;
  for (int j = 0; j < size_n; ++j){
    for (int i = 0; i < size[j]; ++i){
      if (i < k){
        out[index] = i;
      }
      ++index;
    }
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
IntegerVector after_sequence(IntegerVector size, int k) {
  if (Rcpp::min(size) < 0){
    stop("size must be a vector of non-negative integers");
  }
  int size_n = size.length();
  k = std::max(k, 0);
  IntegerVector out(sum(size), k);
  int index = 0;
  int idiff;
  for (int j = 0; j < size_n; ++j){
    for (int i = 0; i < size[j]; ++i){
      idiff = size[j] - i - 1;
      if (idiff < k){
        out[index] = idiff;
      }
      ++index;
    }
  }
  return out;
}
// My version of base::sequence()
// SEXP cpp_int_sequence(SEXP size, SEXP from, SEXP by) {
//   int size_n = Rf_length(size);
//   int from_n = Rf_length(from);
//   int by_n = Rf_length(by);
//   if (from_n <= 0 || by_n <= 0){
//     Rcpp::stop("from and by must both have length >= 0");
//   }
//   SEXP out = PROTECT(Rf_allocVector(INTSXP, r_sum(size)));
//   int *p_out = INTEGER(out);
//   R_xlen_t index = 0;
//   // int sj;
//   int fj;
//   int bj;
//   int start;
//   int increment;
//   double seq_end;
//   if (size_n > 0){
//     int *p_size = INTEGER(size);
//     int *p_from = INTEGER(from);
//     int *p_by = INTEGER(by);
//     for (int j = 0; j < size_n; ++j){
//       if (p_size[j] < 0){
//         Rcpp::stop("size must be a vector of non-negative integers");
//       }
//       // sj = j % size_n;
//       fj = j % from_n;
//       bj = j % by_n;
//       // Throw error if integer overflow
//       seq_end = p_from[fj] + (p_by[bj] * (std::fmax(p_size[j] - 1, 0)));
//       if (seq_end > std::numeric_limits<int>::max()){
//         Rcpp::stop("Integer overflow");
//       }
//       start = p_from[fj];
//       increment = p_by[bj];
//       if (start == NA_INTEGER){
//         Rcpp::stop("from contains NA values");
//       }
//       if (increment == NA_INTEGER){
//         Rcpp::stop("by contains NA values");
//       }
//       for (int i = 0; i < p_size[j]; ++i){
//         p_out[index] = start;
//         start += increment;
//         ++index;
//       }
//     }
//   }
//   UNPROTECT(1);
//   return out;
// }
// [[Rcpp::export(rng = false)]]
SEXP cpp_dbl_sequence(SEXP size, SEXP from, SEXP by) {
  int size_n = Rf_length(size);
  int from_n = Rf_length(from);
  int by_n = Rf_length(by);
  if (size_n > 0 && (from_n <= 0 || by_n <= 0)){
    Rcpp::stop("from and by must both have length >= 0");
  }
  SEXP out = PROTECT(Rf_allocVector(REALSXP, r_sum(size)));
  double *p_out = REAL(out);
  R_xlen_t index = 0;
  // int sj;
  int fj;
  int bj;
  double start;
  double increment;
  if (size_n > 0){
    int *p_size = INTEGER(size);
    double *p_from = REAL(from);
    double *p_by = REAL(by);
    for (int j = 0; j < size_n; ++j){
      if (p_size[j] < 0){
        Rcpp::stop("size must be a vector of non-negative integers");
      }
      // sj = j % size_n;
      fj = j % from_n;
      bj = j % by_n;
      start = p_from[fj];
      increment = p_by[bj];
      if (!(start == start)){
        Rcpp::stop("from contains NA values");
      }
      if (!(increment == increment)){
        Rcpp::stop("by contains NA values");
      }
      for (int i = 0; i < p_size[j]; ++i){
        start = p_from[fj] + (i * increment);
        p_out[index] = start;
        // start += increment;
        ++index;
      }
    }
  }
  UNPROTECT(1);
  return out;
}
// SEXP cpp_sequence(IntegerVector size, NumericVector from, NumericVector by) {
//   int size_n = size.length();
//   int from_n = from.length();
//   int by_n = by.length();
//   if (from_n <= 0 || by_n <= 0){
//     Rcpp::stop("from and by must both have length >= 0");
//   }
//   if (size_n > 0 && Rcpp::min(size) < 0){
//     Rcpp::stop("size must be a vector of non-negative integers");
//   }
//   SEXP out = PROTECT(Rf_allocVector(REALSXP, Rcpp::sum(size)));
//   double *p_out = REAL(out);
//   R_xlen_t index = 0;
//   int fj;
//   int bj;
//   double start;
//   double increment;
//   if (size_n > 0){
//     for (int j = 0; j < size_n; ++j){
//       fj = j % from_n;
//       bj = j % by_n;
//       start = from[fj];
//       increment = by[bj];
//       if (!(start == start)){
//         Rcpp::stop("from contains NA values");
//       }
//       if (!(increment == increment)){
//         Rcpp::stop("by contains NA values");
//       }
//       for (int i = 0; i < size[j]; ++i){
//         p_out[index] = start;
//         start += increment;
//         ++index;
//       }
//     }
//   }
//   UNPROTECT(1);
//   return out;
// }
// [[Rcpp::export(rng = false)]]
IntegerVector window_sequence(IntegerVector size,
                              double k,
                              bool partial = true,
                              bool ascending = true) {
  int size_n = size.length();
  if (Rcpp::min(size) < 0){
    Rcpp::stop("size must be a vector of non-negative integers");
  }
  k = std::fmax(k, 0);
  // double out_len = r_sum(size);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, Rcpp::sum(size)));
  int *p_out = INTEGER(out);
  R_xlen_t index = 0;
  if (ascending){
    // right aligned window sequences
    if (partial){
      for (int j = 0; j < size_n; ++j){
        for (int i = 0; i < size[j]; ++i){
          if (i < k){
            p_out[index] = i + 1;
          } else {
            p_out[index] = k;
          }
          ++index;
        }
      }
    } else {
      for (int j = 0; j < size_n; ++j){
        for (int i = 0; i < size[j]; ++i){
          if (i < (k - 1)){
            p_out[index] = NA_INTEGER;
          } else {
            p_out[index] = k;
          }
          ++index;
        }
      }
    }
  } else {
    // left aligned window sequences
    int idiff;
    if (partial){
      for (int j = 0; j < size_n; ++j){
        for (int i = 0; i < size[j]; ++i){
          idiff = size[j] - i - 1;
          if (idiff < k){
            p_out[index] = idiff + 1;
          } else {
            p_out[index] = k;
          }
          ++index;
        }
      }
    } else {
      for (int j = 0; j < size_n; ++j){
        for (int i = 0; i < size[j]; ++i){
          idiff = size[j] - i - 1;
          if (idiff < (k - 1)){
            p_out[index] = NA_INTEGER;
          } else {
            p_out[index] = k;
          }
          ++index;
        }
      }
    }
  }
  UNPROTECT(1);
  return out;
}

// [[Rcpp::export(rng = false)]]
IntegerVector lag_sequence(IntegerVector size, int k) {
  if (Rcpp::min(size) < 0){
    stop("size must be a vector of non-negative integers");
  }
  int size_n = size.length();
  k = std::max(k, 0);
  IntegerVector out(sum(size), k);
  int index = 0;
  for (int j = 0; j < size_n; ++j){
    for (int i = 0; i < size[j]; ++i){
      if (i < k){
        out[index] = NA_INTEGER;
      }
      ++index;
    }
  }
  return out;
}
// [[Rcpp::export(rng = false)]]
IntegerVector lead_sequence(IntegerVector size, int k) {
  if (Rcpp::min(size) < 0){
    stop("size must be a vector of non-negative integers");
  }
  int size_n = size.length();
  k = std::max(k, 0);
  IntegerVector out(sum(size), k);
  int index = 0;
  int idiff;
  for (int j = 0; j < size_n; ++j){
    for (int i = 0; i < size[j]; ++i){
      idiff = size[j] - i - 1;
      if (idiff < k){
        out[index] = NA_INTEGER;
      }
      ++index;
    }
  }
  return out;
}
