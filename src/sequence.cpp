#include <Rcpp.h>
using namespace Rcpp;


// R sum but result is always a double
double r_sum(SEXP x){
  double out = 0;
  // Rcpp::Environment base("package:base");
  // Rcpp::Function r_sum = base["sum"];
  // Rcpp::Environment base(Rcpp::Environment::base_env());
  Rcpp::Function base_sum = Rcpp::Environment::base_env()["sum"];
  SEXP sum = PROTECT(base_sum(x));
  switch (TYPEOF(sum)){
  case INTSXP: {
    int *p_sum = INTEGER(sum);
    if (Rf_length(sum) > 0){
      out = p_sum[0];
    }
    break;
  }
  case REALSXP: {
    double *p_sum = REAL(sum);
    if (Rf_length(sum) > 0){
      out = p_sum[0];
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
IntegerVector before_sequence(IntegerVector size, double k) {
  if (Rcpp::min(size) < 0){
    stop("size must be a vector of non-negative integers");
  }
  int size_n = size.length();
  k = std::fmax(k, 0);
  IntegerVector out(r_sum(size));
  R_xlen_t index = 0;
  for (int j = 0; j < size_n; ++j){
    for (int i = 0; i < size[j]; ++i){
      if (i < k){
        out[index] = i;
      } else {
        out[index] = k;
      }
      ++index;
    }
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
IntegerVector after_sequence(IntegerVector size, double k) {
  if (Rcpp::min(size) < 0){
    stop("size must be a vector of non-negative integers");
  }
  int size_n = size.length();
  k = std::fmax(k, 0);
  IntegerVector out(r_sum(size));
  R_xlen_t index = 0;
  int idiff;
  for (int j = 0; j < size_n; ++j){
    for (int i = 0; i < size[j]; ++i){
      idiff = size[j] - i - 1;
      if (idiff < k){
        out[index] = idiff;
      } else {
        out[index] = k;
      }
      ++index;
    }
  }
  return out;
}

// My version of base::sequence()
// [[Rcpp::export(rng = false)]]
SEXP cpp_int_sequence(SEXP size, SEXP from, SEXP by) {
  int size_n = Rf_length(size);
  int from_n = Rf_length(from);
  int by_n = Rf_length(by);
  if (from_n <= 0 || by_n <= 0){
    Rcpp::stop("from and by must both have length >= 0");
  }
  SEXP out = PROTECT(Rf_allocVector(INTSXP, r_sum(size)));
  int *p_out = INTEGER(out);
  R_xlen_t index = 0;
  // int sj;
  int fj;
  int bj;
  int start;
  int increment;
  int seq_size;
  double seq_end;
  if (size_n > 0){
    int *p_size = INTEGER(size);
    int *p_from = INTEGER(from);
    int *p_by = INTEGER(by);
    for (int j = 0; j < size_n; ++j){
      seq_size = p_size[j];
      if (seq_size < 0){
        Rcpp::stop("size must be a vector of non-negative integers");
      }
      // sj = j % size_n;
      fj = j % from_n;
      bj = j % by_n;
      start = p_from[fj];
      increment = p_by[bj];
      // Throw error if integer overflow
      seq_end = start + (increment * (std::fmax(seq_size - 1, 0)));
      if (seq_end > std::numeric_limits<int>::max()){
        Rcpp::stop("Integer overflow value of %f in sequence %f", seq_end, j + 1);
      }
      if (start == NA_INTEGER){
        Rcpp::stop("from contains NA values");
      }
      if (increment == NA_INTEGER){
        Rcpp::stop("by contains NA values");
      }
      for (int i = 0; i < seq_size; ++i){
        p_out[index] = start;
        start += increment;
        ++index;
      }
    }
  }
  UNPROTECT(1);
  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP cpp_dbl_sequence(SEXP size, SEXP from, SEXP by) {
  int size_n = Rf_length(size);
  int from_n = Rf_length(from);
  int by_n = Rf_length(by);
  // int n = std::max(std::max(size_n, from_n), by_n);
  if (size_n > 0 && (from_n <= 0 || by_n <= 0)){
    Rcpp::stop("from and by must both have length >= 0");
  }
  // To recycle we would need to do sum * remainder of the sum over n
  SEXP out = PROTECT(Rf_allocVector(REALSXP, r_sum(size)));
  double *p_out = REAL(out);
  R_xlen_t index = 0;
  // int sj;
  int fj;
  int bj;
  int seq_size;
  double start;
  double increment;
  if (size_n > 0){
    int *p_size = INTEGER(size);
    double *p_from = REAL(from);
    double *p_by = REAL(by);
    for (int j = 0; j < size_n; ++j){
      seq_size = p_size[j];
      if (seq_size < 0){
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
      for (int i = 0; i < seq_size; ++i){
        p_out[index] = ( start + (i * increment) );
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
  SEXP out = PROTECT(Rf_allocVector(INTSXP, r_sum(size)));
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
IntegerVector lag_sequence(IntegerVector size, double k) {
  if (Rcpp::min(size) < 0){
    stop("size must be a vector of non-negative integers");
  }
  int size_n = size.length();
  k = std::fmax(k, 0);
  IntegerVector out(r_sum(size));
  R_xlen_t index = 0;
  for (int j = 0; j < size_n; ++j){
    for (int i = 0; i < size[j]; ++i){
      if (i < k){
        out[index] = NA_INTEGER;
      } else {
        out[index] = k;
      }
      ++index;
    }
  }
  return out;
}
// [[Rcpp::export(rng = false)]]
IntegerVector lead_sequence(IntegerVector size, double k) {
  if (Rcpp::min(size) < 0){
    stop("size must be a vector of non-negative integers");
  }
  int size_n = size.length();
  k = std::fmax(k, 0);
  IntegerVector out(r_sum(size));
  R_xlen_t index = 0;
  int idiff;
  for (int j = 0; j < size_n; ++j){
    for (int i = 0; i < size[j]; ++i){
      idiff = size[j] - i - 1;
      if (idiff < k){
        out[index] = NA_INTEGER;
      } else {
        out[index] = k;
      }
      ++index;
    }
  }
  return out;
}
