#include <Rcpp.h>
using namespace Rcpp;

double r_sum(SEXP x){
  Rcpp::Function base_sum = Rcpp::Environment::base_env()["sum"];
  double out = 0;
  SEXP sum = PROTECT(base_sum(x));
  SEXP sum_double = PROTECT(Rf_coerceVector(sum, REALSXP));
  double *p_sum = REAL(sum_double);
  if (Rf_length(sum_double) > 0){
    out = p_sum[0];
  }
  UNPROTECT(2);
  return out;
}

double r_min(SEXP x){
  Rcpp::Function base_min = Rcpp::Environment::base_env()["min"];
  double out = R_PosInf;
  if (Rf_length(x) > 0){
    SEXP min = PROTECT(base_min(x));
    SEXP min_double = PROTECT(Rf_coerceVector(min, REALSXP));
    double *p_min = REAL(min_double);
    out = p_min[0];
    UNPROTECT(2);
  }
  return out;
}

// Rcpp version
// double r_sum(SEXP x){
//   Rcpp::Function base_sum = Rcpp::Environment::base_env()["sum"];
//   double out = 0;
//   NumericVector outv = Rcpp::as<Rcpp::NumericVector>(base_sum(x));
//   if (outv.length() > 0){
//     out = outv[0];
//   }
//   return out;
// }

// Rcpp version
// double r_min(SEXP x){
//   Rcpp::Function base_min = Rcpp::Environment::base_env()["min"];
//   double out = R_PosInf;
//   if (Rf_length(x) > 0){
//     NumericVector outv = Rcpp::as<Rcpp::NumericVector>(base_min(x));
//     out = outv[0];
//   }
//   return out;
// }

// [[Rcpp::export(rng = false)]]
IntegerVector before_sequence(IntegerVector size, double k) {
  if (Rcpp::min(size) < 0){
    Rf_error("size must be a vector of non-negative integers");
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
    Rf_error("size must be a vector of non-negative integers");
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
// SEXP cpp_int_sequence(SEXP size, SEXP from, SEXP by) {
//   int size_n = Rf_length(size);
//   int from_n = Rf_length(from);
//   int by_n = Rf_length(by);
//   if (from_n <= 0 || by_n <= 0){
//     Rf_error("from and by must both have length >= 0");
//   }
//   SEXP out = PROTECT(Rf_allocVector(INTSXP, r_sum(size)));
//   int *p_out = INTEGER(out);
//   R_xlen_t index = 0;
//   // int sj;
//   int fj;
//   int bj;
//   int start;
//   int increment;
//   int seq_size;
//   double seq_end;
//   if (size_n > 0){
//     int *p_size = INTEGER(size);
//     int *p_from = INTEGER(from);
//     int *p_by = INTEGER(by);
//     for (int j = 0; j < size_n; ++j){
//       seq_size = p_size[j];
//       if (seq_size < 0){
//         Rf_error("size must be a vector of non-negative integers");
//       }
//       // sj = j % size_n;
//       fj = j % from_n;
//       bj = j % by_n;
//       start = p_from[fj];
//       increment = p_by[bj];
//       // Throw error if integer overflow
//       seq_end = start + (increment * (std::fmax(seq_size - 1, 0)));
//       if (seq_end > std::numeric_limits<int>::max()){
//         Rcpp::stop("Integer overflow value of %f in sequence %f", seq_end, j + 1);
//       }
//       if (start == NA_INTEGER){
//         Rf_error("from contains NA values");
//       }
//       if (increment == NA_INTEGER){
//         Rf_error("by contains NA values");
//       }
//       for (int i = 0; i < seq_size; ++i){
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
    Rf_error("from and by must both have length >= 0");
  }
  // To recycle we would need to do sum * remainder of the sum over n
  double out_size = r_sum(size);
  double min_size = r_min(size);
  if (!(out_size == out_size)){
    Rf_error("size must not contain NA values");
  }
  if (min_size < 0){
    Rf_error("size must be a vector of non-negative integers");
  }
  SEXP out = PROTECT(Rf_allocVector(REALSXP, out_size));
  double *p_out = REAL(out);
  R_xlen_t index = 0;
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
      // NA sizes
      // if (seq_size == NA_INTEGER){
      //   UNPROTECT(1);
      //   Rf_error("sequence sizes cannot be NA");
      // }
      // Negative sizes
      // if (seq_size < 0){
      //   UNPROTECT(1);
      //   Rf_error("size must be a vector of non-negative integers");
      // }
      fj = j % from_n;
      bj = j % by_n;
      start = p_from[fj];
      increment = p_by[bj];
      if (!(start == start)){
        UNPROTECT(1);
        Rf_error("from contains NA values");
      }
      if (!(increment == increment)){
        UNPROTECT(1);
        Rf_error("by contains NA values");
      }
      for (int i = 0; i < seq_size; ++i){
        p_out[index] = ( start + (i * increment) );
        ++index;
      }
    }
  }
  UNPROTECT(1);
  return out;
}

// [[Rcpp::export(rng = false)]]
IntegerVector window_sequence(IntegerVector size,
                              double k,
                              bool partial = true,
                              bool ascending = true) {
  int size_n = size.length();
  if (Rcpp::min(size) < 0){
    Rf_error("size must be a vector of non-negative integers");
  }
  k = std::fmax(k, 0);
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
IntegerVector lag_sequence(IntegerVector size, double k, bool partial = false) {
  if (Rcpp::min(size) < 0){
    Rf_error("size must be a vector of non-negative integers");
  }
  int size_n = size.length();
  k = std::fmax(k, 0);
  IntegerVector out(r_sum(size));
  R_xlen_t index = 0;
  if (partial){
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
  } else {
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
  }
  return out;
}
// [[Rcpp::export(rng = false)]]
IntegerVector lead_sequence(IntegerVector size, double k, bool partial = false) {
  if (Rcpp::min(size) < 0){
    Rf_error("size must be a vector of non-negative integers");
  }
  int size_n = size.length();
  k = std::fmax(k, 0);
  IntegerVector out(r_sum(size));
  R_xlen_t index = 0;
  int idiff;
  if (partial){
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
  } else {
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
  }
  return out;
}
