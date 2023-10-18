#include <Rcpp.h>
using namespace Rcpp;

// This cannot handle NAs but is fast

// NumericVector roll_apply_max_fast(NumericVector x,
//                                   int before,
//                                   int after) {
//   int n = x.length();
//   int istart;
//   int iend;
//   int j_n;
//   double imax;
//   double ix;
//   NumericVector out(n);
//   for (int i = 0; i < n; ++i) {
//     istart = std::max(i - before, 0);
//     iend = std::min(i + after, n - 1);
//     j_n = iend - istart + 1;
//     imax = x[istart];
//     for (int j = 0; j < j_n; ++j){
//       ix = x[istart + j];
//       if (ix > imax){
//         imax = ix;
//       }
//     }
//     out[i] = imax;
//   }
//   return out;
// }


// NumericVector roll_apply_max(NumericVector x,
//                              int before,
//                              int after,
//                              bool na_rm,
//                              bool partial) {
//   int n = x.length();
//   int istart;
//   int iend;
//   int j_n;
//   double imax;
//   double ix;
//   int min_size;
//   if (partial){
//     min_size = 0;
//   } else {
//     min_size = before + after;
//   }
//   Function before_sequence("before_sequence");
//   Function after_sequence("after_sequence");
//   IntegerVector befores = before_sequence(n, before);
//   IntegerVector afters = after_sequence(n, after);
//   NumericVector out(n);
//   for (int i = 0; i < n; ++i) {
//     istart = i - befores[i];
//     iend = i + afters[i];
//     j_n = iend - istart + 1;
//     imax = x[istart];
//     if ( (istart + iend) < min_size){
//       out[i] = NA_REAL;
//     } else {
//       for (int j = 0; j < j_n; ++j){
//         ix = x[istart + j];
//         if (NumericVector::is_na(ix)){
//           if (na_rm){
//             imax = x[std::min(istart + j + 1, j_n - 1)];
//           } else {
//             imax = NA_REAL;
//             break;
//           }
//         } else if (ix > imax){
//           imax = ix;
//         }
//       }
//       out[i] = imax;
//     }
//   }
//   return out;
// }

// [[Rcpp::export(rng = false)]]
IntegerVector before_sequence(IntegerVector size, int k) {
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


// [[Rcpp::export(rng = false)]]
IntegerVector window_sequence(IntegerVector size,
                              int k,
                              bool partial = true,
                              bool ascending = true) {
  int size_n = size.length();
  k = std::max(k, 0);
  IntegerVector out(sum(size), k);
  int index = 0;
  if (ascending){
    // right aligned window sequences
    if (partial){
      for (int j = 0; j < size_n; ++j){
        for (int i = 0; i < size[j]; ++i){
          if (i < k){
            out[index] = i + 1;
          }
          ++index;
        }
      }
    } else {
      for (int j = 0; j < size_n; ++j){
        for (int i = 0; i < size[j]; ++i){
          if (i < (k - 1)){
            out[index] = NA_INTEGER;
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
            out[index] = idiff + 1;
          }
          ++index;
        }
      }
    } else {
      for (int j = 0; j < size_n; ++j){
        for (int i = 0; i < size[j]; ++i){
          idiff = size[j] - i - 1;
          if (idiff < (k - 1)){
            out[index] = NA_INTEGER;
          }
          ++index;
        }
      }
    }
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
IntegerVector lag_sequence(IntegerVector size, int k) {
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

// NumericVector roll_apply_max(NumericVector x,
//                              IntegerVector before,
//                              IntegerVector after,
//                              bool na_rm) {
//   int n = x.length();
//   int b_n = before.length();
//   int a_n = after.length();
//   if (b_n != n || a_n != n){
//     stop("before and after must be the same length as x");
//   }
//   int istart;
//   int iend;
//   NumericVector out(n);
//   if (na_rm == true){
//     for (int i = 0; i < n; ++i) {
//       if (IntegerVector::is_na(before[i]) ||
//           IntegerVector::is_na(after[i])){
//         out[i] = NA_REAL;
//       } else {
//         istart = i - before[i];
//         iend = i + after[i];
//         out[i] = max(na_omit(x[Rcpp::Range(istart, iend)]));
//       }
//     }
//   } else {
//     for (int i = 0; i < n; ++i) {
//       if (IntegerVector::is_na(before[i]) ||
//           IntegerVector::is_na(after[i])){
//         out[i] = NA_REAL;
//       } else {
//         istart = i - before[i];
//         iend = i + after[i];
//         out[i] = max(x[Rcpp::Range(istart, iend)]);
//       }
//     }
//   }
//
//   return out;
// }
// NumericVector roll_apply_max2(NumericVector x,
//                              int before,
//                              int after) {
//   int n = x.length();
//   int istart;
//   int iend;
//   NumericVector out(n);
//   for (int i = 0; i < n; ++i) {
//     istart = std::max(i - before, 0);
//     iend = std::min(i + after, n - 1);
//     out[i] = max(na_omit(x[Rcpp::Range(istart, iend)]));
//   }
//   return out;
// }

// I thought this should have partial implementation
// But it makes more sense to have each roll function
// Have a partial implementation and for
// the before/after sequences to have no NAs

// IntegerVector before_sequence(IntegerVector size,
//                               int k,
//                               bool partial) {
//   int size_n = size.length();
//   k = std::max(k, 0);
//   IntegerVector out(sum(size));
//   int index = 0;
//   if (partial){
//     for (int j = 0; j < size_n; ++j){
//       for (int i = 0; i < size[j]; ++i){
//         if (i < k){
//           out[index] = i;
//         } else {
//           out[index] = k;
//         }
//         index = index + 1;
//       }
//     }
//   } else {
//     for (int j = 0; j < size_n; ++j){
//       for (int i = 0; i < size[j]; ++i){
//         if (i < k){
//           out[index] = NA_INTEGER;
//         } else {
//           out[index] = k;
//         }
//         index = index + 1;
//       }
//     }
//   }
//   return out;
// }

// IntegerVector after_sequence(IntegerVector size,
//                              int k,
//                              bool partial) {
//   int size_n = size.length();
//   k = std::max(k, 0);
//   IntegerVector out(sum(size));
//   int index = 0;
//   int idiff;
//   if (partial){
//     for (int j = 0; j < size_n; ++j){
//       for (int i = 0; i < size[j]; ++i){
//         idiff = size[j] - i - 1;
//         if (idiff < k){
//           out[index] = idiff;
//         } else {
//           out[index] = k;
//         }
//         index = index + 1;
//       }
//     }
//   } else {
//     for (int j = 0; j < size_n; ++j){
//       for (int i = 0; i < size[j]; ++i){
//         idiff = size[j] - i - 1;
//         if (idiff < k){
//           out[index] = NA_INTEGER;
//         } else {
//           out[index] = k;
//         }
//         index = index + 1;
//       }
//     }
//   }
//   return out;
// }

// IntegerVector window_sequence(IntegerVector size,
//                               int k,
//                               bool partial,
//                               bool ascending) {
//   int size_n = size.length();
//   k = std::max(k, 0);
//   IntegerVector out(sum(size));
//   int index = 0;
//   if (ascending){
//     // right aligned window sequences
//     if (partial){
//       for (int j = 0; j < size_n; ++j){
//         for (int i = 0; i < size[j]; ++i){
//           if (i < k){
//             out[index] = i + 1;
//           } else {
//             out[index] = k;
//           }
//           index = index + 1;
//         }
//       }
//     } else {
//       for (int j = 0; j < size_n; ++j){
//         for (int i = 0; i < size[j]; ++i){
//           if (i < (k - 1)){
//             out[index] = NA_INTEGER;
//           } else {
//             out[index] = k;
//           }
//           index = index + 1;
//         }
//       }
//     }
//   } else {
//     // left aligned window sequences
//     int idiff;
//     if (partial){
//       for (int j = 0; j < size_n; ++j){
//         for (int i = 0; i < size[j]; ++i){
//           idiff = size[j] - i - 1;
//           if (idiff < k){
//             out[index] = idiff + 1;
//           } else {
//             out[index] = k;
//           }
//           index = index + 1;
//         }
//       }
//     } else {
//       for (int j = 0; j < size_n; ++j){
//         for (int i = 0; i < size[j]; ++i){
//           idiff = size[j] - i - 1;
//           if (idiff < (k - 1)){
//             out[index] = NA_INTEGER;
//           } else {
//             out[index] = k;
//           }
//           index = index + 1;
//         }
//       }
//     }
//   }
//   return out;
// }

// NumericVector roll_apply_max4(NumericVector x,
//                               int before,
//                               int after,
//                               bool na_rm) {
//   int n = x.length();
//   int istart;
//   int iend;
//   int j_n;
//   double imax;
//   double ix;
//   NumericVector out(n);
//   for (int i = 0; i < n; ++i) {
//     istart = std::max(i - before, 0);
//     iend = std::min(i + after, n - 1);
//     j_n = iend - istart + 1;
//     imax = x[istart];
//     for (int j = 0; j < j_n; ++j){
//       ix = x[istart + j];
//       if (NumericVector::is_na(ix)){
//         if (na_rm){
//           imax = x[std::min(istart + j + 1, j_n - 1)];
//         } else {
//           imax = NA_REAL;
//           break;
//         }
//       }
//       if (ix > imax){
//         imax = ix;
//       }
//     }
//     out[i] = imax;
//   }
//   return out;
// }

// NumericVector roll_apply_max(NumericVector x,
//                              int before,
//                              int after,
//                              bool na_rm,
//                              bool partial) {
//   int n = x.length();
//   // int b_n = before.length();
//   // int a_n = after.length();
//   // if (b_n != n || a_n != n){
//   //   stop("before and after must be the same length as x");
//   // }
//   int istart;
//   int iend;
//   int j_n;
//   double imax;
//   double ix;
//   int min_size;
//   if (partial){
//     min_size = 0;
//   } else {
//     min_size = before + after;
//   }
//   Function before_sequence("before_sequence");
//   Function after_sequence("after_sequence");
//   IntegerVector befores(n);
//   IntegerVector afters(n);
//   befores = before_sequence(n, before);
//   afters = after_sequence(n, after);
//   NumericVector out(n);
//   if (partial){
//
//     for (int i = 0; i < n; ++i) {
//       istart = i - befores[i];
//       iend = i + afters[i];
//       j_n = iend - istart + 1;
//       imax = x[istart];
//       for (int j = 0; j < j_n; ++j){
//         ix = x[istart + j];
//         if (NumericVector::is_na(ix)){
//           if (na_rm){
//             imax = x[std::min(istart + j + 1, j_n - 1)];
//           } else {
//             imax = NA_REAL;
//             break;
//           }
//         } else if (ix > imax){
//           imax = ix;
//         }
//       }
//       out[i] = imax;
//     }
//   } else {
//
//     for (int i = 0; i < n; ++i) {
//       istart = i - befores[i];
//       iend = i + afters[i];
//       j_n = iend - istart + 1;
//       imax = x[istart];
//       for (int j = 0; j < j_n; ++j){
//         ix = x[istart + j];
//         if (NumericVector::is_na(ix)){
//           if (na_rm){
//             imax = x[std::min(istart + j + 1, j_n - 1)];
//           } else {
//             imax = NA_REAL;
//             break;
//           }
//         } else if (ix > imax){
//           imax = ix;
//         }
//       }
//       out[i] = imax;
//     }
//   }
//   return out;
// }

// Fill n NA values with last non NA observation
// NumericVector fill_n(NumericVector x, int n) {
//   NumericVector out = clone(x);
//   int size = x.length();
//   int j = 0;
//   bool first_non_na = false;
//   bool fill_flag = false;
//   bool is_na;
//   double fill;
//   for (int i = 0; i < size; ++i) {
//     is_na = NumericVector::is_na(x[i]);
//     // First non-NA
//     if (!is_na){
//       first_non_na = true;
//     }
//     // Fill NA value
//     if (first_non_na && is_na && !fill_flag){
//       fill = x[i - 1];
//       fill_flag = true;
//     }
//     if (first_non_na && is_na){
//       out[i] = fill;
//       j = j + 1;
//     }
//     // Reset searching for first fill value
//     if (j == n){
//       break;
//     }
//   }
//   return out;
// }
