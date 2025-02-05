#include "timeplyr.h"

// Internal function
// x must be a numeric or integer
// threshold must be a numeric or integer of length 1
// This basically takes as input a cumulatively increasing time vector,
// and once a threshold is reached
// This is flagged a 1, the threshold is reset at that time value, and
// a new flag will occur once this new threshold is reached, and so on.
// Again, x must be a cumulatively increasing vector.

[[cpp11::register]]
SEXP roll_time_threshold(SEXP x, double threshold, bool switch_on_boundary) {
  int n = Rf_length(x);
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  int *p_out = INTEGER(out);
  double init_threshold = 0;
  init_threshold = init_threshold + threshold;
  switch( TYPEOF(x) ) {
  case REALSXP: {
    double *p_x = REAL(x);
    double tol = std::sqrt(std::numeric_limits<double>::epsilon());
    if (switch_on_boundary){
      tol = -tol;
    }
    for (int i = 0; i < n; ++i) {
      if (!(p_x[i] == p_x[i])){
        p_out[i] = NA_INTEGER;
      } else if ((p_x[i] - threshold) > tol) {
        p_out[i] = 1;
        threshold = init_threshold + p_x[i];
      } else {
        p_out[i] = 0;
      }
    }
    break;
  }
  case INTSXP: {
    int *p_x = INTEGER(x);
    if (switch_on_boundary == true){
      for (int i = 0; i < n; ++i) {
        if (p_x[i] == NA_INTEGER){
          p_out[i] = NA_INTEGER;
        } else if (p_x[i] >= threshold) {
          p_out[i] = 1;
          threshold = init_threshold + p_x[i];
        } else {
          p_out[i] = 0;
        }
      }
    } else {
      for (int i = 0; i < n; ++i) {
        if (p_x[i] == NA_INTEGER){
          p_out[i] = NA_INTEGER;
        } else if (p_x[i] > threshold) {
          p_out[i] = 1;
          threshold = init_threshold + p_x[i];
        } else {
          p_out[i] = 0;
        }
      }
    }
    break;
  }
  default: {
    Rf_unprotect(1);
    Rf_error("roll_time_threshold only supports integer and numeric vectors");
  }
  }
  Rf_unprotect(1);
  return out;
}

[[cpp11::register]]
SEXP cpp_consecutive_na_id(SEXP x, bool left_to_right){
  int NP = 0;
  int count = 0;
  int is_na;
  cpp11::function cheapr_length = cpp11::package("cheapr")["vector_length"];
  SEXP vec_length = Rf_protect(Rf_coerceVector(cheapr_length(x), REALSXP)); ++NP;
  R_xlen_t n = REAL(vec_length)[0];
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n)); ++NP;
  int *p_out = INTEGER(out);
  // This nicely handles NULL and avoids loop too
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    if (left_to_right){
      for (R_xlen_t i = 0; i < n; ++i){
        is_na = (p_x[i] == NA_INTEGER);
        count = (count + is_na) * is_na;
        p_out[i] = count;
        // p_out[i] = count == 0 ? NA_INTEGER : count;
      }
    } else {
      for (R_xlen_t i = n - 1; i >= 0; --i){
        is_na = (p_x[i] == NA_INTEGER);
        count = (count + is_na) * is_na;
        p_out[i] = count;
        // p_out[i] = count == 0 ? NA_INTEGER : count;
      }
    }

    break;
  }
  case REALSXP: {
    double *p_x = REAL(x);
    if (left_to_right){
      for (R_xlen_t i = 0; i < n; ++i){
        // Because NaN == NaN is false
        is_na = (p_x[i] != p_x[i]);
        count = (count + is_na) * is_na;
        p_out[i] = count;
        // p_out[i] = count == 0 ? NA_INTEGER : count;
      }
    } else {
      for (R_xlen_t i = n - 1; i >= 0; --i){
        // Because NaN == NaN is false
        is_na = (p_x[i] != p_x[i]);
        count = (count + is_na) * is_na;
        p_out[i] = count;
        // p_out[i] = count == 0 ? NA_INTEGER : count;
      }
    }

    break;
  }
  case STRSXP: {
    const SEXP *p_x = STRING_PTR_RO(x);
    if (left_to_right){
      for (R_xlen_t i = 0; i < n; ++i){
        is_na = (p_x[i] == NA_STRING);
        count = (count + is_na) * is_na;
        p_out[i] = count;
        // p_out[i] = count == 0 ? NA_INTEGER : count;
      }
    } else {
      for (R_xlen_t i = n - 1; i >= 0; --i){
        is_na = (p_x[i] == NA_STRING);
        count = (count + is_na) * is_na;
        p_out[i] = count;
        // p_out[i] = count == 0 ? NA_INTEGER : count;
      }
    }

    break;
  }
  case RAWSXP: {
    break;
  }
  case CPLXSXP: {
    Rcomplex *p_x = COMPLEX(x);
    if (left_to_right){
      for (R_xlen_t i = 0; i < n; ++i){
        is_na = (p_x[i]).r != (p_x[i]).r || (p_x[i]).i != (p_x[i]).i;
        count = (count + is_na) * is_na;
        p_out[i] = count;
        // p_out[i] = count == 0 ? NA_INTEGER : count;
      }
    } else {
      for (R_xlen_t i = n - 1; i >= 0; --i){
        is_na = (p_x[i]).r != (p_x[i]).r || (p_x[i]).i != (p_x[i]).i;
        count = (count + is_na) * is_na;
        p_out[i] = count;
        // p_out[i] = count == 0 ? NA_INTEGER : count;
      }
    }

    break;
  }
  // case VECSXP: {
  //   SEXP is_empty = Rf_protect(cpp_missing_row(x, 1, true));
  //   ++NP;
  //   int *p_is_empty = LOGICAL(is_empty);
  //   if (left_to_right){
  //     for (R_xlen_t i = 0; i < n; ++i){
  //       is_na = (p_is_empty[i] == TRUE);
  //       count = (count + is_na) * is_na;
  //       p_out[i] = count;
  //       // p_out[i] = count == 0 ? NA_INTEGER : count;
  //     }
  //   } else {
  //     for (R_xlen_t i = n - 1; i >= 0; --i){
  //       is_na = (p_is_empty[i] == TRUE);
  //       count = (count + is_na) * is_na;
  //       p_out[i] = count;
  //       // p_out[i] = count == 0 ? NA_INTEGER : count;
  //     }
  //   }
  //   break;
  // }
  default: {
    Rf_unprotect(NP);
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
    break;
  }
  }
  Rf_unprotect(NP);
  return out;
}

[[cpp11::register]]
SEXP cpp_which_first_gap(SEXP x, int increment, bool left_to_right) {
  int n = Rf_length(x);
  int *p_x = INTEGER(x);
  bool no_gap = true;
  int gap_loc;
  if (left_to_right){
    for (int i = 0; i < (n - 1); ++i){
      if ((p_x[i + 1] - p_x[i]) > increment){
        gap_loc = i + 1;
        no_gap = false;
        break;
      }
    }
  } else {
    for (int i = (n - 1); i > 0; --i){
      if ((p_x[i] - p_x[i - 1]) > increment){
        gap_loc = i + 1;
        no_gap = false;
        break;
      }
    }
  }
  if (no_gap){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 0));
    Rf_unprotect(1);
    return out;
  } else {
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 1));
    int *p_out = INTEGER(out);
    p_out[0] = gap_loc;
    Rf_unprotect(1);
    return out;
  }
}

void cpp_copy_names(SEXP source, SEXP target){
  SEXP source_nms = Rf_protect(Rf_getAttrib(source, R_NamesSymbol));
  SEXP target_nms = Rf_protect(Rf_duplicate(source_nms));
  Rf_setAttrib(target, R_NamesSymbol, target_nms);
  Rf_unprotect(2);
}

// cpp11::list period_add_v(const cpp11::integers sizes,
//                          const cpp11::doubles from,
//                          const cpp11::doubles by,
//                          const cpp11::strings unit,
//                          const cpp11::strings roll_month,
//                          const cpp11::strings roll_dst){
//
//   R_xlen_t n = sizes.size();
//   R_xlen_t from_size = from.size();
//   R_xlen_t by_size = by.size();
//
//   if (n == 0 || from_size == 0 || by_size == 0){
//     cpp11::stop("`sizes`, `from` and `by` must all have length > 0");
//   }
//
//   cpp11::writable::list period(1);
//   period.names() = unit;
//   cpp11::writable::list out(n);
//
//   double num;
//   int size;
//
//   cpp11::writable::doubles start(1);
//
//   for (R_xlen_t i = 0; i < n; ++i){
//     num = by[i % by_size];
//     size = sizes[i];
//
//     cpp11::writable::doubles temp(size);
//
//     // seq_len(size) * increment but starting from 0 not 1
//     for (R_xlen_t j = 0; j < size; ++j) temp[j] = num * j;
//
//     period[0] = temp;
//     start[0] = from[i % from_size];
//     SHALLOW_DUPLICATE_ATTRIB(start, from);
//     std::string roll_month_str = roll_month[0];
//     out[i] = C_time_add(start, period, roll_month_str, roll_dst);
//   }
//   return out;
// }

// cpp11::doubles adjust_duration_estimate(
//     cpp11::writable::doubles est, cpp11::doubles start, cpp11::doubles end,
//     cpp11::doubles width, const cpp11::strings unit
// ){
//
//   R_xlen_t n = est.size();
//   R_xlen_t end_size = end.size();
//   cpp11::strings roll_dst = Rf_protect(Rf_ScalarString(Rf_mkChar("NA")));
//   std::string roll_month = "preday";
//
//   for (R_xlen_t i = 0; i < n; ++i){
//     est[i] = std::ceil(est[i]);
//   }
//
//   // Temporary variables
//   cpp11::writable::doubles temp_adj(n);
//   cpp11::writable::list period_add(1);
//   period_add.names() = unit;
//
//   double res;
//
//   // width * est
//   for (R_xlen_t i = 0; i < n; ++i){
//     res = width[i] * est[i];
//     temp_adj[i] = res != res ? NA_REAL : res;
//   }
//
//
//   period_add[0] = temp_adj;
//   cpp11::writable::doubles up_date = C_time_add(start, period_add, roll_month, roll_dst);
//
//   R_xlen_t n_adj, k;
//
//   cpp11::function cheapr_which = cpp11::package("cheapr")["which_"];
//   cpp11::writable::logicals compare(n);
//
//   for (R_xlen_t i = 0; i < n; ++i){
//     compare[i] = up_date[i] < end[i];
//   }
//   cpp11::integers which = cpp11::as_integers(cheapr_which(compare));
//
//   n_adj = which.size();
//   while (n_adj > 0){
//     cpp11::writable::doubles temp2(n_adj);
//     cpp11::writable::doubles temp_start(n_adj);
//     for (R_xlen_t j = 0; j < n_adj; ++j){
//       k = which[j];
//       est[k] += 1;
//       temp2[j] = width[k] * est[k];
//       temp_start[j] = start[k];
//     }
//     period_add[0] = temp2;
//     temp_start = C_time_add(temp_start, period_add, roll_month, roll_dst);
//     for (R_xlen_t j = 0; j < n_adj; ++j){
//       k = which[j];
//       up_date[k] = temp_start[j];
//     }
//     for (R_xlen_t i = 0; i < n; ++i){
//       compare[i] = up_date[i] < end[i];
//     }
//     which = cpp11::as_integers(cheapr_which(compare));
//     n_adj = which.size();
//   }
//
//   cpp11::writable::doubles low_date = up_date;
//
//   for (R_xlen_t i = 0; i < n; ++i){
//     compare[i] = low_date[i] > end[i];
//   }
//   which = cpp11::as_integers(cheapr_which(compare));
//
//   n_adj = which.size();
//   while (n_adj > 0){
//     cpp11::writable::doubles temp2(n_adj);
//     cpp11::writable::doubles temp_start(n_adj);
//     for (R_xlen_t j = 0; j < n_adj; ++j){
//       k = which[j];
//       est[k] -= 1;
//       up_date[k] = low_date[k];
//       temp2[j] = width[k] * est[k];
//       temp_start[j] = start[k];
//     }
//     period_add[0] = temp2;
//     temp_start = C_time_add(temp_start, period_add, roll_month, roll_dst);
//     for (R_xlen_t j = 0; j < n_adj; ++j){
//       k = which[j];
//       low_date[k] = temp_start[j];
//     }
//     for (R_xlen_t i = 0; i < n; ++i){
//       compare[i] = low_date[i] > end[i];
//     }
//     which = cpp11::as_integers(cheapr_which(compare));
//     n_adj = which.size();
//   }
//
//   double frac;
//   cpp11::writable::doubles out(n);
//
//   for (R_xlen_t i = 0; i < n; ++i){
//     frac = (low_date[i] == up_date[i]) ?
//     0 : (end[i % end_size] - low_date[i]) / (up_date[i] - low_date[i]);
//     out[i] = est[i] + frac;
//   }
//   Rf_unprotect(1);
//   return out;
// }
