#include "timeplyr.h"

[[cpp11::register]]
R_xlen_t cpp_vector_size(SEXP x){
  if (Rf_isFrame(x)){
    return Rf_xlength(Rf_getAttrib(x, R_RowNamesSymbol));
    // Is x a list?
  } else if (Rf_isVectorList(x)){
    if (Rf_inherits(x, "vctrs_rcrd")){
      return cpp_vector_size(VECTOR_ELT(x, 0));
    } else if (Rf_inherits(x, "POSIXlt")){
      const SEXP *p_x = VECTOR_PTR_RO(x);
      R_xlen_t out = 0;
      for (int i = 0; i != 10; ++i){
        out = std::max(out, Rf_xlength(p_x[i]));
      }
      return out;
    } else if (Rf_isObject(x)){
      return Rf_asReal(base_r_length(x));
    } else {
      return Rf_xlength(x);
    }
    // Catch-all
  } else {
    return Rf_xlength(x);
  }
}

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
  int n_protections = 0;
  int count = 0;
  int is_na;
  R_xlen_t n = cpp_vector_size(x);
  SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
  ++n_protections;
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
  //   ++n_protections;
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
    Rf_unprotect(n_protections);
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
    break;
  }
  }
  Rf_unprotect(n_protections);
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
