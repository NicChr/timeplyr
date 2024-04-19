#include "timeplyr_cpp.h"

// o must be the order of groups, e.g order(g)
// sizes must be the sorted group sizes, e.g table(g), or count(df, g)[["g"]]

[[cpp11::register]]
SEXP cpp_roll_lag(SEXP x, int k, SEXP fill) {
  R_xlen_t size = cpp_vector_size(x);
  int fill_size = Rf_length(fill);
  int n_protections = 0;
  if (fill_size > 1){
    Rf_error("fill size must be NULL or length 1");
  }
  if (k < 0){
    Rf_error("k must be a non-negative integer");
  }
  switch(TYPEOF(x)){
  case LGLSXP:
  case INTSXP: {
    int fill_value = NA_INTEGER;
    if (fill_size >= 1){
      fill_value = Rf_asInteger(fill);
    }
    SEXP out = Rf_protect(Rf_duplicate(x));
    ++n_protections;
    int *p_x = INTEGER(x);
    int *p_out = INTEGER(out);
    for (R_xlen_t i = 0; i < size; ++i) {
      p_out[i] = i < k ? fill_value : p_x[i - k];
    }
    Rf_unprotect(n_protections);
    return out;
  }
  case REALSXP: {
    double fill_value = NA_REAL;
    if (fill_size >= 1){
      fill_value = Rf_asReal(fill);
    }
    SEXP out = Rf_protect(Rf_duplicate(x));
    ++n_protections;
    double *p_x = REAL(x);
    double *p_out = REAL(out);
    for (R_xlen_t i = 0; i < size; ++i) {
      p_out[i] = i < k ? fill_value : p_x[i - k];
    }
    Rf_unprotect(n_protections);
    return out;
  }
  case STRSXP: {
    SEXP fill_char = fill_size >= 1 ? Rf_protect(Rf_asChar(fill)) : Rf_protect(NA_STRING);
    ++n_protections;
    SEXP out = Rf_protect(Rf_duplicate(x));
    ++n_protections;
    SEXP *p_x = STRING_PTR(x);;
    for (R_xlen_t i = 0; i < size; ++i) {
      SET_STRING_ELT(out, i, i < k ? fill_char : p_x[i - k]);
    }
    Rf_unprotect(n_protections);
    return out;
  }
  case VECSXP: {
    int num_col = Rf_length(x);
    const SEXP *p_x = VECTOR_PTR_RO(x);
    SEXP out = Rf_protect(Rf_allocVector(VECSXP, num_col));
    ++n_protections;
    SHALLOW_DUPLICATE_ATTRIB(out, x);
    for (int i = 0; i < num_col; ++i){
      SEXP list_item = Rf_protect(cpp_roll_lag(p_x[i], k, fill));
      SET_VECTOR_ELT(out, i, list_item);
      ++n_protections;
    }
    Rf_unprotect(n_protections);
    return out;
  }
  default: {
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
    break;
  }
  }
}

[[cpp11::register]]
SEXP cpp_roll_lead(SEXP x, int k, SEXP fill) {
  R_xlen_t size = cpp_vector_size(x);
  int fill_size = Rf_length(fill);
  int n_protections = 0;
  if (fill_size > 1){
    Rf_error("fill size must be NULL or length 1");
  }
  if (k < 0){
    Rf_error("k must be a non-negative integer");
  }
  switch(TYPEOF(x)){
  case LGLSXP:
  case INTSXP: {
    int fill_value = NA_INTEGER;
    if (fill_size >= 1){
      fill_value = Rf_asInteger(fill);
    }
    SEXP out = Rf_protect(Rf_duplicate(x));
    ++n_protections;
    int *p_x = INTEGER(x);
    int *p_out = INTEGER(out);
    for (R_xlen_t i = 0; i < size; ++i) {
      p_out[i] = (i >= (size - k)) ? fill_value : p_x[i + k];
    }
    Rf_unprotect(n_protections);
    return out;
  }
  case REALSXP: {
    double fill_value = NA_REAL;
    if (fill_size >= 1){
      fill_value = Rf_asReal(fill);
    }
    SEXP out = Rf_protect(Rf_duplicate(x));
    ++n_protections;
    double *p_x = REAL(x);
    double *p_out = REAL(out);
    for (R_xlen_t i = 0; i < size; ++i) {
      p_out[i] = (i >= (size - k)) ? fill_value : p_x[i + k];
    }
    Rf_unprotect(n_protections);
    return out;
  }
  case STRSXP: {
    SEXP fill_char = fill_size >= 1 ? Rf_protect(Rf_asChar(fill)) : Rf_protect(NA_STRING);
    ++n_protections;
    SEXP out = Rf_protect(Rf_duplicate(x));
    ++n_protections;
    SEXP *p_x = STRING_PTR(x);;
    for (R_xlen_t i = 0; i < size; ++i) {
      SET_STRING_ELT(out, i, (i >= (size - k)) ? fill_char : p_x[i + k]);
    }
    Rf_unprotect(n_protections);
    return out;
  }
  case VECSXP: {
    int num_col = Rf_length(x);
    const SEXP *p_x = VECTOR_PTR_RO(x);
    SEXP out = Rf_protect(Rf_allocVector(VECSXP, num_col));
    ++n_protections;
    SHALLOW_DUPLICATE_ATTRIB(out, x);
    for (int i = 0; i < num_col; ++i){
      SEXP list_item = Rf_protect(cpp_roll_lead(p_x[i], k, fill));
      ++n_protections;
      SET_VECTOR_ELT(out, i, list_item);
    }
    Rf_unprotect(n_protections);
    return out;
  }
  default: {
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
    break;
  }
  }
}


[[cpp11::register]]
SEXP cpp_roll_lag_grouped(SEXP x, int k, SEXP o, SEXP sizes, SEXP fill) {
  int size = cpp_vector_size(x);
  int o_size = Rf_length(o);
  int fill_size = Rf_length(fill);
  if (o_size != size){
    Rf_error("x and o must both be the same length");
  }
  if (fill_size > 1){
    Rf_error("fill size must be NULL or length 1");
  }
  if (k < 0){
    Rf_error("k must be a non-negative integer");
  }
  int *p_sizes = INTEGER(sizes);
  int *p_o = INTEGER(o);
  int oi;
  int j = 0;
  int group_count = 0;
  int running_group_size = p_sizes[0];
  switch(TYPEOF(x)){
  case LGLSXP:
  case INTSXP: {
    int fill_value = NA_INTEGER;
    // k = std::min(k, p_sizes[j]);
    if (fill_size >= 1){
      fill_value = Rf_asInteger(fill);
    }
    SEXP out = Rf_protect(Rf_duplicate(x));
    int *p_x = INTEGER(x);
    int *p_out = INTEGER(out);
    for (int i = 0; i < size; ++i) {
      oi = p_o[i] - 1;
      // Start of new group?
      if (i > (running_group_size - 1)){
        ++j;
        running_group_size += p_sizes[j];
        group_count = 0;
      }
      if (group_count < k){
        p_out[oi] = fill_value;
      } else {
        p_out[oi] = p_x[p_o[i - k] - 1];
      }
      ++group_count;
    }
    Rf_unprotect(1);
    return out;
  }
  case REALSXP: {
    double fill_value = NA_REAL;
    // k = std::min(k, p_sizes[j]);
    if (fill_size >= 1){
      fill_value = Rf_asReal(fill);
    }
        SEXP out = Rf_protect(Rf_duplicate(x));
        double *p_x = REAL(x);
        double *p_out = REAL(out);
        for (int i = 0; i < size; ++i) {
            oi = p_o[i] - 1;
            // Start of new group?
            if (i > (running_group_size - 1)){
                ++j;
                running_group_size += p_sizes[j];
                group_count = 0;
            }
            if (group_count < k){
                p_out[oi] = fill_value;
            } else {
                p_out[oi] = p_x[p_o[i - k] - 1];
            }
            ++group_count;
        }
        Rf_unprotect(1);
        return out;
    }
    case STRSXP: {
        SEXP fill_sexp = Rf_protect(Rf_allocVector(STRSXP, 1));
        // SEXP fill_value = NA_STRING;
        // k = std::min(k, p_sizes[j]);
        if (fill_size >= 1){
            SET_STRING_ELT(fill_sexp, 0, Rf_asChar(fill));
        } else {
            SET_STRING_ELT(fill_sexp, 0, NA_STRING);
        }
        // if (fill_size >= 1){
        //     fill_value = Rf_asChar(fill);
        // }
        SEXP out = Rf_protect(Rf_duplicate(x));
        for (int i = 0; i < size; ++i) {
            oi = p_o[i] - 1;
            // Start of new group?
            if (i > (running_group_size - 1)){
                ++j;
                running_group_size += p_sizes[j];
                group_count = 0;
            }
            if (group_count < k){
                SET_STRING_ELT(out, oi, STRING_ELT(fill_sexp, 0));
                // SET_STRING_ELT(out, oi, fill_value);
            } else {
                SET_STRING_ELT(out, oi, STRING_ELT(x, p_o[i - k] - 1));
            }
            ++group_count;
        }
        Rf_unprotect(2);
        return out;
    }
  case VECSXP: {
    int num_col = Rf_length(x);
    const SEXP *p_x = VECTOR_PTR_RO(x);
    SEXP out = Rf_protect(Rf_allocVector(VECSXP, num_col));
    SHALLOW_DUPLICATE_ATTRIB(out, x);
    for (int i = 0; i < num_col; ++i){
      SET_VECTOR_ELT(out, i, Rf_protect(cpp_roll_lag_grouped(p_x[i], k, o, sizes, fill)));
    }
    Rf_unprotect(num_col + 1);
    return out;
  }
    default: {
      Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
      break;
    }
    }
}

[[cpp11::register]]
SEXP cpp_roll_lead_grouped(SEXP x, int k, SEXP o, SEXP sizes, SEXP fill) {
    int size = cpp_vector_size(x);
    int o_size = Rf_length(o);
    int fill_size = Rf_length(fill);
    if (o_size != size){
        Rf_error("x and o must both be the same length");
    }
    if (fill_size > 1){
        Rf_error("fill size must be NULL or length 1");
    }
    if (k < 0){
        Rf_error("k must be a non-negative integer");
    }
    int *p_sizes = INTEGER(sizes);
    int *p_o = INTEGER(o);
    int oi;
    int j = 0;
    int group_count = p_sizes[0];
    int running_group_size = p_sizes[0];
    switch(TYPEOF(x)){
    case LGLSXP:
    case INTSXP: {
        int fill_value = NA_INTEGER;
        // k = std::min(k, p_sizes[j]);
        if (fill_size >= 1){
            fill_value = Rf_asInteger(fill);
        }
        SEXP out = Rf_protect(Rf_duplicate(x));
        int *p_x = INTEGER(x);
        int *p_out = INTEGER(out);
        for (int i = 0; i < size; ++i) {
            oi = p_o[i] - 1;
            // Start of new group?
            if (i > (running_group_size - 1)){
                ++j;
                running_group_size += p_sizes[j];
                group_count = p_sizes[j];
            }
            if (group_count < (k + 1)){
                p_out[oi] = fill_value;
            } else {
                p_out[oi] = p_x[p_o[i + k] - 1];
            }
            --group_count;
        }
        Rf_unprotect(1);
        return out;
    }
    case REALSXP: {
        double fill_value = NA_REAL;
        // k = std::min(k, p_sizes[j]);
        if (fill_size >= 1){
            fill_value = Rf_asReal(fill);
        }
        SEXP out = Rf_protect(Rf_duplicate(x));
        double *p_x = REAL(x);
        double *p_out = REAL(out);
        for (int i = 0; i < size; ++i) {
            oi = p_o[i] - 1;
            // Start of new group?
            if (i > (running_group_size - 1)){
                ++j;
                running_group_size += p_sizes[j];
                group_count = p_sizes[j];
            }
            if (group_count < (k + 1)){
                p_out[oi] = fill_value;
            } else {
                p_out[oi] = p_x[p_o[i + k] - 1];
            }
            --group_count;
        }
        Rf_unprotect(1);
        return out;
    }
    case STRSXP: {
        SEXP fill_sexp = Rf_protect(Rf_allocVector(STRSXP, 1));
        if (fill_size >= 1){
            SET_STRING_ELT(fill_sexp, 0, Rf_asChar(fill));
        } else {
            SET_STRING_ELT(fill_sexp, 0, NA_STRING);
        }
        SEXP out = Rf_protect(Rf_duplicate(x));
        for (int i = 0; i < size; ++i) {
            oi = p_o[i] - 1;
            // Start of new group?
            if (i > (running_group_size - 1)){
                ++j;
                running_group_size += p_sizes[j];
                group_count = p_sizes[j];
            }
            if (group_count < (k + 1)){
                SET_STRING_ELT(out, oi, STRING_ELT(fill_sexp, 0));
            } else {
                SET_STRING_ELT(out, oi, STRING_ELT(x, p_o[i + k] - 1));
            }
            --group_count;
        }
        Rf_unprotect(2);
        return out;
    }
    case VECSXP: {
      int num_col = Rf_length(x);
      const SEXP *p_x = VECTOR_PTR_RO(x);
      SEXP out = Rf_protect(Rf_allocVector(VECSXP, num_col));
      SHALLOW_DUPLICATE_ATTRIB(out, x);
      for (int i = 0; i < num_col; ++i){
        SET_VECTOR_ELT(out, i, Rf_protect(cpp_roll_lead_grouped(p_x[i], k, o, sizes, fill)));
      }
      Rf_unprotect(num_col + 1);
      return out;
    }
    default: {
      Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
      break;
    }
    }
}

[[cpp11::register]]
SEXP cpp_roll_diff(SEXP x, int k, SEXP fill) {
    R_xlen_t size = Rf_xlength(x);
    int fill_size = Rf_length(fill);
    int n_protections = 0;
    if (fill_size > 1){
        Rf_error("fill size must be NULL or length 1");
    }
    switch(TYPEOF(x)){
    case LGLSXP: {
      int fill_value = NA_INTEGER;
      if (fill_size >= 1){
        fill_value = Rf_asInteger(fill);
      }
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, size));
      ++n_protections;
      int *p_x = INTEGER(x);
      int *p_out = INTEGER(out);
      if (k >= 0){
        for (R_xlen_t i = 0; i < size; ++i) {
          if (i < k){
            p_out[i] = fill_value;
          } else if ((p_x[i] == NA_INTEGER) || (p_x[i - k] == NA_INTEGER)) {
            p_out[i] = NA_INTEGER;
          } else {
            p_out[i] = p_x[i] - p_x[i - k];
          }
        }
      } else {
        for (R_xlen_t i = (size - 1); i >= 0; --i) {
          if (i >= (size + k)){
            p_out[i] = fill_value;
          } else if ((p_x[i] == NA_INTEGER) || (p_x[i - k] == NA_INTEGER)) {
            p_out[i] = NA_INTEGER;
          }
          else {
            p_out[i] = p_x[i] - p_x[i - k];
          }
        }
      }
      Rf_unprotect(n_protections);
      return out;
    }
    case INTSXP: {
      long long diff;
      long long value;
      long long lag_value;
      long long int_max = integer_max_;
      int fill_value = NA_INTEGER;
      if (fill_size >= 1){
        fill_value = Rf_asInteger(fill);
      }
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, size));
      ++n_protections;
      int *p_x = INTEGER(x);
      int *p_out = INTEGER(out);
      if (k >= 0){
        for (R_xlen_t i = 0; i < size; ++i) {
          if (i < k){
            p_out[i] = fill_value;
          } else if ((p_x[i] == NA_INTEGER) || (p_x[i - k] == NA_INTEGER)) {
            p_out[i] = NA_INTEGER;
          } else {
            value = p_x[i];
            lag_value = p_x[i - k];
            diff = value - lag_value;
            if (std::llabs(diff) > int_max){
              p_out[i] = NA_INTEGER;
            } else {
              p_out[i] = p_x[i] - p_x[i - k];
            }
          }
        }
      } else {
        for (R_xlen_t i = (size - 1); i >= 0; --i) {
          if (i >= (size + k)){
            p_out[i] = fill_value;
          } else if ((p_x[i] == NA_INTEGER) || (p_x[i - k] == NA_INTEGER)) {
            p_out[i] = NA_INTEGER;
          } else {
            value = p_x[i];
            lag_value = p_x[i - k];
            diff = value - lag_value;
            if (std::llabs(diff) > int_max){
              p_out[i] = NA_INTEGER;
            } else {
              p_out[i] = p_x[i] - p_x[i - k];
            }
          }
        }
      }
      Rf_unprotect(n_protections);
      return out;
    }
    case REALSXP: {
        double fill_value = NA_REAL;
        if (fill_size >= 1){
            fill_value = Rf_asReal(fill);
        }
        SEXP out = Rf_protect(Rf_allocVector(REALSXP, size));
        ++n_protections;
        double *p_x = REAL(x);
        double *p_out = REAL(out);
        if (k >= 0){
            for (R_xlen_t i = 0; i < size; ++i) {
                if (i < k){
                    p_out[i] = fill_value;
                } else {
                    p_out[i] = p_x[i] - p_x[i - k];
                }
            }
        } else {
            for (R_xlen_t i = (size - 1); i >= 0; --i) {
                if (i >= (size + k)){
                    p_out[i] = fill_value;
                } else {
                    p_out[i] = p_x[i] - p_x[i - k];
                }
            }
        }
        Rf_unprotect(n_protections);
        return out;
    }
    case VECSXP: {
      int num_col = Rf_length(x);
      const SEXP *p_x = VECTOR_PTR_RO(x);
      SEXP out = Rf_protect(Rf_allocVector(VECSXP, num_col));
      ++n_protections;
      SHALLOW_DUPLICATE_ATTRIB(out, x);
      for (int i = 0; i < num_col; ++i){
        SET_VECTOR_ELT(out, i, Rf_protect(cpp_roll_diff(p_x[i], k, fill)));
        ++n_protections;
      }
      Rf_unprotect(n_protections);
      return out;
    }
    default: {
      Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
      break;
    }
    }
}

[[cpp11::register]]
SEXP cpp_roll_diff_grouped(SEXP x, int k, SEXP o, SEXP sizes, SEXP fill) {
    int size = cpp_vector_size(x);
    int o_size = Rf_length(o);
    int fill_size = Rf_length(fill);
    int n_groups = Rf_length(sizes);
    if (o_size != size){
        Rf_error("x and o must both be the same length");
    }
    if (fill_size > 1){
        Rf_error("fill size must be NULL or length 1");
    }
    int *p_sizes = INTEGER(sizes);
    int *p_o = INTEGER(o);
    int oi;
    int j = 0;
    int group_count = 0;
    int running_group_size = p_sizes[0];
    switch(TYPEOF(x)){
    case LGLSXP: {
        int fill_value = NA_INTEGER;
        if (fill_size >= 1){
            fill_value = Rf_asInteger(fill);
        }
        SEXP out = Rf_protect(Rf_allocVector(INTSXP, size));
        int *p_x = INTEGER(x);
        int *p_out = INTEGER(out);
        if (k >= 0){
            for (int i = 0; i < size; ++i) {
                oi = p_o[i] - 1;
                // Start of new group?
                if (i > (running_group_size - 1)){
                    ++j;
                    running_group_size += p_sizes[j];
                    group_count = 0;
                }
                if (group_count < k){
                    p_out[oi] = fill_value;
                } else if ((p_x[oi] == NA_INTEGER) || (p_x[p_o[i - k] - 1] == NA_INTEGER)) {
                  p_out[oi] = NA_INTEGER;
                } else {
                    p_out[oi] = p_x[oi] - p_x[p_o[i - k] - 1];
                }
                ++group_count;
            }
        } else {
          j = n_groups - 1;
          running_group_size = p_sizes[j];
          group_count = running_group_size - 1;
            for (int i = (size - 1); i >= 0; --i) {
                oi = p_o[i] - 1;
                // Start of new group?
                if ( (size - i) > running_group_size){
                  --j;
                  running_group_size += p_sizes[j];
                  group_count = p_sizes[j] - 1;
                }
                if (group_count >= (p_sizes[j] + k)){
                    p_out[oi] = fill_value;
                } else if ((p_x[oi] == NA_INTEGER) || (p_x[p_o[i - k] - 1] == NA_INTEGER)) {
                  p_out[oi] = NA_INTEGER;
                } else {
                    p_out[oi] = p_x[oi] - p_x[p_o[i - k] - 1];
                }
                --group_count;
            }
        }
        Rf_unprotect(1);
        return out;
    }
    case INTSXP: {
      long long diff;
      long long value;
      long long lag_value;
      long long int_max = integer_max_;
      int fill_value = NA_INTEGER;
      if (fill_size >= 1){
        fill_value = Rf_asInteger(fill);
      }
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, size));
      int *p_x = INTEGER(x);
      int *p_out = INTEGER(out);
      if (k >= 0){
        for (int i = 0; i < size; ++i) {
          oi = p_o[i] - 1;
          // Start of new group?
          if (i > (running_group_size - 1)){
            ++j;
            running_group_size += p_sizes[j];
            group_count = 0;
          }
          if (group_count < k){
            p_out[oi] = fill_value;
          } else if ((p_x[oi] == NA_INTEGER) || (p_x[p_o[i - k] - 1] == NA_INTEGER)) {
            p_out[oi] = NA_INTEGER;
          } else {
            value = p_x[oi];
            lag_value = p_x[p_o[i - k] - 1];
            diff = value - lag_value;
            if (std::llabs(diff) > int_max){
              p_out[oi] = NA_INTEGER;
            } else {
              p_out[oi] = p_x[oi] - p_x[p_o[i - k] - 1];
            }
          }
          ++group_count;
        }
      } else {
        j = n_groups - 1;
        running_group_size = p_sizes[j];
        group_count = running_group_size - 1;
        for (int i = (size - 1); i >= 0; --i) {
          oi = p_o[i] - 1;
          // Start of new group?
          if ( (size - i) > running_group_size){
            --j;
            running_group_size += p_sizes[j];
            group_count = p_sizes[j] - 1;
          }
          if (group_count >= (p_sizes[j] + k)){
            p_out[oi] = fill_value;
          } else if ((p_x[oi] == NA_INTEGER) || (p_x[p_o[i - k] - 1] == NA_INTEGER)) {
            p_out[oi] = NA_INTEGER;
          } else {
            value = p_x[oi];
            lag_value = p_x[p_o[i - k] - 1];
            diff = value - lag_value;
            if (std::llabs(diff) > int_max){
              p_out[oi] = NA_INTEGER;
            } else {
              p_out[oi] = p_x[oi] - p_x[p_o[i - k] - 1];
            }
          }
          --group_count;
        }
      }
      Rf_unprotect(1);
      return out;
    }
    case REALSXP: {
        double fill_value = NA_REAL;
        if (fill_size >= 1){
            fill_value = Rf_asReal(fill);
        }
        SEXP out = Rf_protect(Rf_allocVector(REALSXP, size));
        double *p_x = REAL(x);
        double *p_out = REAL(out);
        if (k >= 0){
          for (int i = 0; i < size; ++i) {
            oi = p_o[i] - 1;
            // Start of new group?
            if (i > (running_group_size - 1)){
              ++j;
              running_group_size += p_sizes[j];
              group_count = 0;
            }
            if (group_count < k){
              p_out[oi] = fill_value;
            } else {
              p_out[oi] = p_x[oi] - p_x[p_o[i - k] - 1];
            }
            ++group_count;
          }
        } else {
          j = n_groups - 1;
          running_group_size = p_sizes[j];
          group_count = running_group_size - 1;
          for (int i = (size - 1); i >= 0; --i) {
            oi = p_o[i] - 1;
            // Start of new group?
            if ( (size - i) > running_group_size){
              --j;
              running_group_size += p_sizes[j];
              group_count = p_sizes[j] - 1;
            }
            if (group_count >= (p_sizes[j] + k)){
              p_out[oi] = fill_value;
            } else {
              p_out[oi] = p_x[oi] - p_x[p_o[i - k] - 1];
            }
            --group_count;
          }
        }
        Rf_unprotect(1);
        return out;
    }
    case VECSXP: {
      int num_col = Rf_length(x);
      const SEXP *p_x = VECTOR_PTR_RO(x);
      SEXP out = Rf_protect(Rf_allocVector(VECSXP, num_col));
      SHALLOW_DUPLICATE_ATTRIB(out, x);
      for (int i = 0; i < num_col; ++i){
        SET_VECTOR_ELT(out, i, Rf_protect(cpp_roll_diff_grouped(p_x[i], k, o, sizes, fill)));
      }
      Rf_unprotect(num_col + 1);
      return out;
    }
    default: {
      Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
      break;
    }
    }
}

// Alternative that tries to combine the grouped and ungrouped versions
// It's unfortunately a bit slower
// SEXP cpp_lag(SEXP x, int k, SEXP sizes, SEXP fill, SEXP o) {
//   int n_protections = 0;
//   int size = cpp_vector_size(x);
//   int o_size = Rf_length(o);
//   int fill_size = Rf_length(fill);
//   bool has_groups = !Rf_isNull(o);
//   if (has_groups && o_size != size){
//     Rf_error("x and o must both be the same length");
//   }
//   if (fill_size > 1){
//     Rf_error("fill size must be NULL or length 1");
//   }
//   if (k < 0){
//     Rf_error("k must be a non-negative integer");
//   }
//   SEXP total_size = Rf_protect(Rf_allocVector(INTSXP, 1));
//   ++n_protections;
//   INTEGER(total_size)[0] = size;
//   int *p_sizes = has_groups ? INTEGER(sizes): INTEGER(total_size);
//   int *p_o = has_groups ? INTEGER(o): INTEGER(Rf_protect(Rf_allocVector(INTSXP, 0)));
//   if (!has_groups) ++n_protections;
//   int oi, oi2;
//   int j = 0;
//   int group_count = 0;
//   int running_group_size = p_sizes[0];
//   bool new_group;
//   switch(TYPEOF(x)){
//   case LGLSXP:
//   case INTSXP: {
//     int fill_value = NA_INTEGER;
//     if (fill_size >= 1){
//       fill_value = Rf_asInteger(fill);
//     }
//     SEXP out = Rf_protect(Rf_duplicate(x));
//     ++n_protections;
//     int *p_x = INTEGER(x);
//     int *p_out = INTEGER(out);
//     for (int i = 0; i < size; ++i) {
//       oi = i;
//       oi2 = i - k;
//       if (has_groups){
//         oi = p_o[i] - 1;
//         oi2 = p_o[i - k] - 1;
//         new_group = (i > (running_group_size - 1));
//         j = j + new_group;
//         running_group_size = running_group_size + (p_sizes[j] * new_group);
//         group_count = group_count * (!new_group);
//       } else {
//         oi = i;
//         oi2 = i - k;
//       }
//       // Start of new group?
//       if (group_count < k){
//         p_out[oi] = fill_value;
//       } else {
//         p_out[oi] = p_x[oi2];
//       }
//       ++group_count;
//     }
//     Rf_unprotect(n_protections);
//     return out;
//   }
//   default: {
//     Rf_error("cpp_roll_lag_grouped cannot handle the supplied SEXP");
//     break;
//   }
//   }
// }
