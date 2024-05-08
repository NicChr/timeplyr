#include "timeplyr_cpp.h"

// o must be the order of groups, e.g order(g)
// sizes must be the sorted group sizes, e.g table(g), or count(df, g)[["g"]]

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
