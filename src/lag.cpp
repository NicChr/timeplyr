#include <cpp11.hpp>
#include <Rinternals.h>

#define R_NO_REMAP

// o must be the order of groups, e.g order(g)
// sizes must be the sorted group sizes, e.g table(g), or count(df, g)[["g"]]

[[cpp11::register]]
SEXP cpp_roll_lag(SEXP x, int k, SEXP fill) {
    R_xlen_t size = Rf_xlength(x);
    int fill_size = Rf_length(fill);
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
        int *p_x = INTEGER(x);
        int *p_out = INTEGER(out);
        for (R_xlen_t i = 0; i < size; ++i) {
            if (i < k){
                p_out[i] = fill_value;
            } else {
                p_out[i] = p_x[i - k];
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
        SEXP out = Rf_protect(Rf_duplicate(x));
        double *p_x = REAL(x);
        double *p_out = REAL(out);
        for (R_xlen_t i = 0; i < size; ++i) {
            if (i < k){
                p_out[i] = fill_value;
            } else {
                p_out[i] = p_x[i - k];
            }
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
        for (R_xlen_t i = 0; i < size; ++i) {
            if (i < k){
                SET_STRING_ELT(out, i, STRING_ELT(fill_sexp, 0));
            } else {
                SET_STRING_ELT(out, i, STRING_ELT(x, i - k));
            }
        }
        Rf_unprotect(2);
        return out;
    }
    default: {
        Rf_error("cpp_roll_lag cannot handle the supplied SEXP");
        break;
    }
    }
}

[[cpp11::register]]
SEXP cpp_roll_lead(SEXP x, int k, SEXP fill) {
    R_xlen_t size = Rf_xlength(x);
    int fill_size = Rf_length(fill);
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
        int *p_x = INTEGER(x);
        int *p_out = INTEGER(out);
        for (R_xlen_t i = 0; i < size; ++i) {
            if (i >= (size - k)){
                p_out[i] = fill_value;
            } else {
                p_out[i] = p_x[i + k];
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
        SEXP out = Rf_protect(Rf_duplicate(x));
        double *p_x = REAL(x);
        double *p_out = REAL(out);
        for (R_xlen_t i = 0; i < size; ++i) {
            if (i >= (size - k)){
                p_out[i] = fill_value;
            } else {
                p_out[i] = p_x[i + k];
            }
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
        for (R_xlen_t i = 0; i < size; ++i) {
            if (i >= (size - k)){
                SET_STRING_ELT(out, i, STRING_ELT(fill_sexp, 0));
            } else {
                SET_STRING_ELT(out, i, STRING_ELT(x, i + k));
            }
        }
        Rf_unprotect(2);
        return out;
    }
    default: {
        Rf_error("cpp_roll_lead cannot handle the supplied SEXP");
        break;
    }
    }
}


[[cpp11::register]]
SEXP cpp_roll_lag_grouped(SEXP x, int k, SEXP o, SEXP sizes, SEXP fill) {
    int size = Rf_length(x);
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
    default: {
        Rf_error("cpp_roll_lag_grouped cannot handle the supplied SEXP");
        break;
    }
    }
}

[[cpp11::register]]
SEXP cpp_roll_lead_grouped(SEXP x, int k, SEXP o, SEXP sizes, SEXP fill) {
    int size = Rf_length(x);
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
    default: {
        Rf_error("cpp_roll_lead_grouped cannot handle the supplied SEXP");
        break;
    }
    }
}

[[cpp11::register]]
SEXP cpp_roll_diff(SEXP x, int k, SEXP fill) {
    R_xlen_t size = Rf_xlength(x);
    int fill_size = Rf_length(fill);
    if (fill_size > 1){
        Rf_error("fill size must be NULL or length 1");
    }
    switch(TYPEOF(x)){
    case LGLSXP:
    case INTSXP: {
        int fill_value = NA_INTEGER;
        if (fill_size >= 1){
            fill_value = Rf_asInteger(fill);
        }
        SEXP out = Rf_protect(Rf_allocVector(INTSXP, size));
        int *p_x = INTEGER(x);
        int *p_out = INTEGER(out);
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
        Rf_unprotect(1);
        return out;
    }
    default: {
        Rf_error("cpp_roll_diff cannot handle the supplied SEXP");
        break;
    }
    }
}

[[cpp11::register]]
SEXP cpp_roll_diff_grouped(SEXP x, int k, SEXP o, SEXP sizes, SEXP fill) {
    int size = Rf_length(x);
    int o_size = Rf_length(o);
    int fill_size = Rf_length(fill);
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
    case LGLSXP:
    case INTSXP: {
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
                } else {
                    p_out[oi] = p_x[oi] - p_x[p_o[i - k] - 1];
                }
                ++group_count;
            }
        } else {
            group_count = running_group_size - 1;
            for (int i = (size - 1); i >= 0; --i) {
                oi = p_o[i] - 1;
                // Start of new group?
                if (i > (running_group_size - 1)){
                    group_count = (p_sizes[j] - 1);
                    ++j;
                    running_group_size += p_sizes[j];
                }
                if (group_count >= (size + k)){
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
    case REALSXP: {
        double fill_value = NA_REAL;
        // k = std::min(k, p_sizes[j]);
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
            group_count = running_group_size - 1;
            for (int i = (size - 1); i >= 0; --i) {
                oi = p_o[i] - 1;
                // Start of new group?
                if (i > (running_group_size - 1)){
                    group_count = (p_sizes[j] - 1);
                    ++j;
                    running_group_size += p_sizes[j];
                }
                if (group_count >= (size + k)){
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
    default: {
        Rf_error("cpp_roll_diff_grouped cannot handle the supplied SEXP");
        break;
    }
    }
}
