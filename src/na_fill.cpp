#include <cpp11.hpp>
#include <Rinternals.h>

#define R_NO_REMAP

bool r_is_sorted(SEXP x) {
    cpp11::function r_is_unsorted = cpp11::package("base")["is.unsorted"];
    SEXP is_unsorted = Rf_protect(r_is_unsorted(x));
    int *p_out = LOGICAL(is_unsorted);
    bool out = p_out[0];
    out = (out != NA_LOGICAL) && !out;
    Rf_unprotect(1);
    return out;
}

[[cpp11::register]]
SEXP cpp_roll_na_fill_grouped(SEXP x, SEXP g, double fill_limit, bool check_sorted) {
    R_xlen_t size = Rf_xlength(x);
    R_xlen_t g_size = Rf_xlength(g);
    bool has_groups = g_size > 0;
    if (has_groups && g_size != size){
        Rf_error("x and g must both be the same length");
    }
    SEXP groups = Rf_protect(Rf_coerceVector(g, INTSXP));
    // This will always evaluate to TRUE when g contains NA
    if (check_sorted && !r_is_sorted(groups)){
        Rf_unprotect(1);
        Rf_error("g must be a sorted integer vector");
    }
    int *p_groups = INTEGER(groups);
    fill_limit = std::fmax(fill_limit, 0);
    bool first_non_na = false;
    bool is_na;
    bool prev_is_not_na = false;
    R_xlen_t fill_count = 0;

    switch(TYPEOF(x)){
    case LGLSXP:
    case INTSXP: {
        int fill;
        SEXP out = Rf_protect(Rf_duplicate(x));
        int *p_out = INTEGER(out);
        for (R_xlen_t i = 0; i < size; ++i) {
            // Start of new group?
            if (has_groups && i > 0 && p_groups[i] != p_groups[i - 1]){
                first_non_na = false;
                fill_count = 0;
            }
            is_na = (p_out[i] == NA_INTEGER);
            if (!first_non_na && !is_na){
                first_non_na = true;
            }
            // Resetting fill value
            // Are we in new NA run?
            if (is_na && first_non_na && prev_is_not_na){
                fill_count = 0;
                fill = p_out[i - 1];
            }
            // Should we fill this NA value?
            if (is_na && first_non_na && fill_count < fill_limit){
                p_out[i] = fill;
                ++fill_count;
            }
            prev_is_not_na = !is_na;
        }
        Rf_unprotect(2);
        return out;
    }
    case REALSXP: {
        double fill;
        SEXP out = Rf_protect(Rf_duplicate(x));
        double *p_out = REAL(out);
        for (R_xlen_t i = 0; i < size; ++i) {
            // Start of new group?
            if (has_groups && i > 0 && p_groups[i] != p_groups[i - 1]){
                first_non_na = false;
                fill_count = 0;
            }
            is_na = !(p_out[i] == p_out[i]);
            if (!first_non_na && !is_na){
                first_non_na = true;
            }
            // Resetting fill value
            // Are we in new NA run?
            if (is_na && first_non_na && prev_is_not_na){
                fill_count = 0;
                fill = p_out[i - 1];
            }
            // Should we fill this NA value?
            if (is_na && first_non_na && fill_count < fill_limit){
                p_out[i] = fill;
                ++fill_count;
            }
            prev_is_not_na = !is_na;
        }
        Rf_unprotect(2);
        return out;
    }
    case STRSXP: {
        SEXP fill = Rf_protect(Rf_allocVector(STRSXP, 1));
        SEXP out = Rf_protect(Rf_duplicate(x));
        SEXP *p_out = STRING_PTR(out);
        for (R_xlen_t i = 0; i < size; ++i) {
            // Start of new group?
            if (has_groups && i > 0 && p_groups[i] != p_groups[i - 1]){
                first_non_na = false;
                fill_count = 0;
            }
            is_na = (p_out[i] == NA_STRING);
            if (!first_non_na && !is_na){
                first_non_na = true;
            }
            // Resetting fill value
            // Are we in new NA run?
            if (is_na && first_non_na && prev_is_not_na){
                // SET_STRING_ELT(fill, 0, Rf_mkChar(CHAR(STRING_ELT(out, i - 1))));
                fill_count = 0;
                SET_STRING_ELT(fill, 0, STRING_ELT(out, i - 1));
            }
            // Should we fill this NA value?
            if (is_na && first_non_na && fill_count < fill_limit){
                // SET_STRING_ELT(out, i, Rf_mkChar(CHAR(STRING_ELT(fill, 0))));
                SET_STRING_ELT(out, i, STRING_ELT(fill, 0));
                ++fill_count;
            }
            prev_is_not_na = !is_na;
        }
        Rf_unprotect(3);
        return out;
    }
    default: {
        Rf_unprotect(1);
        Rf_error("cpp_roll_na_fill_grouped cannot handle the supplied SEXP");
        break;
    }
    }
}
