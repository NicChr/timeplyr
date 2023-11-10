#include <cpp11.hpp>
#include <Rinternals.h>

#define R_NO_REMAP

// Returns true if all numbers are whole numbers
// otherwise false
// Returns NA when na_rm is false and the function can't find any
// non-whole numbers and there is at least 1 NA

[[cpp11::register]]
SEXP cpp_is_whole_num(SEXP x, double tol, bool na_rm) {
    R_xlen_t n = Rf_xlength(x);
    bool is_whole;
    double adiff;
    R_xlen_t n_na = 0;
    bool is_na;
    SEXP out = Rf_protect(Rf_allocVector(LGLSXP, 1));
    int *p_out = LOGICAL(out);
    p_out[0] = false;
    switch ( TYPEOF(x) ){
    case LGLSXP:
    case INTSXP: {
        p_out[0] = true;
        break;
    }
    case REALSXP: {
        // Re-initialise so that we can break when we find non-whole num
        p_out[0] = true;
        double *p_x = REAL(x);
        for (R_xlen_t i = 0; i < n; ++i) {
            adiff = std::fabs(p_x[i] - std::round(p_x[i]));
            is_whole = (adiff < tol);
            is_na = !(p_x[i] == p_x[i]);
            n_na += is_na;
            if (!is_whole && !is_na){
                p_out[0] = false;
                break;
            }
        }
        if (!na_rm && n_na > 0){
            p_out[0] = NA_LOGICAL;
            break;
        }
        break;
    }
    }
    Rf_unprotect(1);
    return out;
}
