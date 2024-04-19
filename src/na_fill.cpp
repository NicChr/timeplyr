#include "timeplyr_cpp.h"

// bool r_is_sorted(SEXP x) {
//     cpp11::function r_is_unsorted = cpp11::package("base")["is.unsorted"];
//     SEXP is_unsorted = Rf_protect(r_is_unsorted(x));
//     int *p_out = LOGICAL(is_unsorted);
//     bool out = p_out[0];
//     out = (out != NA_LOGICAL) && !out;
//     Rf_unprotect(1);
//     return out;
// }

[[cpp11::register]]
SEXP cpp_roll_na_fill(SEXP x, double fill_limit) {
    R_xlen_t size = Rf_xlength(x);
    fill_limit = std::fmax(fill_limit, 0);
    bool first_non_na = false;
    bool is_na;
    bool prev_is_not_na = false;
    R_xlen_t fill_count = 0;
    int n_protections = 0;
    switch(TYPEOF(x)){
    case LGLSXP:
    case INTSXP: {
        int fill = 0;
        SEXP out = Rf_protect(Rf_duplicate(x));
        ++n_protections;
        int *p_out = INTEGER(out);
        for (R_xlen_t i = 0; i < size; ++i) {
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
        Rf_unprotect(n_protections);
        return out;
    }
    case REALSXP: {
        double fill = 0;
        SEXP out = Rf_protect(Rf_duplicate(x));
        ++n_protections;
        double *p_out = REAL(out);
        for (R_xlen_t i = 0; i < size; ++i) {
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
        Rf_unprotect(n_protections);
        return out;
    }
    case STRSXP: {
      SEXP fill = Rf_protect(Rf_allocVector(STRSXP, 1));
      ++n_protections;
      SEXP out = Rf_protect(Rf_duplicate(x));
      ++n_protections;
      SEXP *p_out = STRING_PTR(out);
      SEXP *p_fill = STRING_PTR(fill);
      for (R_xlen_t i = 0; i < size; ++i) {
        is_na = (p_out[i] == NA_STRING);
        if (!first_non_na && !is_na){
          first_non_na = true;
        }
        // Resetting fill value
        // Are we in new NA run?
        if (is_na && first_non_na && prev_is_not_na){
          fill_count = 0;
          SET_STRING_ELT(fill, 0, p_out[i - 1]);
        }
        // Should we fill this NA value?
        if (is_na && first_non_na && fill_count < fill_limit){
          SET_STRING_ELT(out, i, p_fill[0]);
          ++fill_count;
        }
        prev_is_not_na = !is_na;
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
        SET_VECTOR_ELT(out, i, Rf_protect(cpp_roll_na_fill(p_x[i], fill_limit)));
        ++n_protections;
      }
      Rf_unprotect(n_protections);
      return out;
    }
    default: {
      Rf_unprotect(n_protections);
      Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
      break;
    }
    }
}

// o must be the order of groups, e.g order(g)
// sizes must be the sorted group sizes, e.g table(g), or count(df, g)[["g"]]

[[cpp11::register]]
SEXP cpp_roll_na_fill_grouped(SEXP x, SEXP o, SEXP sizes, double fill_limit) {
    int size = cpp_vector_size(x);
    int o_size = Rf_length(o);
    if (o_size != size){
        Rf_error("x and o must both be the same length");
    }
    int *p_sizes = INTEGER(sizes);
    int *p_o = INTEGER(o);
    fill_limit = std::fmax(fill_limit, 0);
    bool first_non_na = false;
    bool is_na;
    bool prev_is_not_na = false;
    int fill_count = 0;
    int oi;
    int j = 0;
    int running_group_size = p_sizes[0];
    switch(TYPEOF(x)){
    case LGLSXP:
    case INTSXP: {
        int fill = 0;
        SEXP out = Rf_protect(Rf_duplicate(x));
        int *p_out = INTEGER(out);
        for (int i = 0; i < size; ++i) {
            oi = p_o[i] - 1;
            // Start of new group?
            if (i > (running_group_size - 1)){
                running_group_size += p_sizes[++j];
                first_non_na = false;
                fill_count = 0;
            }
            is_na = (p_out[oi] == NA_INTEGER);
            if (!first_non_na && !is_na){
                first_non_na = true;
            }
            // Resetting fill value
            // Are we in new NA run?
            if (is_na && first_non_na && prev_is_not_na){
                fill_count = 0;
                fill = p_out[p_o[i - 1] - 1];
            }
            // Should we fill this NA value?
            if (is_na && first_non_na && fill_count < fill_limit){
                p_out[oi] = fill;
                ++fill_count;
            }
            prev_is_not_na = !is_na;
        }
        Rf_unprotect(1);
        return out;
    }
    case REALSXP: {
        double fill = 0;
        SEXP out = Rf_protect(Rf_duplicate(x));
        double *p_out = REAL(out);
        for (int i = 0; i < size; ++i) {
            oi = p_o[i] - 1;
            // Start of new group?
            if (i > (running_group_size - 1)){
                running_group_size += p_sizes[++j];
                first_non_na = false;
                fill_count = 0;
            }
            is_na = !(p_out[oi] == p_out[oi]);
            if (!first_non_na && !is_na){
                first_non_na = true;
            }
            // Resetting fill value
            // Are we in new NA run?
            if (is_na && first_non_na && prev_is_not_na){
                fill_count = 0;
                fill = p_out[p_o[i - 1] - 1];
            }
            // Should we fill this NA value?
            if (is_na && first_non_na && fill_count < fill_limit){
                p_out[oi] = fill;
                ++fill_count;
            }
            prev_is_not_na = !is_na;
        }
        Rf_unprotect(1);
        return out;
    }
    case STRSXP: {
        SEXP fill = Rf_protect(Rf_allocVector(STRSXP, 1));
        SEXP out = Rf_protect(Rf_duplicate(x));
        SEXP *p_out = STRING_PTR(out);
        SEXP *p_fill = STRING_PTR(fill);
        for (int i = 0; i < size; ++i) {
            oi = p_o[i] - 1;
            // Start of new group?
            if (i > (running_group_size - 1)){
                running_group_size += p_sizes[++j];
                first_non_na = false;
                fill_count = 0;
            }
            is_na = (p_out[oi] == NA_STRING);
            if (!first_non_na && !is_na){
                first_non_na = true;
            }
            // Resetting fill value
            // Are we in new NA run?
            if (is_na && first_non_na && prev_is_not_na){
                fill_count = 0;
                SET_STRING_ELT(fill, 0, p_out[p_o[i - 1] - 1]);
            }
            // Should we fill this NA value?
            if (is_na && first_non_na && fill_count < fill_limit){
                SET_STRING_ELT(out, oi, p_fill[0]);
                ++fill_count;
            }
            prev_is_not_na = !is_na;
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
        SET_VECTOR_ELT(out, i, Rf_protect(cpp_roll_na_fill_grouped(p_x[i], o, sizes, fill_limit)));
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

// This implementation is fast but requires groups are pre-sorted

// SEXP cpp_roll_na_fill_grouped(SEXP x, SEXP g, double fill_limit, bool check_sorted) {
//     R_xlen_t size = Rf_xlength(x);
//     R_xlen_t g_size = Rf_xlength(g);
//     bool has_groups = g_size > 0;
//     if (has_groups && g_size != size){
//         Rf_error("x and g must both be the same length");
//     }
//     SEXP groups = Rf_protect(Rf_coerceVector(g, INTSXP));
//     // This will always evaluate to TRUE when g contains NA
//     if (check_sorted && !r_is_sorted(groups)){
//         Rf_unprotect(1);
//         Rf_error("g must be a sorted integer vector");
//     }
//     int *p_groups = INTEGER(groups);
//     fill_limit = std::fmax(fill_limit, 0);
//     bool first_non_na = false;
//     bool is_na;
//     bool prev_is_not_na = false;
//     R_xlen_t fill_count = 0;
//
//     switch(TYPEOF(x)){
//     case LGLSXP:
//     case INTSXP: {
//         int fill;
//         SEXP out = Rf_protect(Rf_duplicate(x));
//         int *p_out = INTEGER(out);
//         for (R_xlen_t i = 0; i < size; ++i) {
//             // Start of new group?
//             if (has_groups && i > 0 && p_groups[i] != p_groups[i - 1]){
//                 first_non_na = false;
//                 fill_count = 0;
//             }
//             is_na = (p_out[i] == NA_INTEGER);
//             if (!first_non_na && !is_na){
//                 first_non_na = true;
//             }
//             // Resetting fill value
//             // Are we in new NA run?
//             if (is_na && first_non_na && prev_is_not_na){
//                 fill_count = 0;
//                 fill = p_out[i - 1];
//             }
//             // Should we fill this NA value?
//             if (is_na && first_non_na && fill_count < fill_limit){
//                 p_out[i] = fill;
//                 ++fill_count;
//             }
//             prev_is_not_na = !is_na;
//         }
//         Rf_unprotect(2);
//         return out;
//     }
//     case REALSXP: {
//         double fill;
//         SEXP out = Rf_protect(Rf_duplicate(x));
//         double *p_out = REAL(out);
//         for (R_xlen_t i = 0; i < size; ++i) {
//             // Start of new group?
//             if (has_groups && i > 0 && p_groups[i] != p_groups[i - 1]){
//                 first_non_na = false;
//                 fill_count = 0;
//             }
//             is_na = !(p_out[i] == p_out[i]);
//             if (!first_non_na && !is_na){
//                 first_non_na = true;
//             }
//             // Resetting fill value
//             // Are we in new NA run?
//             if (is_na && first_non_na && prev_is_not_na){
//                 fill_count = 0;
//                 fill = p_out[i - 1];
//             }
//             // Should we fill this NA value?
//             if (is_na && first_non_na && fill_count < fill_limit){
//                 p_out[i] = fill;
//                 ++fill_count;
//             }
//             prev_is_not_na = !is_na;
//         }
//         Rf_unprotect(2);
//         return out;
//     }
//     case STRSXP: {
//         SEXP fill = Rf_protect(Rf_allocVector(STRSXP, 1));
//         SEXP out = Rf_protect(Rf_duplicate(x));
//         SEXP *p_out = STRING_PTR(out);
//         for (R_xlen_t i = 0; i < size; ++i) {
//             // Start of new group?
//             if (has_groups && i > 0 && p_groups[i] != p_groups[i - 1]){
//                 first_non_na = false;
//                 fill_count = 0;
//             }
//             is_na = (p_out[i] == NA_STRING);
//             if (!first_non_na && !is_na){
//                 first_non_na = true;
//             }
//             // Resetting fill value
//             // Are we in new NA run?
//             if (is_na && first_non_na && prev_is_not_na){
//                 // SET_STRING_ELT(fill, 0, Rf_mkChar(CHAR(STRING_ELT(out, i - 1))));
//                 fill_count = 0;
//                 SET_STRING_ELT(fill, 0, STRING_ELT(out, i - 1));
//             }
//             // Should we fill this NA value?
//             if (is_na && first_non_na && fill_count < fill_limit){
//                 // SET_STRING_ELT(out, i, Rf_mkChar(CHAR(STRING_ELT(fill, 0))));
//                 SET_STRING_ELT(out, i, STRING_ELT(fill, 0));
//                 ++fill_count;
//             }
//             prev_is_not_na = !is_na;
//         }
//         Rf_unprotect(3);
//         return out;
//     }
//     default: {
//         Rf_unprotect(1);
//         Rf_error("cpp_roll_na_fill_grouped cannot handle the supplied SEXP");
//         break;
//     }
//     }
// }
