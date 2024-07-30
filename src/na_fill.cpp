#include "timeplyr_cpp.h"

[[cpp11::register]]
SEXP cpp_roll_na_fill(SEXP x, double fill_limit) {
  R_xlen_t size = Rf_xlength(x);
  fill_limit = std::fmax(fill_limit, 0);
  bool first_non_na = false;
  bool is_na;
  bool prev_is_not_na = false;
  R_xlen_t fill_count = 0;
  int n_prot = 0;
  SEXP out;
  switch(TYPEOF(x)){
  case NILSXP: {
    out = Rf_protect(R_NilValue);
    ++n_prot;
    break;
  }
  case LGLSXP:
  case INTSXP: {
    int fill = 0;
    out = Rf_protect(Rf_duplicate(x));
    ++n_prot;
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
    break;
  }
  case REALSXP: {
    double fill = 0;
    out = Rf_protect(Rf_duplicate(x));
    ++n_prot;
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
    break;
  }
  case STRSXP: {
    SEXP fill = Rf_protect(Rf_mkChar(""));
    ++n_prot;
    out = Rf_protect(Rf_duplicate(x));
    ++n_prot;
    const SEXP *p_out = STRING_PTR_RO(out);
    for (R_xlen_t i = 0; i < size; ++i) {
      is_na = (p_out[i] == NA_STRING);
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
        SET_STRING_ELT(out, i, fill);
        ++fill_count;
      }
      prev_is_not_na = !is_na;
    }
    break;
  }
  case VECSXP: {
    const SEXP *p_x = VECTOR_PTR_RO(x);
    out = Rf_protect(Rf_allocVector(VECSXP, size));
    ++n_prot;
    SHALLOW_DUPLICATE_ATTRIB(out, x);
    for (R_xlen_t i = 0; i < size; ++i){
      SET_VECTOR_ELT(out, i, cpp_roll_na_fill(p_x[i], fill_limit));
    }
    break;
  }
  default: {
    Rf_unprotect(n_prot);
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
  }
  }
  Rf_unprotect(n_prot);
  return out;
}

// o must be the order of groups, e.g order(g)
// sizes must be the sorted group sizes, e.g table(g), or count(df, g)[["g"]]

[[cpp11::register]]
SEXP cpp_roll_na_fill_grouped(SEXP x, SEXP o, SEXP sizes, double fill_limit) {
  int size = Rf_length(x);
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
  int n_prot = 0;
  SEXP out;
  switch(TYPEOF(x)){
  case NILSXP: {
    out = Rf_protect(R_NilValue);
    ++n_prot;
    break;
  }
  case LGLSXP:
  case INTSXP: {
    int fill = 0;
    out = Rf_protect(Rf_duplicate(x));
    ++n_prot;
    int *p_out = INTEGER(out);
    for (int i = 0; i < size; ++i) {
      oi = p_o[i] - 1;
      // Start of new group?
      if (i >= running_group_size){
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
    break;
  }
  case REALSXP: {
    double fill = 0;
    out = Rf_protect(Rf_duplicate(x));
    ++n_prot;
    double *p_out = REAL(out);
    for (int i = 0; i < size; ++i) {
      oi = p_o[i] - 1;
      // Start of new group?
      if (i >= running_group_size){
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
    break;
  }
  case STRSXP: {
    SEXP fill = Rf_protect(Rf_mkChar(""));
    ++n_prot;
    out = Rf_protect(Rf_duplicate(x));
    ++n_prot;
    const SEXP *p_out = STRING_PTR_RO(out);
    for (int i = 0; i < size; ++i) {
      oi = p_o[i] - 1;
      // Start of new group?
      if (i >= running_group_size){
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
        fill = p_out[p_o[i - 1] - 1];
      }
      // Should we fill this NA value?
      if (is_na && first_non_na && fill_count < fill_limit){
        SET_STRING_ELT(out, oi, fill);
        ++fill_count;
      }
      prev_is_not_na = !is_na;
    }
    break;
  }
  case VECSXP: {
    const SEXP *p_x = VECTOR_PTR_RO(x);
    out = Rf_protect(Rf_allocVector(VECSXP, size));
    ++n_prot;
    SHALLOW_DUPLICATE_ATTRIB(out, x);
    for (int i = 0; i < size; ++i){
      SET_VECTOR_ELT(out, i, cpp_roll_na_fill_grouped(p_x[i], o, sizes, fill_limit));
    }
    break;
  }
  default: {
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
  }
  }
  Rf_unprotect(n_prot);
  return out;
}
