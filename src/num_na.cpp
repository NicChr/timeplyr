#include <cpp11.hpp>
#include <Rinternals.h>

#define R_NO_REMAP
#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))

[[cpp11::register]]
SEXP cpp_num_na(SEXP x){
  R_xlen_t n = Rf_xlength(x);
  R_xlen_t count = 0;
  int n_protections = 0;
  // This nicely handles NULL and avoids loop too
  if (n == 0){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 1));
    int *p_out = INTEGER(out);
    int n_na = 0;
    p_out[0] = n_na;
    Rf_unprotect(1);
    return out;
  }
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    for (R_xlen_t i = 0; i < n; i++){
      count += (p_x[i] == NA_INTEGER);
    }
    break;
  }
  case REALSXP: {
    double *p_x = REAL(x);
    for (R_xlen_t i = 0; i < n; i++){
      // Because NaN == NaN is false
      count += !(p_x[i] == p_x[i]);
    }
    break;
  }
  case STRSXP: {
    SEXP *p_x = STRING_PTR(x);
    for (R_xlen_t i = 0; i < n; i++){
      count += (p_x[i] == NA_STRING);
    }
    break;
  }
  case RAWSXP: {
    break;
  }
  case CPLXSXP: {
    Rcomplex *p_x = COMPLEX(x);
    for (R_xlen_t i = 0; i < n; i++){
      count +=
        ( !((p_x[i]).r == (p_x[i]).r) ) ||
        ( !((p_x[i]).i == (p_x[i]).i) );
  }
    break;
  }
  case VECSXP: {
    const SEXP *p_x = VECTOR_PTR_RO(x);
    int num_col = Rf_length(x);
    int num_row = 0;
    if (num_col > 0){
      num_row = Rf_xlength(p_x[0]);
    }
    // Check that list elements are all equal in length
    // and all atomic vectors and not NULL
    for (int j = 0; j < num_col; j++){
      if (!Rf_isVectorAtomic(p_x[j])){
        Rf_error("All list elements must be atomic vectors");
      }
      if (Rf_xlength(p_x[j]) != num_row){
        Rf_error("All list elements must be of equal length");
      }
    }
    SEXP n_nas = Rf_protect(Rf_allocVector(INTSXP, num_row));
    ++n_protections;
    int *p_n_nas = INTEGER(n_nas);
    memset(p_n_nas, 0, num_row * sizeof(int));
    for (int j = 0; j < (num_col - 1) ; j++){
      switch ( TYPEOF(p_x[j]) ){
      case LGLSXP:
      case INTSXP: {
        int *p_xj = INTEGER(p_x[j]);
        for (R_xlen_t i = 0; i < num_row; i++){
          p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_INTEGER);
        }
        break;
      }
      case REALSXP: {
        double *p_xj = REAL(p_x[j]);
        for (R_xlen_t i = 0; i < num_row; i++){
          p_n_nas[i] = p_n_nas[i] + !(p_xj[i] == p_xj[i]);
        }
        break;
      }
      case STRSXP: {
        SEXP *p_xj = STRING_PTR(p_x[j]);
        for (R_xlen_t i = 0; i < num_row; i++){
          p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_STRING);
        }
        break;
      }
      case RAWSXP: {
        break;
      }
      case CPLXSXP: {
        Rcomplex *p_xj = COMPLEX(p_x[j]);
        for (R_xlen_t i = 0; i < num_row; i++){
          p_n_nas[i] = p_n_nas[i] +
            ( !((p_xj[i]).r == (p_xj[i]).r) ) ||
            ( !((p_xj[i]).i == (p_xj[i]).i) );
        }
        break;
      }
      }
    }
    switch ( TYPEOF(p_x[num_col - 1]) ){
    case LGLSXP:
    case INTSXP: {
      int *p_xj = INTEGER(p_x[num_col - 1]);
      for (R_xlen_t i = 0; i < num_row; i++){
        p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_INTEGER);
        count += (p_n_nas[i] == num_col);
      }
      break;
    }
    case REALSXP: {
      double *p_xj = REAL(p_x[num_col - 1]);
      for (R_xlen_t i = 0; i < num_row; i++){
        p_n_nas[i] = p_n_nas[i] + !(p_xj[i] == p_xj[i]);
        count += (p_n_nas[i] == num_col);
      }
      break;
    }
    case STRSXP: {
      SEXP *p_xj = STRING_PTR(p_x[num_col - 1]);
      for (R_xlen_t i = 0; i < num_row; i++){
        p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_STRING);
        count += (p_n_nas[i] == num_col);
      }
      break;
    }
    case RAWSXP: {
      break;
    }
    case CPLXSXP: {
      Rcomplex *p_xj = COMPLEX(p_x[num_col - 1]);
      for (R_xlen_t i = 0; i < num_row; i++){
        p_n_nas[i] = p_n_nas[i] +
          ( !((p_xj[i]).r == (p_xj[i]).r) ) ||
          ( !((p_xj[i]).i == (p_xj[i]).i) );
        count += (p_n_nas[i] == num_col);
      }
      break;
    }
    }
    break;
  }
  // case VECSXP: {
  //   const SEXP *p_x = VECTOR_PTR_RO(x);
  //   int num_col = Rf_length(x);
  //   int num_row;
  //   if (num_col > 0){
  //     num_row = Rf_xlength(p_x[0]);
  //   }
  //   // Check that list elements are all equal in length
  //   // and all atomic vectors and not NULL
  //   for (int j = 0; j < num_col; j++){
  //     if (!Rf_isVectorAtomic(p_x[j])){
  //       Rf_error("All list elements must be atomic vectors");
  //     }
  //     if (Rf_xlength(p_x[j]) != num_row){
  //       Rf_error("All list elements must be of equal length");
  //     }
  //   }
  //   SEXP n_nas = Rf_protect(Rf_allocVector(INTSXP, num_row));
  //   ++n_protections;
  //   int *p_n_nas = INTEGER(n_nas);
  //   memset(p_n_nas, 0, sizeof(int) * num_row);
  //   for (int j = 0; j < (num_col - 1) ; j++){
  //     int *p_xj = INTEGER(p_x[j]);
  //     for (R_xlen_t i = 0; i < num_row; i++){
  //       p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_INTEGER);
  //     }
  //   }
  //   int *p_xj = INTEGER(p_x[num_col - 1]);
  //   for (R_xlen_t i = 0; i < num_row; i++){
  //     p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_INTEGER);
  //     count += (p_n_nas[i] == num_col);
  //   }
  //   break;
  // }
  default: {
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
    break;
  }
  }
  if (count <= std::numeric_limits<int>::max()){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 1));
    ++n_protections;
    int *p_out = INTEGER(out);
    p_out[0] = int(count);
    Rf_unprotect(n_protections);
    return out;
  } else {
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, 1));
    ++n_protections;
    double *p_out = REAL(out);
    p_out[0] = double(count);
    Rf_unprotect(n_protections);
    return out;
  }
}

// SEXP cpp_is_na(SEXP x){
//   R_xlen_t n = Rf_xlength(x);
//   SEXP out = Rf_protect(Rf_allocVector(LGLSXP, n));
//   int *p_out = LOGICAL(out);
//   switch ( TYPEOF(x) ){
//   case LGLSXP:
//   case INTSXP: {
//     int *p_x = INTEGER(x);
//     for (R_xlen_t i = 0; i < n; i++){
//       p_out[i] = (p_x[i] == NA_INTEGER);
//     }
//     break;
//   }
//   case REALSXP: {
//     double *p_x = REAL(x);
//     for (R_xlen_t i = 0; i < n; i++){
//       // Because NaN == NaN is false
//       p_out[i] = !(p_x[i] == p_x[i]);
//     }
//     break;
//   }
//   case STRSXP: {
//     SEXP *p_x = STRING_PTR(x);
//     for (R_xlen_t i = 0; i < n; i++){
//       p_out[i] = (p_x[i] == NA_STRING);
//     }
//     break;
//   }
//   case RAWSXP: {
//     break;
//   }
//   case CPLXSXP: {
//     Rcomplex *p_x = COMPLEX(x);
//     for (R_xlen_t i = 0; i < n; i++){
//       p_out[i] =
//         ( !((p_x[i]).r == (p_x[i]).r) ) ||
//         ( !((p_x[i]).i == (p_x[i]).i) );
//     }
//     break;
//   }
//   default: {
//     Rf_unprotect(1);
//     Rf_error("cpp_is_na cannot handle the supplied SEXP");
//     break;
//   }
//   }
//   Rf_unprotect(1);
//   return out;
// }
