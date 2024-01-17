#ifdef _OPENMP
#include <omp.h>
  #define OMP_NUM_PROCS omp_get_num_procs()
  #define OMP_THREAD_LIMIT omp_get_thread_limit()
  #define OMP_MAX_THREADS omp_get_max_threads()
// #if _OPENMP >= 201307
//   #define OMP_VER_4
// #endif
#else
  #define OMP_NUM_PROCS 1
  #define OMP_THREAD_LIMIT 1
  #define OMP_MAX_THREADS 1
#endif

// #ifdef OMP_VER_4
// #define SAFE_SIMD _Pragma("omp simd")
// #define SAFE_FOR_SIMD _Pragma("omp for simd")
// #define SAFE_PARALLEL_FOR_SIMD _Pragma("omp parallel for simd")
// #else
// #define SAFE_SIMD
// #define SAFE_FOR_SIMD
// #define SAFE_PARALLEL_FOR_SIMD
// #endif

#include <cpp11.hpp>
#include <Rinternals.h>

#define R_NO_REMAP
#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))

// SEXP cpp_get_option(SEXP option){
//   return Rf_GetOption1(Rf_installChar(STRING_ELT(option, 0)));
//   // SEXP out = Rf_protect(Rf_duplicate(Rf_GetOption1(Rf_installTrChar(STRING_ELT(option, 0)))));
//   // if (Rf_isNull(out)){
//   //   Rf_unprotect(1);
//   //   return default_value;
//   // } else {
//   //   Rf_unprotect(1);
//   //   return out;
//   // }
// }

int num_cores(){
  int out = Rf_asInteger(Rf_GetOption1(Rf_installChar(Rf_mkChar("timeplyr.cores"))));
  if (out >= 1){
    int max_cores = omp_get_max_threads();
    return out > max_cores ? max_cores : out;
  } else {
    return 1;
  }
  // int out = 1;
  // SEXP n_cores = Rf_protect(Rf_duplicate(Rf_GetOption1(Rf_installChar(Rf_mkChar("timeplyr.cores")))));
  // if (Rf_isNull(n_cores)){
  //   Rf_unprotect(1);
  //   return out;
  // } else {
  //   Rf_unprotect(1);
  //   return Rf_asInteger(n_cores);
  // }
}

[[cpp11::register]]
SEXP cpp_num_na(SEXP x){
  R_xlen_t n = Rf_xlength(x);
  R_xlen_t count = 0;
  int n_protections = 0;
  // This nicely handles NULL and avoids loop too
  if (n == 0){
    return Rf_ScalarInteger(count);
  }
  int cores;
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    if (n >= 100000){
      cores = num_cores();
      #pragma omp parallel for simd num_threads(cores) reduction(+:count)
      // #pragma omp parallel for simd num_threads(Rf_asInteger(cpp11::package("base")["getOption"]("timeplyr.cores", 1))) reduction(+:count)
      for (R_xlen_t i = 0; i < n; ++i){
        count += (p_x[i] == NA_INTEGER);
      }
    } else {
      for (R_xlen_t i = 0; i < n; ++i){
        count += (p_x[i] == NA_INTEGER);
      }
    }
    break;
  }
  case REALSXP: {
    double *p_x = REAL(x);
    if (n >= 100000){
      cores = num_cores();
      #pragma omp parallel for simd num_threads(cores) reduction(+:count)
      for (R_xlen_t i = 0; i < n; ++i){
        count += (p_x[i] != p_x[i]);
      }
    } else {
      for (R_xlen_t i = 0; i < n; ++i){
        count += (p_x[i] != p_x[i]);
      }
    }
    break;
  }
  case STRSXP: {
    SEXP *p_x = STRING_PTR(x);
    if (n >= 100000){
      cores = num_cores();
      #pragma omp parallel for simd num_threads(cores) reduction(+:count)
      for (R_xlen_t i = 0; i < n; ++i){
        count += (p_x[i] == NA_STRING);
      }
    } else {
      for (R_xlen_t i = 0; i < n; ++i){
        count += (p_x[i] == NA_STRING);
      }
    }
    break;
  }
  case RAWSXP: {
    break;
  }
  case CPLXSXP: {
    Rcomplex *p_x = COMPLEX(x);
    if (n >= 100000){
      cores = num_cores();
      #pragma omp parallel for simd num_threads(cores) reduction(+:count)
      for (R_xlen_t i = 0; i < n; ++i){
        count +=
          ( !((p_x[i]).r == (p_x[i]).r) ) ||
          ( !((p_x[i]).i == (p_x[i]).i) );
      }
    } else {
      for (R_xlen_t i = 0; i < n; ++i){
        count +=
          ( !((p_x[i]).r == (p_x[i]).r) ) ||
          ( !((p_x[i]).i == (p_x[i]).i) );
      }
    }
    break;
  }
  case VECSXP: {
    cores = num_cores();
    const SEXP *p_x = VECTOR_PTR_RO(x);
    int num_col = Rf_length(x);
    R_xlen_t num_row = 0;
    if (num_col > 0){
      num_row = Rf_xlength(p_x[0]);
    }
    // Check that list elements are all equal in length
    // and all atomic vectors and not NULL
    for (int j = 0; j < num_col; ++j){
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
    for (int j = 0; j < (num_col - 1) ; ++j){
      switch ( TYPEOF(p_x[j]) ){
      case LGLSXP:
      case INTSXP: {
        int *p_xj = INTEGER(p_x[j]);
        #pragma omp parallel for simd num_threads(cores) reduction(+:count)
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_INTEGER);
        }
        break;
      }
      case REALSXP: {
        double *p_xj = REAL(p_x[j]);
        #pragma omp parallel for simd num_threads(cores) reduction(+:count)
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_n_nas[i] = p_n_nas[i] + !(p_xj[i] == p_xj[i]);
        }
        break;
      }
      case STRSXP: {
        SEXP *p_xj = STRING_PTR(p_x[j]);
        #pragma omp parallel for simd num_threads(cores) reduction(+:count)
        for (R_xlen_t i = 0; i < num_row; ++i){
          p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_STRING);
        }
        break;
      }
      case RAWSXP: {
        break;
      }
      case CPLXSXP: {
        Rcomplex *p_xj = COMPLEX(p_x[j]);
        #pragma omp parallel for simd num_threads(cores) reduction(+:count)
        for (R_xlen_t i = 0; i < num_row; ++i){
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
      #pragma omp parallel for simd num_threads(cores) reduction(+:count)
      for (R_xlen_t i = 0; i < num_row; ++i){
        p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_INTEGER);
        count += (p_n_nas[i] == num_col);
      }
      break;
    }
    case REALSXP: {
      double *p_xj = REAL(p_x[num_col - 1]);
      #pragma omp parallel for simd num_threads(cores) reduction(+:count)
      for (R_xlen_t i = 0; i < num_row; ++i){
        p_n_nas[i] = p_n_nas[i] + !(p_xj[i] == p_xj[i]);
        count += (p_n_nas[i] == num_col);
      }
      break;
    }
    case STRSXP: {
      SEXP *p_xj = STRING_PTR(p_x[num_col - 1]);
      #pragma omp parallel for simd num_threads(cores) reduction(+:count)
      for (R_xlen_t i = 0; i < num_row; ++i){
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
      #pragma omp parallel for simd num_threads(cores) reduction(+:count)
      for (R_xlen_t i = 0; i < num_row; ++i){
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
  //   for (int j = 0; j < num_col; ++j){
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
  //   for (int j = 0; j < (num_col - 1) ; ++j){
  //     int *p_xj = INTEGER(p_x[j]);
  //     for (R_xlen_t i = 0; i < num_row; ++i){
  //       p_n_nas[i] = p_n_nas[i] + (p_xj[i] == NA_INTEGER);
  //     }
  //   }
  //   int *p_xj = INTEGER(p_x[num_col - 1]);
  //   for (R_xlen_t i = 0; i < num_row; ++i){
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
    Rf_unprotect(n_protections);
    return Rf_ScalarInteger(count);
  } else {
    Rf_unprotect(n_protections);
    return Rf_ScalarReal(count);
  }
}

// Memory-efficient which(is.na(x))

[[cpp11::register]]
SEXP cpp_which_na(SEXP x){
  R_xlen_t n = Rf_xlength(x);
  // R_xlen_t count = 0;
  // switch ( TYPEOF(x) ){
  // case LGLSXP:
  // case INTSXP: {
  //   break;
  // }
  // case REALSXP: {
  //   break;
  // }
  // case STRSXP: {
  //   break;
  // }
  // case RAWSXP: {
  //   break;
  // }
  // case CPLXSXP: {
  //   break;
  // }
  // default: {
  //   Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
  //   break;
  // }
  // }
  if (n == 0){
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 0));
    Rf_unprotect(1);
    return out;
  }
  R_xlen_t count = Rf_asReal(Rf_protect(cpp_num_na(x)));
  // switch ( TYPEOF(x) ){
  // case LGLSXP:
  // case INTSXP: {
  //   int *p_x = INTEGER(x);
  //   for (R_xlen_t i = 0; i < n; ++i){
  //     count += (p_x[i] == NA_INTEGER);
  //   }
  //   break;
  // }
  // case REALSXP: {
  //   double *p_x = REAL(x);
  //   for (R_xlen_t i = 0; i < n; ++i){
  //     count += (p_x[i] != p_x[i]);
  //   }
  //   break;
  // }
  // case STRSXP: {
  //   SEXP *p_x = STRING_PTR(x);
  //   for (R_xlen_t i = 0; i < n; ++i){
  //     count += (p_x[i] == NA_STRING);
  //   }
  //   break;
  // }
  // case RAWSXP: {
  //   break;
  // }
  // case CPLXSXP: {
  //   Rcomplex *p_x = COMPLEX(x);
  //   for (R_xlen_t i = 0; i < n; ++i){
  //     count +=
  //       ( !((p_x[i]).r == (p_x[i]).r) ) ||
  //       ( !((p_x[i]).i == (p_x[i]).i) );
  //   }
  //   break;
  // }
  // default: {
  //   Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
  //   break;
  // }
  // }
  switch ( TYPEOF(x) ){
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    if (n <= std::numeric_limits<int>::max()){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, count));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == NA_INTEGER);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, count));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == NA_INTEGER);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  case REALSXP: {
    double *p_x = REAL(x);
    if (n <= std::numeric_limits<int>::max()){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, count));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] != p_x[i]);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, count));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] != p_x[i]);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  case STRSXP: {
    SEXP *p_x = STRING_PTR(x);
    if (n <= std::numeric_limits<int>::max()){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, count));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == NA_STRING);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, count));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += (p_x[i] == NA_STRING);
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  case RAWSXP: {
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, 0));
    Rf_unprotect(2);
    return out;
  }
  case CPLXSXP: {
    Rcomplex *p_x = COMPLEX(x);
    if (n <= std::numeric_limits<int>::max()){
      SEXP out = Rf_protect(Rf_allocVector(INTSXP, count));
      int *p_out = INTEGER(out);
      int whichi = 0;
      int i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += ( !((p_x[i]).r == (p_x[i]).r) ) ||
          ( !((p_x[i]).i == (p_x[i]).i) );
        ++i;
      }
      Rf_unprotect(2);
      return out;
    } else {
      SEXP out = Rf_protect(Rf_allocVector(REALSXP, count));
      double *p_out = REAL(out);
      R_xlen_t whichi = 0;
      R_xlen_t i = 0;
      while (whichi < count){
        p_out[whichi] = i + 1;
        whichi += ( !((p_x[i]).r == (p_x[i]).r) ) ||
          ( !((p_x[i]).i == (p_x[i]).i) );
        ++i;
      }
      Rf_unprotect(2);
      return out;
    }
  }
  default: {
    Rf_unprotect(1);
    Rf_error("%s cannot handle an object of type %s", __func__, Rf_type2char(TYPEOF(x)));
    break;
  }
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
//     for (R_xlen_t i = 0; i < n; ++i){
//       p_out[i] = (p_x[i] == NA_INTEGER);
//     }
//     break;
//   }
//   case REALSXP: {
//     double *p_x = REAL(x);
//     for (R_xlen_t i = 0; i < n; ++i){
//       // Because NaN == NaN is false
//       p_out[i] = !(p_x[i] == p_x[i]);
//     }
//     break;
//   }
//   case STRSXP: {
//     SEXP *p_x = STRING_PTR(x);
//     for (R_xlen_t i = 0; i < n; ++i){
//       p_out[i] = (p_x[i] == NA_STRING);
//     }
//     break;
//   }
//   case RAWSXP: {
//     break;
//   }
//   case CPLXSXP: {
//     Rcomplex *p_x = COMPLEX(x);
//     for (R_xlen_t i = 0; i < n; ++i){
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
