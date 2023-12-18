#include <cpp11.hpp>
#include <Rinternals.h>

// #include <numeric>

#define R_NO_REMAP

[[cpp11::register]]
double cpp_gcd2(double x, double y, double tol, bool na_rm){
  double zero = 0.0;
  if (!na_rm && ( !(x == x) || !(y == y) )){
    return NA_REAL;
  }
  // GCD(0,0)=0
  if (x == zero && y == zero){
    return zero;
  }
  // GCD(a,0)=a
  if (x == zero){
    return y;
  }
  // GCD(a,0)=a
  if (y == zero){
    return x;
  }
  double r;
  // Taken from number theory lecture notes
  while(std::fabs(y) > tol){
    r = std::fmod(x, y);
    x = y;
    y = r;
  }
  return x;
}

int cpp_gcd2_int(int x, int y, bool na_rm){
  int zero = 0;
  bool has_na = ( x == NA_INTEGER || y == NA_INTEGER );
  if (!na_rm && has_na){
    return NA_INTEGER;
  }
  if (na_rm && has_na){
    if (x == NA_INTEGER){
      return y;
    } else {
      return x;
    }
  }
  // GCD(0,0)=0
  if (x == zero && y == zero){
    return zero;
  }
  // GCD(a,0)=a
  if (x == zero){
    return y;
  }
  // GCD(a,0)=a
  if (y == zero){
    return x;
  }
  int r;
  // Taken from number theory lecture notes
  while(y != zero){
    r = x % y;
    x = y;
    y = r;
  }
  return x;
}

[[cpp11::register]]
double cpp_lcm2(double x, double y, double tol, bool na_rm){
  if (na_rm && ( !(x == x) || !(y == y) )){
    return ( !(x == x) ? y : x);
  }
  return ( std::fabs(x) / cpp_gcd2(x, y, tol, true) ) * std::fabs(y);
}

double cpp_lcm2_int(int x, int y, bool na_rm){
  int num_nas = (x == NA_INTEGER) + (y == NA_INTEGER);
  if ( num_nas >= 1 ){
    if (na_rm && num_nas == 1){
      return (x == NA_INTEGER ? y : x);
    } else {
      return NA_REAL;
    }
  }
  return ( std::fabs(x) / cpp_gcd2_int(x, y, false) ) * std::fabs(y);
}

// greatest common divisor with tolerance
// This is nice but uses memory

// double cpp_gcd3(cpp11::writable::doubles x, double tol){
//     double init = x[0];
//     return std::reduce(x.begin(), x.end(), init,
//                        [tol](double x1, double x2){
//                            if (x1 == 0 && x2 == 0){
//                                return 0.0;
//                            }
//                            if (x1 == 0){
//                                return x2;
//                            }
//                            if (x2 == 0){
//                                return x1;
//                            }
//                            double r = std::fmod(x1, x2);
//                            while(x2 > tol){
//                                r = std::fmod(x1, x2);
//                                x1 = x2;
//                                x2 = r;
//                            }
//                            return x1;
//                        });
// }


// Alternate method that uses round(x * 10^digits)
// to ensure it's a whole number
// and dividing the final gcd by 10^digits

// SEXP cpp_gcd(SEXP x, int digits, double tol, bool na_rm){
//   if (tol < 0 || tol >= 1){
//     Rf_error("tol must be >= 0 and < 1");
//   }
//   int n = Rf_length(x);
//   switch(TYPEOF(x)){
//   case LGLSXP:
//   case INTSXP: {
//     int *p_x = INTEGER(x);
//     SEXP out = Rf_protect(Rf_allocVector(INTSXP, std::min(n, 1)));
//     int *p_out = INTEGER(out);
//     int gcd = p_x[0];
//     int agcd;
//     for (int i = 1; i < n; ++i) {
//       gcd = cpp_gcd2_int(gcd, p_x[i], na_rm);
//       agcd = std::abs(gcd);
//       if ((agcd > 0 && agcd <= 1) || (!na_rm && (gcd == NA_INTEGER))){
//         break;
//       }
//     }
//     p_out[0] = gcd;
//     Rf_unprotect(1);
//     return out;
//   }
//   default: {
//     double *p_x = REAL(x);
//     SEXP out = Rf_protect(Rf_allocVector(REALSXP, std::min(n, 1)));
//     double *p_out = REAL(out);
//     double gcd = p_x[0];
//     // bool is_na = !(gcd == gcd);
//     double factor = std::pow(10, digits);
//     gcd = std::round(gcd * factor);
//     bool is_na;
//     double agcd;
//     for (int i = 1; i < n; ++i) {
//       is_na = !(gcd == gcd);
//       if (!na_rm && is_na){
//         break;
//       }
//       if (na_rm && is_na){
//         gcd = std::round(p_x[i] * factor);
//         continue;
//       }
//       gcd = cpp_gcd2(gcd, std::round(p_x[i] * factor), tol, na_rm);
//       // gcd = cpp_gcd_alt(gcd, std::round(p_x[i] * factor), na_rm);
//       agcd = std::fabs(gcd);
//       if (agcd > 0.0 && agcd <= factor){
//         break;
//       }
//     }
//     gcd = gcd / factor;
//     p_out[0] = gcd;
//     Rf_unprotect(1);
//     return out;
//   }
//   }
// }

[[cpp11::register]]
SEXP cpp_gcd(SEXP x, double tol, bool na_rm, bool break_early, bool round){
  if (tol < 0 || tol >= 1){
    Rf_error("tol must be >= 0 and < 1");
  }
  int n = Rf_length(x);
  switch(TYPEOF(x)){
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, std::min(n, 1)));
    int *p_out = INTEGER(out);
    int gcd = p_x[0];
    double agcd; // A double because you cant do abs(NA_integer_)
    for (int i = 1; i < n; ++i) {
      gcd = cpp_gcd2_int(gcd, p_x[i], na_rm);
      agcd = std::fabs(gcd);
      if ((agcd > 0.0 && agcd <= 1.0) || (!na_rm && (gcd == NA_INTEGER))){
        break;
      }
    }
    p_out[0] = gcd;
    Rf_unprotect(1);
    return out;
  }
  default: {
    double *p_x = REAL(x);
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, std::min(n, 1)));
    double *p_out = REAL(out);
    double gcd = p_x[0];
    double agcd;
    for (int i = 1; i < n; ++i) {
      gcd = cpp_gcd2(gcd, p_x[i], tol, na_rm);
      agcd = std::fabs(gcd);
      if ((!na_rm && !(gcd == gcd))){
        break;
      }
      if (break_early && agcd > 0.0 && agcd < (tol + tol)){
        gcd = tol * ( (gcd > 0) - (gcd < 0)); // tol * sign(gcd)
        break;
      }
    }
    if (round && tol > 0){
      double factor = std::pow(10, std::ceil(std::fabs(std::log10(tol))) + 1);
      gcd = std::round(gcd * factor) / factor;
    }
    p_out[0] = gcd;
    Rf_unprotect(1);
    return out;
  }
  }
}

// Lowest common multiple using GCD Euclidean algorithm

[[cpp11::register]]
SEXP cpp_lcm(SEXP x, double tol, bool na_rm){
  if (tol < 0 || tol >= 1){
    Rf_error("tol must be >= 0 and < 1");
  }
  int n = Rf_length(x);
  switch(TYPEOF(x)){
  case LGLSXP:
  case INTSXP: {
    int *p_x = INTEGER(x);
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, std::min(n, 1)));
    double *p_out = REAL(out);
    double lcm = p_x[0];
    if (p_x[0] == NA_INTEGER){
      lcm = NA_REAL;
    }
    int lcm_int = p_x[0];
    double int_max = double(std::numeric_limits<int>::max());
    for (int i = 1; i < n; ++i) {
      if (!na_rm && !(lcm == lcm)){
        lcm = NA_REAL;
        break;
      }
      lcm = cpp_lcm2_int(lcm_int, p_x[i], na_rm);
      if (std::fabs(lcm) > int_max){
        Rf_warning("Integer overflow, returning NA");
        lcm = NA_REAL;
        break;
      }
      lcm_int = (lcm == lcm) ? lcm : NA_INTEGER;
    }
    p_out[0] = lcm;
    Rf_protect(out = Rf_coerceVector(out, INTSXP));
    Rf_unprotect(2);
    return out;
  }
  default: {
    double *p_x = REAL(x);
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, std::min(n, 1)));
    double *p_out = REAL(out);
    double lcm = p_x[0];
    for (int i = 1; i < n; ++i) {
      if (!na_rm && !(lcm == lcm)){
        lcm = NA_REAL;
        break;
      }
      lcm = cpp_lcm2(lcm, p_x[i], tol, na_rm);
      if (lcm == R_PosInf || lcm == R_NegInf) break;
    }
    p_out[0] = lcm;
    Rf_unprotect(1);
    return out;
  }
  }
}

// Vectorised binary gcd

[[cpp11::register]]
SEXP cpp_gcd2_vectorised(SEXP x, SEXP y, double tol, bool na_rm){
  if (tol < 0 || tol >= 1){
    Rf_error("tol must be >= 0 and < 1");
  }
  R_xlen_t xn = Rf_xlength(x);
  R_xlen_t yn = Rf_xlength(y);
  R_xlen_t n = std::max(xn, yn);
  if (xn == 0 || yn == 0){
    n = 0;
  }
  switch(TYPEOF(x)){
  case INTSXP: {
    Rf_protect(x = Rf_coerceVector(x, INTSXP));
    Rf_protect(y = Rf_coerceVector(y, INTSXP));
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
    int *p_out = INTEGER(out);
    int *p_x = INTEGER(x);
    int *p_y = INTEGER(y);
    R_xlen_t xi;
    R_xlen_t yi;
    for (R_xlen_t i = 0; i < n; ++i) {
      xi = i % xn;
      yi = i % yn;
      p_out[i] = cpp_gcd2_int(p_x[xi], p_y[yi], na_rm);
    }
    Rf_unprotect(3);
    return out;
  }
  default: {
    Rf_protect(x = Rf_coerceVector(x, REALSXP));
    Rf_protect(y = Rf_coerceVector(y, REALSXP));
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, n));
    double *p_out = REAL(out);
    double *p_x = REAL(x);
    double *p_y = REAL(y);
    R_xlen_t xi;
    R_xlen_t yi;
    for (R_xlen_t i = 0; i < n; ++i) {
      xi = i % xn;
      yi = i % yn;
      p_out[i] = cpp_gcd2(p_x[xi], p_y[yi], tol, na_rm);
    }
    Rf_unprotect(3);
    return out;
  }
  }
}

[[cpp11::register]]
SEXP cpp_lcm2_vectorised(SEXP x, SEXP y, double tol, bool na_rm){
  if (tol < 0 || tol >= 1){
    Rf_error("tol must be >= 0 and < 1");
  }
  R_xlen_t xn = Rf_xlength(x);
  R_xlen_t yn = Rf_xlength(y);
  R_xlen_t n = std::max(xn, yn);
  if (xn == 0 || yn == 0){
    n = 0;
  }
  switch(TYPEOF(x)){
  case INTSXP: {
    double dbl_lcm;
    int int_lcm;
    double int_max = std::numeric_limits<int>::max();
    Rf_protect(x = Rf_coerceVector(x, INTSXP));
    Rf_protect(y = Rf_coerceVector(y, INTSXP));
    SEXP out = Rf_protect(Rf_allocVector(INTSXP, n));
    int *p_out = INTEGER(out);
    int *p_x = INTEGER(x);
    int *p_y = INTEGER(y);
    R_xlen_t xi;
    R_xlen_t yi;
    for (R_xlen_t i = 0; i < n; ++i) {
      xi = i % xn;
      yi = i % yn;
      dbl_lcm = cpp_lcm2_int(p_x[xi], p_y[yi], na_rm);
      if (!(dbl_lcm == dbl_lcm) || std::fabs(dbl_lcm) > int_max){
        p_out[i] = NA_INTEGER;
      } else {
        int_lcm = dbl_lcm;
        p_out[i] = int_lcm;
      }
    }
    Rf_unprotect(3);
    return out;
  }
  default: {
    Rf_protect(x = Rf_coerceVector(x, REALSXP));
    Rf_protect(y = Rf_coerceVector(y, REALSXP));
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, n));
    double *p_out = REAL(out);
    double *p_x = REAL(x);
    double *p_y = REAL(y);
    R_xlen_t xi;
    R_xlen_t yi;
    for (R_xlen_t i = 0; i < n; ++i) {
      xi = i % xn;
      yi = i % yn;
      p_out[i] = cpp_lcm2(p_x[xi], p_y[yi], tol, na_rm);
    }
    Rf_unprotect(3);
    return out;
  }
  }
}
