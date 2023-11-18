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
    return ( std::fabs(x) / cpp_gcd2(x, y, tol, na_rm) ) * std::fabs(y);
}

double cpp_lcm2_int(int x, int y, bool na_rm){
    return ( std::fabs(x) / cpp_gcd2_int(x, y, na_rm) ) * std::fabs(y);
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

[[cpp11::register]]
SEXP cpp_gcd(SEXP x, double tol, bool na_rm, int start, bool break_early, bool round){
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
        int gcd = p_x[start - 1];
        int agcd;
        for (int i = start; i < n; ++i) {
            gcd = cpp_gcd2_int(gcd, p_x[i], na_rm);
            agcd = std::abs(gcd);
            if ( // break early for small gcd
                    (agcd > 0 && agcd <= 1) ||
                        // Break early if NA and na_rm = FALSE
                        (!na_rm && (gcd == NA_INTEGER))
                        // (!na_rm && (p_x[i] == NA_INTEGER))
            ){
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
        double gcd = p_x[start - 1];
        double agcd;
        for (int i = start; i < n; ++i) {
            gcd = cpp_gcd2(gcd, p_x[i], tol, na_rm);
            agcd = std::fabs(gcd);
            if ((!na_rm && !(gcd == gcd))){
                break;
            }
            if (break_early && agcd > 0.0 && agcd < (tol + tol)){
                // tol * sign(gcd)
                gcd = tol * ( (gcd > 0) - (gcd < 0));
                break;
            }
            // if (  // break early for small gcd
            //         (break_early && agcd > 0.0 && agcd < (tol + tol)) ||
            //             // Break early if NA and na_rm = FALSE
            //             (!na_rm && !(gcd == gcd))
            //             // (!na_rm && !(p_x[i] == p_x[i]))
            // ){
            //     break;
            // }
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
SEXP cpp_lcm(SEXP x, double tol, bool na_rm, bool round){
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
        double lcm = double(p_x[0]);
        if (na_rm && n > 1 && !(lcm == lcm)){
            lcm = 1.0;
        }
        double int_max = double(std::numeric_limits<int>::max());
        // int lcm = p_x[0];
        // if (na_rm && n > 1 && lcm == NA_INTEGER){
        //     lcm = 1;
        // }
        for (int i = 1; i < n; ++i) {
            if (na_rm && p_x[i] == NA_INTEGER) continue;
            lcm = cpp_lcm2_int(lcm, p_x[i], true);
            if (std::fabs(lcm) > int_max){
                Rf_warning("Integer overflow, returning NA");
                lcm = NA_REAL;
                break;
            }
            // lcm = cpp_lcm2_int(lcm, p_x[i], 0, true);
            // if (p_x[i] != NA_INTEGER && lcm == NA_INTEGER){
            //     break;
            // }
            // lcm = cpp_lcm2_int(lcm, p_x[i], true);
        }
        // p_out[0] = lcm;
        p_out[0] = int(lcm);
        Rf_unprotect(1);
        return out;
    }
    default: {
        double *p_x = REAL(x);
        SEXP out = Rf_protect(Rf_allocVector(REALSXP, std::min(n, 1)));
        double *p_out = REAL(out);
        double lcm = p_x[0];
        if (na_rm && n > 1 && !(lcm == lcm)){
            lcm = 1.0;
        }
        for (int i = 1; i < n; ++i) {
            if (lcm == R_PosInf || lcm == R_NegInf) break;
            if (na_rm && !(p_x[i] == p_x[i])) continue;
            lcm = cpp_lcm2(lcm, p_x[i], tol, true);
        }
        if (round && tol > 0){
            double factor = std::pow(10, std::ceil(std::fabs(std::log10(tol))) + 1);
            lcm = std::round(lcm * factor) / factor;
        }
        p_out[0] = lcm;
        Rf_unprotect(1);
        return out;
    }
    }
}
