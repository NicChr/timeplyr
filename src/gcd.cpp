#include <cpp11.hpp>
#include <Rinternals.h>

// #include <numeric>

#define R_NO_REMAP

double cpp_gcd2(double x, double y, double tol = 0, bool na_rm = true){
    if (!na_rm && ( !(x == x) || !(y == y) )){
        return NA_REAL;
    }
    // GCD(0,0)=0
    if (x == 0 && y == 0){
        return 0.0;
    }
    // GCD(a,0)=a
    if (x == 0){
        return y;
    }
    // GCD(a,0)=a
    if (y == 0){
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
SEXP cpp_gcd(SEXP x, double tol, bool na_rm, int start, bool break_early){
    int n = Rf_length(x);
    double *p_x = REAL(x);
    SEXP out = Rf_protect(Rf_allocVector(REALSXP, std::min(n, 1)));
    double *p_out = REAL(out);
    double gcd = p_x[start - 1];
    for (int i = start; i < n; ++i) {
        gcd = cpp_gcd2(gcd, p_x[i], tol, na_rm);
        // If we break early and x contains consecutive zeros,
        // The result isn't correct
        if (break_early && gcd <= tol){
            break;
        }
    }
    if (tol > 0){
        double factor = std::pow(10, std::ceil(std::fabs(std::log10(tol))) + 1);
        gcd = std::round(gcd * factor) / factor;
    }
    p_out[0] = gcd;
    Rf_unprotect(1);
    return out;
}
