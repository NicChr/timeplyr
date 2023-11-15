#include <cpp11.hpp>
#include <Rinternals.h>

// #include <numeric>

#define R_NO_REMAP

double cpp_gcd2(double x, double y, double tol = 0){
    if (x == 0 && y == 0){
        return 0.0;
    }
    if (x == 0){
        return y;
    }
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

// double cpp_gcd2(double x, double y, double tol = 0){
//     if (y == 0){
//         return 0.0;
//     }
//     if (x == R_PosInf){
//         return y;
//     }
//     if (y == R_PosInf){
//         return x;
//     }
//     double r;
//     // double r = std::fmod(x, y);
//     // while(r > tol){
//     //     y = r;
//     //     r = std::fmod(x, y);
//     //     x = y;
//     // }
//     while(y > tol){
//         r = std::fmod(x, y);
//         x = y;
//         y = r;
//     }
//     return x;
// }

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
//     // double init = x[0];
//     // return std::reduce(x.begin(), x.end(), init,
//     //                          [](double x1, double x2){
//     //                              if (x2 == 0){
//     //                                  return 0.0;
//     //                              }
//     //                              int i = 0;
//     //                              double r = std::fmod(x1, x2);
//     //                              while(r > 0){
//     //                                  x2 = r;
//     //                                  r = std::fmod(x1, x2);
//     //                                  x1 = x2;
//     //                                  if (i >= 100){
//     //                                      break;
//     //                                  }
//     //                                  ++i;
//     //                              }
//     //                              return x1;
//     //                          });
// }

[[cpp11::register]]
double cpp_gcd(SEXP x, double tol, int start){
    int n = Rf_length(x);
    double *p_x = REAL(x);
    double out = p_x[start - 1];
    for (int i = start; i < n; ++i) {
        out = cpp_gcd2(out, p_x[i], tol);
        if (out <= tol){
            break;
        }
    }
    return out;
}
