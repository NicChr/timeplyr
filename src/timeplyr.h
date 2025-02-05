#ifndef timeplyr_cpp_
#define timeplyr_cpp_

#include <cpp11.hpp>
#include <Rinternals.h>
// #include <cstdint> // for intmax_t

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#ifndef VECTOR_PTR_RO
#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))
#endif


// Largest 32-bit int
#ifndef r_integer_max_
#define r_integer_max_ std::numeric_limits<int>::max()
#endif

// Smallest 32-bit int
#ifndef r_integer_min_
#define r_integer_min_ std::numeric_limits<int>::min()
#endif

double r_sum(SEXP x, bool na_rm);
double r_min(SEXP x);
void cpp_copy_names(SEXP source, SEXP target);

#endif
