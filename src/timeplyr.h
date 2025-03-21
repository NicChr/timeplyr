#ifndef timeplyr_cpp_
#define timeplyr_cpp_

#include <cpp11.hpp>
#include <Rinternals.h>

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

#endif
