#include <cpp11.hpp>
#include <Rinternals.h>

#ifndef timeplyr_cpp_funs
#define timeplyr_cpp_funs

#define R_NO_REMAP
#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))

#define integer_max_ std::numeric_limits<int>::max()

R_xlen_t cpp_vector_size(SEXP x);
int cpp_vector_width(SEXP x);
double r_sum(SEXP x, bool na_rm);
double r_min(SEXP x);

#endif
