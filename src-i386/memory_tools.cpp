#include <Rcpp.h>
#include <stdlib.h>

#ifdef __GLIBC__
#include <malloc.h>
#endif

#ifdef USE_BRK
#include <unistd.h>
#include <stdint.h>
#endif
#include <Rinternals.h>

// workaround for issue when glibc on ubuntu doesn't release memory
// https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14611
// [[Rcpp::export]]
SEXP malloc_trim(SEXP keep) {
#ifdef __GLIBC__
  return Rf_ScalarLogical(malloc_trim(Rf_asInteger(keep)));
#else
  return R_NilValue;
#endif
}
