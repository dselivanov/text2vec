#include <Rcpp.h>
#include <string>
#include <vector>
#include <functional>
#include <unordered_map>
#include <RcppParallel.h>

// fast integer hashing
uint32_t fast_int_hash(uint32_t a);
