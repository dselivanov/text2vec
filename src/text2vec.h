#include <Rcpp.h>
#include <string>
#include <vector>
#include <functional>
#include <unordered_map>
#include <RcppParallel.h>

using namespace std;

// n-gram generator
vector<string> get_ngrams(const Rcpp::CharacterVector terms,
           uint32_t ngram_min, uint32_t ngram_max,
           // delimiter for terms concatenation
           const string ngram_delim);
