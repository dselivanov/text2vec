#include "text2vec.h"
using namespace Rcpp;
using namespace std;

//implements n-gram generation and processing
void ngram_generator(const CharacterVector terms,
           std::function<void(const string)> term_handler,
           uint32_t n_min = 1, uint32_t n_max = 1,
           const string delim = "_") {
  // iterates through input vector by window of size = n_max and build n-grams
  // for terms ["a", "b", "c", "d"] and n_min = 1, n_max = 2
  // will build 1:3-grams in following order
  //"a"     "a_b"   "a_b_c" "b"     "b_c"   "b_c_d" "c"     "c_d"   "d"
  string k_gram;
  size_t k;
  size_t last_observed;
  size_t len = terms.size();
  for(size_t j = 0; j < len; j ++ ) {
    k = 0;
    last_observed = j + k;
    while (k < n_max && last_observed < len) {
      if( k == 0)
        k_gram = as<string>(terms[last_observed]);
      else
        k_gram = k_gram + delim + as<string>(terms[last_observed]);
      if(k >= n_min - 1) {
        // here we catch next ngram and should process it
        term_handler(k_gram);
      }
      k = k + 1;
      last_observed = j + k;
    }
  }
}
