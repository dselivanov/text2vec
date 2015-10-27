#include "text2vec.h"
using namespace Rcpp;
using namespace std;
//
// //implements n-gram generation and processing
vector<string> get_ngrams(const CharacterVector terms, uint32_t ngram_min, uint32_t ngram_max, const string ngram_delim = "_") {
  // iterates through input vector by window of size = n_max and build n-grams
  // for terms ["a", "b", "c", "d"] and n_min = 1, n_max = 2
  // will build 1:3-grams in following order
  //"a"     "a_b"   "a_b_c" "b"     "b_c"   "b_c_d" "c"     "c_d"   "d"

  size_t len = terms.size();

  // calculate res size
  size_t out_len = 0;
  if(len >= ngram_min)
    for(size_t i = ngram_min; i <= ngram_max; i++)
      out_len += (len - i) + 1;
  vector< string> res(out_len);

  string k_gram;
  size_t k, i = 0, last_observed;
  for(size_t j = 0; j < len; j ++ ) {
    k = 0;
    last_observed = j + k;
    while (k < ngram_max && last_observed < len) {
      if( k == 0) {
        k_gram = terms[last_observed];
      }
      else
        k_gram = k_gram + ngram_delim + terms[last_observed];
      if(k >= ngram_min - 1) {
        res[i] = k_gram;
        i++;
      }
      k = k + 1;
      last_observed = j + k;
    }
  }
  return res;
}
