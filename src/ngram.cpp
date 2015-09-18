#include "tmlite.h"
using namespace Rcpp;
using namespace std;
//implements n-gram counting
void ngram_count(const vector<string> &terms,
           unordered_map<uint32_t, int> &term_count_map,
           unordered_map<string, int> &dict,
           vector<string> &terms_vec,
           int n_min = 1, int n_max = 2,
           const string delim = "_") {
  // iterates through input vector by window of size = n_max and build n-grams
  // for terms ["a", "b", "c", "d"] and n_min = 1, n_max = 2
  // will build 1:3-grams in following order
  //"a"     "a_b"   "a_b_c" "b"     "b_c"   "b_c_d" "c"     "c_d"   "d"
  string k_gram;
  int k;
  int last_observed;
  int len = terms.size();
  for(int j = 0; j < len; j ++ ) {
    k = 0;
    last_observed = j + k;
    while (k < n_max && last_observed < len) {
      if( k == 0)
        k_gram = terms[last_observed];
      else
        k_gram = k_gram + delim + terms[last_observed];
      if(k >= n_min - 1) {
        // here we catch next ngram and should process it
        process_term_dict(k_gram, term_count_map, dict, terms_vec);
      }
      k = k + 1;
      last_observed = j + k;
    }
  }
}


// this temporarly expose ngram functionality
//'@export
// [[Rcpp::export]]
CharacterVector ngram(CharacterVector terms,
                     int n_min = 1, int n_max = 1, std::string delim = "_") {
  // iterates through input vector by window of size = n_max and build n-grams
  // for terms ["a", "b", "c", "d"] and n_min = 1, n_max = 2
  // will build 1:3-grams in following order
  //"a"     "a_b"   "a_b_c" "b"     "b_c"   "b_c_d" "c"     "c_d"   "d"

  int len = terms.size();

  // calculate res size
  int out_len = 0;
  if(len >= n_min)
    for(int i = n_min; i <= n_max; i++) {
      out_len += (len - i) + 1;
    }

  CharacterVector res(out_len);
  string k_gram;
  int k;
  int i = 0;
  int last_observed;

  for(int j = 0; j < len; j ++ ) {
    k = 0;
    last_observed = j + k;
    while (k < n_max && last_observed < len) {
      if( k == 0)
        k_gram = as<string>(terms[last_observed]);
      else
        k_gram = k_gram + delim + as<string>(terms[last_observed]);
      if(k >= n_min - 1) {
        res[i] = k_gram;
        i++;
      }
      k = k + 1;
      last_observed = j + k;
    }
  }
  return res;
}
