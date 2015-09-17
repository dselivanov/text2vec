#include "tmlite.h"
//implements n-gram counting
template <typename T>
void ngram(const vector<string> &terms,
           unordered_map<T, int> &dict,
           unordered_map<uint32_t, int> &indices,
           vector<T> &terms_set,
           int n_min = 1, int n_max = 2, const string delim = "_") {
  // iterates through input vector by window of size = n_max and build n-grams
  // for terms ["a", "b", "c", "d"] and n_min = 1, n_max = 2
  // will build n-grams in following order
  //"a"     "a_b"   "a_b_c" "b"     "b_c"   "b_c_d" "c"     "c_d"   "d"
  int debug = 0;
  vector<string> res;
  int dict_ind;
  int len = terms.size();
  //typename unordered_map < T, int > :: const_iterator term_iterator;
  typename unordered_map < T, int > :: iterator term_iterator;
  for(int j = 0; j < len; j ++ ) {
    if (debug) printf("j=%d\n", j);
    T k_gram;
    int k = 0;
    int last_observed = j + k;
    while (k < n_max && last_observed < len) {
      if (debug) printf ("k=%d\n", k);
      if( k == 0)
        k_gram = terms[last_observed];
      else
        k_gram = k_gram + delim + terms[last_observed];
      if(k >= n_min - 1) {
        if (debug) printf("%s\n", k_gram.c_str());

        term_iterator = dict.find(k_gram);
        // new ngram
        if(term_iterator == dict.end()) {
          dict.insert(make_pair(k_gram,  1));
          //dict_ind = dict.size();
          //dict.insert(make_pair(k_gram,  dict_ind));
          //terms_set.push_back(k_gram);
        }
        else {
          term_iterator->second++;
          //dict_ind = term_iterator -> second;
        }
        //++indices[dict_ind];
      }
      k = k + 1;
      last_observed = j + k;
    }
  }
}

// [[Rcpp::export]]
unordered_map<string, int> ngram_counter(vector<string> x, int n_min = 1, int n_max = 2, const string delim = "_") {
  unordered_map<string, int> dict;
  unordered_map<uint32_t, int> indices;
  vector<string> terms_set;
  ngram<string>(x, dict, indices, terms_set, n_min, n_max, delim);
  return dict;
}

/*** R
ngram_counter (c(letters[1:6], "a", "b"), 1, 3)
*/
