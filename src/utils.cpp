#include <Rcpp.h>
#include "text2vec.h"
using namespace Rcpp;
using namespace std;

NumericMatrix convert2Rmat(vector<vector<double> > &mat, size_t ncol) {
  NumericMatrix res(mat.size(), ncol);
  for (size_t i = 0; i < mat.size(); i++)
    for (size_t j = 0; j < ncol; j++)
      res(i, j) = mat[i][j];
  return res;
}

// inline double dot_prod(vector<double> &x, vector<double> &y) {
//   double res = 0;
//   for(size_t i = 0; i < x.size(); i++) {
//     res += x[i] * y[i];
//   }
//   return res;
// }

void fill_mat_val(vector<vector<double> > &mat, size_t ncol, double val) {
  for (size_t i = 0; i < mat.size(); i++)
    for (size_t j = 0; j < ncol; j++)
      mat[i][j] = val;
}
void fill_mat_rand(vector<vector<double> > &mat, size_t ncol, double runif_min, double runif_max) {
  for (size_t i = 0; i < mat.size(); i++)
    for (size_t j = 0; j < ncol; j++)
      mat[i][j] = R::runif(runif_min, runif_max); //(double)rand() / (double)RAND_MAX - 0.5;
}

void fill_vec_rand(vector<double>  &vec, double runif_min, double runif_max) {
  for (size_t i = 0; i < vec.size(); i++)
    vec[i] = R::runif(runif_min, runif_max); //(double)rand() / (double)RAND_MAX - 0.5;
}

void fill_vec_val(vector<double>  &vec, double val) {
  for (size_t i = 0; i < vec.size(); i++)
    vec[i] = val;
}

vector<string> get_ngrams(const CharacterVector terms,
                          uint32_t ngram_min,
                          uint32_t ngram_max,
                          const string &ngram_delim) {
  // iterates through input vector by window of size = n_max and build n-grams
  // for terms ["a", "b", "c", "d"] and n_min = 1, n_max = 2
  // will build 1:3-grams in following order
  //"a"     "a_b"   "a_b_c" "b"     "b_c"   "b_c_d" "c"     "c_d"   "d"
  uint32_t len = terms.size();

  // special case for unigram to speed up a little bit
  if(ngram_min == 1 && ngram_max == 1) {
    return(as<vector<string>>(terms));
  }

  // calculate res size
  size_t out_len = 0;

  uint32_t i1 = min(len, ngram_min);
  uint32_t i2 = min(len, ngram_max);

  for(size_t i = i1; i <= i2; i++)
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

// //' @export
// // [[Rcpp::export]]
// CharacterVector get_ngrams_R(const CharacterVector terms,
//                              uint32_t ngram_min,
//                              uint32_t ngram_max) {
//   return wrap(get_ngrams(terms, ngram_min, ngram_max, "_"));
// }
