// Copyright (C) 2015 - 2016  Dmitriy Selivanov
// This file is part of text2vec
//
// text2vec is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// text2vec is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with text2vec.  If not, see <http://www.gnu.org/licenses/>.

#include "text2vec.h"
using namespace Rcpp;
using namespace std;

// Get current date/time, format is YYYY-MM-DD HH:mm:ss
const std::string currentDateTime() {
  time_t     now = time(0);
  struct tm  tstruct;
  char       buf[80];
  tstruct = *localtime(&now);
  // Visit http://en.cppreference.com/w/cpp/chrono/c/strftime
  // for more information about date/time format
  strftime(buf, sizeof(buf), "%Y-%m-%d %X", &tstruct);
  return buf;
}

NumericMatrix convert2Rmat(vector<vector<float> > &mat, size_t ncol) {
  NumericMatrix res(mat.size(), ncol);
  for (size_t i = 0; i < mat.size(); i++)
    for (size_t j = 0; j < ncol; j++)
      res(i, j) = mat[i][j];
  return res;
}

void fill_mat_val(vector<vector<float> > &mat, size_t ncol, float val) {
  for (size_t i = 0; i < mat.size(); i++)
    for (size_t j = 0; j < ncol; j++)
      mat[i][j] = val;
}
void fill_mat_rand(vector<vector<float> > &mat, size_t ncol, float runif_min, float runif_max) {
  for (size_t i = 0; i < mat.size(); i++)
    for (size_t j = 0; j < ncol; j++)
      mat[i][j] = R::runif(runif_min, runif_max); //(double)rand() / (double)RAND_MAX - 0.5;
}

void fill_vec_rand(vector<float>  &vec, float runif_min, float runif_max) {
  for (size_t i = 0; i < vec.size(); i++)
    vec[i] = R::runif(runif_min, runif_max); //(double)rand() / (double)RAND_MAX - 0.5;
}

void fill_vec_val(vector<float>  &vec, float val) {
  for (size_t i = 0; i < vec.size(); i++)
    vec[i] = val;
}

void generate_ngrams(CharacterVector terms_raw,
                               const uint32_t ngram_min,
                               const uint32_t ngram_max,
                               RCPP_UNORDERED_SET<string> &stopwords,
                               // pass buffer by reference to avoid memory allocation
                               // on each iteration
                               vector<string> &terms_filtered_buffer,
                               vector<string> &ngrams,
                               const string ngram_delim) {
  // clear buffers from previous iteration
  terms_filtered_buffer.clear();
  ngrams.clear();

  string term;
  // filter out stopwords
  for (auto it: terms_raw) {
    term = as<string>(it);
    if(stopwords.find(term) == stopwords.end())
      terms_filtered_buffer.push_back(term);
  }

  // special case for unigrams
  if( ngram_min == ngram_max &&  ngram_max == 1 ) {
    ngrams = terms_filtered_buffer;
    return;
  }

  uint32_t len = terms_filtered_buffer.size();

  string k_gram;
  size_t k, j_max_observed;
  // iterates through input vector by window of size = n_max and build n-grams
  // for terms ["a", "b", "c", "d"] and n_min = 1, n_max = 3
  // will build 1:3-grams in following order
  //"a"     "a_b"   "a_b_c" "b"     "b_c"   "b_c_d" "c"     "c_d"   "d"
  for(size_t j = 0; j < len; j ++ ) {
    k = 1;
    j_max_observed = j;
    while (k <= ngram_max && j_max_observed < len) {

      if( k == 1) {
        k_gram = terms_filtered_buffer[j_max_observed];
      } else
        k_gram = k_gram + ngram_delim + terms_filtered_buffer[j_max_observed];

      if(k >= ngram_min) {
        ngrams.push_back(k_gram);
      }
      j_max_observed = j + k;
      k = k + 1;
    }
  }
}
