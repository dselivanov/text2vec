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

// fast integer hashing
uint32_t fast_int_hash(uint32_t a) {
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a);
  return a;
}

// checks if external pointer invalid
// [[Rcpp::export]]
int is_invalid_ptr(SEXP sexp_ptr) {
  Rcpp::XPtr<SEXP> ptr(sexp_ptr);
  return (ptr.get() == NULL);
}

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

std::vector<std::string> charvec2stdvec(CharacterVector terms_raw) {
  std::vector<std::string> result;
  result.reserve(terms_raw.size());
  for (auto it: terms_raw) {
    result.push_back(as<string>(it));
  }
  return(result);
}

vector<string> generate_ngrams(const std::vector< std::string> &terms,
                               const uint32_t ngram_min,
                               const uint32_t ngram_max,
                               unordered_set<string> &stopwords,
                               const string ngram_delim) {
  uint32_t len = terms.size();
  vector<string> ngrams;
  vector<string> terms_filtered;
  terms_filtered.reserve(len);
  ngrams.reserve(len);

  for (auto it: terms) {
    if(stopwords.find(it) == stopwords.end())
      terms_filtered.push_back(it);
  }

  // special case for unigrams
  if( ngram_min == ngram_max &&  ngram_max == 1 ) {
    return(terms_filtered);
  }

  string k_gram;
  size_t k, j_max_observed;
  // iterates through input vector by window of size = n_max and build n-grams
  // for terms ["a", "b", "c", "d"] and n_min = 1, n_max = 3
  // will build 1:3-grams in following order
  //"a"     "a_b"   "a_b_c" "b"     "b_c"   "b_c_d" "c"     "c_d"   "d"
  for(size_t j = 0; j < len; j ++ ) {
    k = 1;
    j_max_observed = j;
    while (k <= ngram_max && j_max_observed < terms_filtered.size()) {

      if( k == 1) {
        k_gram = terms_filtered[j_max_observed];
      } else
        k_gram = k_gram + ngram_delim + terms_filtered[j_max_observed];

      if(k >= ngram_min) {
        ngrams.push_back(k_gram);
      }
      j_max_observed = j + k;
      k = k + 1;
    }
  }
  return(ngrams);
}

std::vector<std::string> fixed_char_tokenizer(const std::string &s, char delim) {
  std::stringstream ss(s);
  std::string item;
  std::vector<std::string> elems;
  while (std::getline(ss, item, delim)) {
    elems.push_back(std::move(item));
  }
  return elems;
}

// [[Rcpp::export]]
SEXP cpp_fixed_char_tokenizer(CharacterVector x, char delim) {
  std::vector<std::string> docs = charvec2stdvec(x);
  std::vector<std::vector < std::string> > *tokens = new std::vector<std::vector < std::string> >;
  tokens->reserve(x.size());
  for(auto doc : docs) {
    tokens->push_back(fixed_char_tokenizer(doc, delim));
  }
  XPtr< std::vector<std::vector < std::string> > > ptr(tokens, true);
  ptr.attr("class") = CharacterVector::create("tokens_xprt");
  return ptr;
}
