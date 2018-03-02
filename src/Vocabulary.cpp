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

#include "Vocabulary.h"

// [[Rcpp::export]]
SEXP cpp_vocab_create(uint32_t ngram_min,
                  uint32_t ngram_max,
                  const CharacterVector stopwords_R,
                  const String delim,
                  int window_size) {
  Vocabulary *v = new Vocabulary(ngram_min, ngram_max, stopwords_R, delim, window_size);
  XPtr< Vocabulary> ptr(v, true);
  return ptr;
}

// [[Rcpp::export]]
void cpp_vocabulary_insert_document_batch(SEXP ptr, const ListOf<const CharacterVector> document_batch) {
  Rcpp::XPtr<Vocabulary> v(ptr);
  v->insert_document_batch(document_batch);
}

// [[Rcpp::export]]
void cpp_vocabulary_insert_document_batch_xptr(SEXP ptr, SEXP document_batch_ptr) {
  Rcpp::XPtr<Vocabulary> v(ptr);
  XPtr< std::vector<std::vector < std::string> > > tokens(document_batch_ptr);
  v->insert_document_batch_ptr(tokens);
}

// [[Rcpp::export]]
DataFrame cpp_get_vocab_statistics(SEXP ptr) {
  Rcpp::XPtr<Vocabulary> v(ptr);
  return v->get_vocab_statistics();
}

// [[Rcpp::export]]
int cpp_get_document_count(SEXP ptr) {
  Rcpp::XPtr<Vocabulary> v(ptr);
  return v->get_document_count();
}
