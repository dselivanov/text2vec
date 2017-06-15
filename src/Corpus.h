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

#include "SparseTripletMatrix.h"
using namespace Rcpp;
using namespace std;

class Corpus {
public:

  void insert_terms (vector< string> &terms);

  void insert_document(const CharacterVector terms);

  void insert_document_batch(const ListOf<const CharacterVector> docs_batch);

  void clear_tcm();

  size_t get_tcm_size();

  // total number of documents in corpus
  uint32_t get_doc_count() { return doc_count; };

  // total number of tokens in corpus
  uint32_t get_token_count() {return token_count;};

protected:
  // token counter
  uint32_t token_count;
  size_t nnz;
  //document counter
  uint32_t doc_count;

  // ngram bounds
  uint32_t ngram_min;
  uint32_t ngram_max;
  // ngram concatenation delimiter
  string ngram_delim;

  // uint32_t window_size;
  // stopwords
  unordered_set<string> stopwords;
  // documents
  SparseTripletMatrix<int> dtm;
  std::vector<int> word_count;

  //#####Glove related
  uint64_t cooc_tokens_number;

  // term cooccurence matrix
  SparseTripletMatrix<float> tcm;

  // inline float weighting_fun(uint32_t offset) {
  //   return 1.0 / (float)offset;
  // }

  SEXP get_dtm_triplet();
};
