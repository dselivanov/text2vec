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

#include "Corpus.h"
// header from digest package
#include <pmurhashAPI.h>

using namespace Rcpp;
using namespace std;

//-----------------------------------------------------------------
// HashCorpus class definitions
//-----------------------------------------------------------------
class HashCorpus: public Corpus {
public:
  // constructor
  HashCorpus(uint32_t size,
             uint32_t ngram_min, uint32_t ngram_max,
             //uint32_t win_size,
             int use_signed_hash);
  // total number of tokens in corpus
  int get_token_count() {return this -> token_count;};
  int get_doc_count() { return this -> doc_count; };

  void clear_tcm() {this->tcm.clear();};
  size_t get_tcm_size() {return this->tcm.size();};

  // implements hashing trick
  void insert_terms (vector< string> &terms, int grow_dtm, int context,
                     uint32_t window_size, const NumericVector &weights);

  void insert_document(const CharacterVector doc, int grow_dtm, int context,
                       uint32_t window_size, const NumericVector &weights);
  void insert_document_batch(const ListOf<const CharacterVector> docs_batch, int grow_dtm,
                             int context, uint32_t window_size, const NumericVector &weights);

  // get term cooccurence matrix
  SEXP get_tcm() {
    CharacterVector dummy_dimnames(0);
    return tcm.get_sparse_triplet_matrix(dummy_dimnames, dummy_dimnames);
  }

  SEXP get_dtm_triplet() {
    CharacterVector dummy_names(0);
    return dtm.get_sparse_triplet_matrix(dummy_names, dummy_names);
  };

  // R's interface to document-term matrix construction
  SEXP get_dtm() { return get_dtm_triplet();};

private:
  uint32_t buckets_size;
  int signed_hash;
};

