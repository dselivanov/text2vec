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
#include "Vocabulary.h"

using namespace Rcpp;
using namespace std;

//-----------------------------------------------------------------
// VocabCorpus class definitions
//-----------------------------------------------------------------
class VocabCorpus: public Corpus {
public:
  // contructor with window_size for term cooccurence matrix
  VocabCorpus(const CharacterVector vocab_R, uint32_t n_min, uint32_t n_max,
              const CharacterVector stopwords_R, const String delim);
  void insert_terms (vector< string> &terms, int grow_dtm, int context, uint32_t window_size, const NumericVector& weights);
  void insert_document(const CharacterVector doc, int grow_dtm, int context, uint32_t window_size, const NumericVector& weights);
  void insert_document_batch(const ListOf<const CharacterVector> docs_batch, int grow_dtm, int context, uint32_t window_size, const NumericVector& weights);
  // total number of tokens in corpus
  int get_token_count() {return this -> token_count;};
  int get_doc_count() { return this -> doc_count; };

  void clear_tcm() {this->tcm.clear();};
  size_t get_tcm_size() {return this->tcm.size();};

  CharacterVector get_vocab();
  // get term cooccurence matrix
  S4 get_tcm();
  // S4 get_dtm_triplet();
  S4 get_dtm();// {return get_dtm_triplet();};

private:
  int verbose;
  // vocabulary
  unordered_map<string, uint32_t> vocab;
  void init(const CharacterVector vocab_R, uint32_t n_min, uint32_t n_max, const CharacterVector stopwords_R, const String delim);
};
