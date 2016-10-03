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
#define TOKEN_VERBOSE 5000000
#define TOKEN_SCALE 1000000

using namespace Rcpp;
using namespace std;

//-----------------------------------------------------------------
// VocabCorpus class definitions
//-----------------------------------------------------------------
class VocabCorpus: public Corpus {
public:
  // contructor with window_size for term cooccurence matrix
  VocabCorpus(const CharacterVector vocab_R, uint32_t n_min, uint32_t n_max, uint32_t window_size,
              const CharacterVector stopwords_R, const String delim);
  void insert_terms (vector< string> &terms, int grow_dtm);
  void insert_document(const CharacterVector doc, int grow_dtm = 1);
  void insert_document_batch(const ListOf<const CharacterVector> docs_batch, int grow_dtm = 1);
  // total number of tokens in corpus
  int get_token_count() {return this -> token_count;};
  int get_doc_count() { return this -> doc_count; };

  void clear_tcm() {this->tcm.clear();};
  size_t get_tcm_size() {return this->tcm.size();};

  CharacterVector get_vocab();
  // get term cooccurence matrix
  SEXP get_tcm();
  SEXP get_dtm_triplet();
  SEXP get_dtm() {return get_dtm_triplet();};

private:
  int verbose;
  // vocabulary
  unordered_map<string, uint32_t> vocab;
  void init(const CharacterVector vocab_R, uint32_t n_min, uint32_t n_max, const CharacterVector stopwords_R, const String delim);
};

//-----------------------------------------------------------------
// VocabCorpus methods implementation
//-----------------------------------------------------------------
VocabCorpus::VocabCorpus(const CharacterVector vocab_R, uint32_t n_min, uint32_t n_max, uint32_t window_size,
                         const CharacterVector stopwords_R, const String delim) {
  tcm = SparseTripletMatrix<float>(vocab_R.size(), vocab_R.size());
  this->window_size = window_size;
  init(vocab_R, n_min, n_max, stopwords_R, delim);
};
//-----------------------------------------------------------------
void VocabCorpus::init(const CharacterVector vocab_R, uint32_t n_min, uint32_t n_max,
                       const CharacterVector stopwords_R, const String delim) {
  //vocab2 = Vocabulary(n_min, n_max, delim);
  this->verbose = 0;
  this->nnz = 0;
  this->token_count = 0;
  this->doc_count = 0;
  this->cooc_tokens_number = 0;
  this->ngram_min = n_min;
  this->ngram_max = n_max;
  // ngram concatenation delimiter
  this->ngram_delim = delim;

  size_t i = 0;
  // we know vocab size, so lets reserve buckets this number
  // and if we will lucky no rehash will needed
  this->vocab.reserve(vocab_R.size());
  // convert R vocab represenation to C++ represenation
  // also fill terms in right order
  for (auto val:vocab_R) {
    //grow vocabulary
    this->vocab.insert(make_pair(as< string >(val), i));
    // fill terms in order we add them in dctionary!
    i++;
  }
  // same for stopwords
  for (auto val:stopwords_R) {
    //grow vocabulary
    this->stopwords.insert(as< string >(val));
    // fill terms in order we add them in dctionary!
    i++;
  }
}

//-----------------------------------------------------------------
void VocabCorpus::insert_terms (vector< string> &terms, int grow_dtm) {

  uint32_t term_index, context_term_index;
  size_t K = terms.size();
  size_t i = 0;
  float increment = 0.0;

  typename unordered_map < string, uint32_t > :: const_iterator term_iterator, context_term_iterator;

  for(auto term:terms) {
    this->token_count++;

    if( this->verbose && token_count % TOKEN_VERBOSE == 0)
      Rprintf("%d M tokens processed, TCM non-zero elements: %.2fM, \n",
              token_count / TOKEN_SCALE ,
              (float)tcm.size() / TOKEN_SCALE );

    term_iterator = this->vocab.find(term);
    //###########################################
    if(term_iterator != this->vocab.end()) {
      // get main word index from vocab
      term_index = term_iterator->second;
      if(grow_dtm) {
        // increment count for input term
        dtm.add(doc_count, term_index, 1);
      }
      //###########################################
      // cooccurence related
      // will check 1 == ngram_min == ngram_max on R side
      // and set window_size = 0 if not
      // will not go into this loop if window_size == 0
      for (uint32_t j = 1; j <= this->window_size; j++) {
        // check doc bounds
        if( i + j < K) {
          context_term_iterator = this->vocab.find((terms[i + j]) );
          // if context word in vocab
          if(context_term_iterator != this->vocab.end()) {
            // get context word index from vocab
            context_term_index = context_term_iterator->second;
            // calculate cooccurence increment for particular position j of context word
            increment = weighting_fun(j);
            // map stores only elements above diagonal because our matrix is symmetrical
            if(term_index < context_term_index) {
              this->tcm.add(term_index, context_term_index, increment);
            }
            else {
              // also we are not interested in context words equal to main word
              // diagonal elememts will be zeros
              if(term_index != context_term_index)
                this->tcm.add(context_term_index, term_index, increment);
            }
          }
        }
      }
    }
    i++;
  }
}
//-----------------------------------------------------------------
void VocabCorpus::insert_document(const CharacterVector doc, int grow_dtm) {
  checkUserInterrupt();
  generate_ngrams(doc, this->ngram_min, this->ngram_max,
                  this->stopwords,
                  this->terms_filtered_buffer,
                  this->ngrams_buffer,
                  this->ngram_delim);
  insert_terms(this->ngrams_buffer, grow_dtm);
  this->dtm.increment_nrows();
  this->doc_count++;
}
//-----------------------------------------------------------------
void VocabCorpus::insert_document_batch(const ListOf<const CharacterVector> docs_batch, int grow_dtm) {
  for (auto it:docs_batch) {
    insert_document(it, grow_dtm);
  }
}

//-----------------------------------------------------------------
CharacterVector VocabCorpus::get_vocab() {
  CharacterVector vocab_R(vocab.size());
  for(auto i:vocab)
    vocab_R[ i.second ] = Rf_mkCharLenCE( i.first.c_str(), i.first.size(), CE_UTF8);
  return vocab_R;
}
//-----------------------------------------------------------------
SEXP VocabCorpus::get_tcm() {
  CharacterVector dimnames(vocab.size());
  for(auto it:vocab)
    dimnames[it.second] = Rf_mkCharLenCE( it.first.c_str(), it.first.size(), CE_UTF8);
  return tcm.get_sparse_triplet_matrix(dimnames, dimnames);
}
//-----------------------------------------------------------------
SEXP VocabCorpus::get_dtm_triplet() {
  CharacterVector dummy_doc_names(0);
  CharacterVector terms(this->vocab.size());
  for(auto it:vocab)
    terms[it.second] = Rf_mkCharLenCE( it.first.c_str(), it.first.size(), CE_UTF8);
  return dtm.get_sparse_triplet_matrix(dummy_doc_names, terms);
}
