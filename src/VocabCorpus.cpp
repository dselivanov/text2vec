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
#include "VocabCorpus.h"

//-----------------------------------------------------------------
// VocabCorpus methods implementation
//-----------------------------------------------------------------
VocabCorpus::VocabCorpus(const CharacterVector vocab_R, uint32_t n_min, uint32_t n_max,
                         const CharacterVector stopwords_R, const String delim) {
  tcm = SparseTripletMatrix<float>(vocab_R.size(), vocab_R.size());
  //this->window_size = window_size;
  init(vocab_R, n_min, n_max, stopwords_R, delim);
  word_count.resize(vocab_R.size());
}
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
// int context = 0 means symmetric context for co-occurence - matrix will be symmetric
// So we will keep only right upper-diagonal elements
// int context = 1 means right words context only
// int context = -1 means left words context only
void VocabCorpus::insert_terms (vector< string> &terms, int grow_dtm, int context, uint32_t window_size,
                                const NumericVector& weights, int flag_binary_cooccurence) {

  uint32_t term_index, context_term_index;
  size_t K = terms.size();
  size_t i = 0;
  float increment = 0.0;

  typename unordered_map < string, uint32_t > :: const_iterator term_iterator, context_term_iterator;
  unordered_set<uint32_t> context_terms_seen;
  for(auto term:terms) {
    this->token_count++;

    term_iterator = this->vocab.find(term);
    //###########################################
    if(term_iterator != this->vocab.end()) {
      // get main word index from vocab
      term_index = term_iterator->second;
      word_count[term_index]++;
      if(grow_dtm) {
        // increment count for input term
        dtm.add(doc_count, term_index, 1);
      }
      //###########################################
      // cooccurence related
      // will check 1 == ngram_min == ngram_max on R side
      // and set window_size = 0 if not
      // will not go into this loop if window_size == 0
      // for (uint32_t j = 1; j <= this->window_size; j++) {
      context_terms_seen.clear();
      // int context_term_seen;
      uint32_t j = 1;
      while(j <= window_size && i + j < K) {
        // context_term_seen = 0;
        // check doc bounds
        context_term_iterator = this->vocab.find((terms[i + j]) );
        // if context word in vocab
        if(context_term_iterator != this->vocab.end()) {
          // get context word index from vocab
          context_term_index = context_term_iterator->second;

          if(flag_binary_cooccurence && context_terms_seen.find(context_term_index) != context_terms_seen.end()) {
            //do nothing
          } else {
            context_terms_seen.insert(context_term_index);
            // calculate cooccurence increment for particular position j of context word
            increment = weights[j - 1];
            // increment = weighting_fun(j2);
            // int context = 0 means symmetric context for co-occurence - matrix will be symmetric
            // So we will keep only right upper-diagonal elements
            // int context = 1 means right words context only
            // int context = -1 means left words context only
            switch(context) {
            // handle symmetric context
            case 0:
              // map stores only elements above diagonal because our matrix is symmetrical
              if(term_index < context_term_index)
                this->tcm.add(term_index, context_term_index, increment);
              else {
                // also we are not interested in context words equal to main word
                // diagonal elememts will be zeros
                // if(term_index != context_term_index)
                // commented out because experiments showed that it is better to have diagonal elements
                this->tcm.add(context_term_index, term_index, increment);
              }
              break;
              // handle right context
            case 1:
              this->tcm.add(term_index, context_term_index, increment);
              break;
              // handle left context
            case -1:
              this->tcm.add(context_term_index, term_index, increment);
              break;
            default:
              ::Rf_error("call to insert_terms with context !in [0,1, -1]");
            }
          }
        }
        j++;
      }
    }
    i++;
  }
}
//-----------------------------------------------------------------
void VocabCorpus::insert_document(const CharacterVector doc, int grow_dtm, int context,
                                  uint32_t window_size, const NumericVector& weights,
                                  int flag_binary_cooccurence) {
  checkUserInterrupt();
  std::vector<std::string> std_string_vec = charvec2stdvec(doc);
  std::vector<std::string> ngram_vec
    = generate_ngrams(std_string_vec, this->ngram_min, this->ngram_max, this->stopwords, this->ngram_delim);

  this->insert_terms(ngram_vec, grow_dtm, context, window_size, weights, flag_binary_cooccurence);
  this->dtm.increment_nrows();
  this->doc_count++;
}
//-----------------------------------------------------------------
void VocabCorpus::insert_document_batch(const ListOf<const CharacterVector> docs_batch, int grow_dtm,
                                        int context, uint32_t window_size, const NumericVector& weights,
                                        int flag_binary_cooccurence) {
  for (auto it:docs_batch) {
    this->insert_document(it, grow_dtm, context, window_size, weights, flag_binary_cooccurence);
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
S4 VocabCorpus::get_tcm() {
  CharacterVector dimnames(vocab.size());
  for(auto it:vocab)
    dimnames[it.second] = Rf_mkCharLenCE( it.first.c_str(), it.first.size(), CE_UTF8);
  S4 res = tcm.get_sparse_triplet_matrix(dimnames, dimnames);
  res.attr("word_count") = wrap(word_count);
  return res;
}
//-----------------------------------------------------------------
S4 VocabCorpus::get_dtm() {
  CharacterVector dummy_doc_names(0);
  CharacterVector terms(this->vocab.size());
  for(auto it:vocab)
    terms[it.second] = Rf_mkCharLenCE( it.first.c_str(), it.first.size(), CE_UTF8);
  return dtm.get_sparse_triplet_matrix(dummy_doc_names, terms);
}

//-----------------------------------------------------------------
// VocabCorpus R wrappers
//-----------------------------------------------------------------

// [[Rcpp::export]]
SEXP cpp_vocabulary_corpus_create(const CharacterVector vocab_R, uint32_t n_min, uint32_t n_max,
                              const CharacterVector stopwords_R, const String delim) {
  VocabCorpus *vc = new VocabCorpus(vocab_R, n_min, n_max, stopwords_R, delim);
  XPtr<VocabCorpus> ptr(vc, true);
  return ptr;
}

// [[Rcpp::export]]
void cpp_vocabulary_corpus_insert_document_batch(SEXP ptr,
                                                  const ListOf<const CharacterVector> &docs_batch,
                                                  int grow_dtm,
                                                  int context,
                                                  uint32_t window_size,
                                                  const NumericVector& weights,
                                                  int flag_binary_cooccurence = 0) {
  Rcpp::XPtr<VocabCorpus> vc(ptr);
  vc->insert_document_batch(docs_batch, grow_dtm, context, window_size, weights, flag_binary_cooccurence);
}

// [[Rcpp::export]]
S4 cpp_vocabulary_corpus_get_tcm(SEXP ptr) {
  Rcpp::XPtr<VocabCorpus> vc(ptr);
  return vc->get_tcm();
}

// [[Rcpp::export]]
S4 cpp_vocabulary_corpus_get_dtm(SEXP ptr) {
  Rcpp::XPtr<VocabCorpus> vc(ptr);
  return vc->get_dtm();
}
