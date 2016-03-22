#include "Corpus.h"
// header from digest package
#include <pmurhashAPI.h>

using namespace Rcpp;
using namespace std;

// seeds for hashing trick
const uint32_t MURMURHASH3_HASH_SEED = 3120602769LL;
const uint32_t MURMURHASH3_SIGN_SEED = 79193439LL;

// feature hash
uint32_t murmurhash3_hash ( string &str) {
  return PMurHash32(MURMURHASH3_HASH_SEED, str.c_str(), str.size());
}

// feature sign hash
int murmurhash3_sign (const string &str) {
  return (int)PMurHash32(MURMURHASH3_SIGN_SEED, str.c_str(), str.size());
}

class HashCorpus: public Corpus {
public:
  // constructor
  HashCorpus(uint32_t size,
             uint32_t ngram_min, uint32_t ngram_max,
             uint32_t win_size,
             int use_signed_hash)
  {
    doc_count = 0;
    token_count = 0;
    buckets_size = size;
    signed_hash = use_signed_hash;
    this->ngram_min = ngram_min;
    this->ngram_max = ngram_max;
    this->window_size = win_size;
    this->ngram_delim = "_";
    // init dtm with ncol = hash_size
    dtm = SparseTripletMatrix<uint32_t>(0, size);
    tcm = SparseTripletMatrix<float>(size, size);
  };
  // total number of tokens in corpus
  // total number of tokens in corpus
  int get_token_count() {return this -> token_count;};
  int get_doc_count() { return this -> doc_count; };

  void clear_tcm() {this->tcm.clear();};
  size_t get_tcm_size() {return this->tcm.size();};

  // implements hashing trick
  void insert_terms (vector< string> &terms) {
    uint32_t term_index, context_term_index;

    size_t K = terms.size();
    size_t i = 0;
    float increment = 0.0;

    for(auto term: terms) {
      this->token_count++;
      term_index = murmurhash3_hash(term) % buckets_size;
      if(signed_hash && murmurhash3_sign(term) < 0) {
        dtm.add(doc_count, term_index, -1);
      }
      else {
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
          context_term_index = murmurhash3_hash(terms[i + j]) % buckets_size;
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
      i++;
    }
  }

  void insert_document(const CharacterVector doc) {
    vector< string> ngrams = get_ngrams(doc, this->ngram_min, this->ngram_max, this->ngram_delim, stopwords);
    insert_terms(ngrams);
    this->dtm.increment_nrows();
    this->doc_count++;
  }

  void insert_document_batch(const ListOf<const CharacterVector> docs_batch) {
    for (auto it:docs_batch)
      insert_document(it);
  }
  // get term cooccurence matrix
  SEXP get_tcm() {
    vector< string> dummy_dimnames(0);
    return tcm.get_sparse_triplet_matrix(dummy_dimnames, dummy_dimnames);
  }

  SEXP get_dtm_triplet() {
    vector< string> dummy_names(0);
    return dtm.get_sparse_triplet_matrix(dummy_names, dummy_names);
  };

  // R's interface to document-term matrix construction
  SEXP get_dtm() { return get_dtm_triplet();};

private:
  uint32_t buckets_size;
  int signed_hash;
};
