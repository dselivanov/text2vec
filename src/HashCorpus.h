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
  HashCorpus(uint32_t size, int use_signed_hash,
             uint32_t ngram_min, uint32_t ngram_max)
  {
    doc_count = 0;
    token_count = 0;
    buckets_size = size;
    signed_hash = use_signed_hash;
    this->ngram_min = ngram_min;
    this->ngram_max = ngram_max;
    this->ngram_delim = "_";
  };
  // total number of tokens in corpus
  int get_token_count() {return this->get_token_count();};
  // total number of documents in corpus
  int get_doc_count() { return doc_count; };

  // implements hashing trick
  void insert_terms (vector< string> &terms) {
    for(auto term: terms) {
      this->token_count++;
      uint32_t term_id = murmurhash3_hash(term) % buckets_size;
      if(signed_hash) {
        if(murmurhash3_sign(term) >= 0) {
          dtm.add(doc_count, term_id, 1);
        }
        else {
          dtm.add(doc_count, term_id, -1);
        }
      }
      else {
        dtm.add(doc_count, term_id, 1);
      }
    }
  }

  void insert_document(const CharacterVector doc) {
    vector< string > ngrams = get_ngrams(doc);
    insert_terms(ngrams);
    this->doc_count++;
  }
  void insert_document_batch(const ListOf<const CharacterVector> docs_batch) {
    for (auto it:docs_batch)
      insert_document(it);
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
