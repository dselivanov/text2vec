#include "hash.h"
#include "Corpus.h"
using namespace Rcpp;
using namespace std;

class HashCorpus: public Corpus {
public:
  // constructor
  HashCorpus(uint32_t size, int use_signed_hash,
             uint32_t ngram_min, uint32_t ngram_max, string ngram_delim)
  {
    doc_count = 0;
    token_count = 0;
    buckets_size = size;
    signed_hash = use_signed_hash;
    this->ngram_min = ngram_min;
    this->ngram_max = ngram_max;
    this->ngram_delim = ngram_delim;
  };
  // total number of tokens in corpus
  int get_token_count() {return this->get_token_count();};
  // total number of documents in corpus
  int get_doc_count() { return doc_count; };

  // implements hashing trick
  void insert_terms (vector< string> &terms) {
    unordered_map<uint32_t, uint32_t> term_count_map;
    for(auto term: terms) {
      this->token_count++;
      uint32_t term_id = murmurhash3_hash(term) % buckets_size;
      if(signed_hash) {
        if(murmurhash3_sign(term) >= 0) {
          ++term_count_map[term_id];
        }
        else {
          --term_count_map[term_id];
        }
      }
      else {
        ++term_count_map[term_id];
      }
    }
    this->insert_dtm_doc(term_count_map);
  }
//   void insert_document(const CharacterVector doc) {this->insert_document(doc);};
//   void insert_document_batch(const ListOf<const CharacterVector> docs_batch) {this->insert_document_batch(docs);};

  void insert_document(const CharacterVector doc) {
    vector< string > ngrams = get_ngrams(doc);
    insert_terms(ngrams);
  }
  void insert_document_batch(const ListOf<const CharacterVector> docs_batch) {
    for (auto it:docs_batch)
      insert_document(it);
  }

  SEXP get_dtm_triplet() {
    size_t ncol = this->buckets_size;

    int i = 0;
    NumericVector dtm_x(this->nnz);
    IntegerVector dtm_i(this->nnz);
    IntegerVector dtm_j(this->nnz);

    for (auto doc: docs) {
      for (int j = 0; j < doc.doc_len; j++) {
        dtm_i[i] = doc.doc_id;
        dtm_j[i] = doc.term_ids[j];
        dtm_x[i] = doc.term_counts[j];
        i++;
      }
    }

    S4 dtm("dgTMatrix");
    dtm.slot("i") = dtm_i;
    dtm.slot("j") = dtm_j;
    dtm.slot("x") = dtm_x;
    dtm.slot("Dim") = IntegerVector::create(doc_count, ncol) ;
    dtm.slot("Dimnames") = List::create(R_NilValue, R_NilValue);

    return dtm;
  }

  // R's interface to document-term matrix construction
  SEXP get_dtm(int type) {
    switch (type) {
    case 0:
      return get_dtm_triplet ();
    case 1:
      return get_dtm_lda_c();
    case 2:
      return get_dtm_minhash();
    default:
      return R_NilValue;
    }
  }

private:
  uint32_t buckets_size;
  int signed_hash;
};
