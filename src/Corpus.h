#include "Document.h"
using namespace Rcpp;
using namespace std;

class Corpus {
public:

  void insert_terms (vector< string> &terms);

  void insert_document(const CharacterVector terms);

  void insert_document_batch(const ListOf<const CharacterVector> docs_batch);

//   virtual void insert_document(const CharacterVector doc) {
//     vector< string > ngrams = get_ngrams(doc);
//     insert_terms(ngrams);
//   }
//   virtual void insert_document_batch(const ListOf<const CharacterVector> docs_batch) {
//     for (auto it:docs_batch)
//       insert_document(it);
//   }


  // total number of documents in corpus
  uint32_t get_doc_count() { return doc_count; };

  // total number of tokens in corpus
  uint32_t get_token_count() {return token_count;};

  vector<string> get_ngrams(const CharacterVector terms) {
    // iterates through input vector by window of size = n_max and build n-grams
    // for terms ["a", "b", "c", "d"] and n_min = 1, n_max = 2
    // will build 1:3-grams in following order
    //"a"     "a_b"   "a_b_c" "b"     "b_c"   "b_c_d" "c"     "c_d"   "d"

    size_t len = terms.size();

    // calculate res size
    size_t out_len = 0;
    if(len >= this->ngram_min)
      for(size_t i = this->ngram_min; i <= this->ngram_max; i++)
        out_len += (len - i) + 1;
    vector< string> res(out_len);

    string k_gram;
    size_t k, i = 0, last_observed;
    for(size_t j = 0; j < len; j ++ ) {
      k = 0;
      last_observed = j + k;
      while (k < this->ngram_max && last_observed < len) {
        if( k == 0) {
          k_gram = terms[last_observed];
        }
        else
          k_gram = k_gram + this->ngram_delim + terms[last_observed];
        if(k >= this->ngram_min - 1) {
          res[i] = k_gram;
          i++;
        }
        k = k + 1;
        last_observed = j + k;
      }
    }
    return res;
  }

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

  // documents
  vector<Document> docs;

  void insert_dtm_doc(const unordered_map<uint32_t, uint32_t> &term_count_map) {
    Document doc(term_count_map, doc_count);
    docs.push_back(doc);
    doc_count++;
    nnz += term_count_map.size();
  }

  SEXP get_dtm_triplet();

  SEXP get_dtm_lda_c() {
    List lda_c_dtm(doc_count);
    int j = 0;
    for(auto doc : this->docs) {
      //first row - term_id
      //second row - term_count
      IntegerMatrix lda_c_doc(2, doc.doc_len);
      for (int i = 0; i < doc.doc_len; i++) {
        lda_c_doc(0, i) = doc.term_ids[i];
        lda_c_doc(1, i) = doc.term_counts[i];
      }
      lda_c_dtm[j] = lda_c_doc;
      j++;
    }
    return lda_c_dtm;
  }

  SEXP get_dtm_minhash() {
    List minhash_dtm(doc_count);
    int j = 0;
    for(auto doc : this->docs) {
      //first row - term_id
      IntegerMatrix minhash_doc(1, doc.doc_len);
      for (int i = 0; i < doc.doc_len; i++) {
        minhash_doc(0, i) = doc.term_ids[i];
      }
      minhash_dtm[j] = minhash_doc;
      j++;
    }
    return minhash_dtm;
  }

};
