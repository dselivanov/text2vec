#include "Document.hpp"
using namespace Rcpp;
using namespace std;

class Corpus {
public:

  void insert_document( const vector <string> &terms, int ngram_min, int ngram_max, const string ngram_delim);

  void insert_document_batch( const vector <vector <string> > &docs_batch, int ngram_min, int ngram_max, const string ngram_delim = "_");

  int get_doc_count();

  int get_token_count() {return token_count;};

protected:
  int token_count;
  vector<Document> docs;
  //document counter
  int doc_count;
  // number of tokens
  void insert_dtm_doc(const unordered_map<uint32_t, uint32_t> &term_count_map) {
    Document doc(term_count_map, doc_count);
    docs.push_back(doc);
    doc_count++;
    token_count += term_count_map.size();
  }

  SEXP get_dtm_dgT();

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
