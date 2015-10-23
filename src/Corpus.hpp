#include "text2vec.h"

class Corpus {
public:

  void insert_document( const vector <string> &terms, int ngram_min, int ngram_max, const string ngram_delim);

  void insert_document_batch( const vector <vector <string> > &docs_batch, int ngram_min, int ngram_max, const string ngram_delim = "_");

  int get_doc_count();

  int get_token_count() {return token_count;};

protected:
  int token_count;
  vector<Document> docs;
  vector<string> global_terms;
  //document counter
  int doc_count;
  // number of tokens
  void insert_dtm_doc(const unordered_map<uint32_t, int> &term_count_map) {
    Document doc(term_count_map, doc_count);
    docs.push_back(doc);
    doc_count++;
    token_count += term_count_map.size();
  }

  SEXP get_dtm_dgT(int ncol) {
    int i = 0;
    NumericVector dtm_x(token_count);
    IntegerVector dtm_i(token_count);
    IntegerVector dtm_j(token_count);

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
    if(this->global_terms.empty())
      dtm.slot("Dimnames") = List::create(R_NilValue, R_NilValue);
    else
      dtm.slot("Dimnames") = List::create(R_NilValue, global_terms);
    return dtm;
  }


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
