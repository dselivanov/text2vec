#include "tmlite.h"

class Document {
public:
  Document(const unordered_map<uint32_t, int> &doc_map, int doc_num) {
    doc_len = doc_map.size();

    vector<uint32_t> term(doc_len);

    vector<int> cnt(doc_map.size());
    int i = 0;
    for(auto it:doc_map) {
      term[i] = it.first;
      cnt[i] = it.second;
      i++;
    }
    term_ids = term;
    term_counts = cnt;
    doc_id = doc_num;
    // Rprintf("doc %d inserted, len = %d, first: %d->%d \n", doc_num, doc_len, doc_map.begin()->first, doc_map.begin()->second);
  };
  vector<uint32_t> term_ids;
  vector<int> term_counts;
  size_t doc_len;
  int doc_id;
};

class Corpus {
public:

  void insert_document( CharacterVector words);

  void insert_document_batch( ListOf<CharacterVector> docs_batch);

  int get_doc_count();

protected:
  vector<Document> docs;
  vector<string> terms;
  //document counter
  int doc_count;
  // number of tokens
  int token_count;
  void insert_dtm_doc(const unordered_map<uint32_t, int> &indices) {
    Document doc(indices, doc_count);
    docs.push_back(doc);
    doc_count++;
    token_count += indices.size();
  }

  SEXP get_dtm_dgT(int ncol) {
    size_t i = 0;
    NumericVector dtm_x(token_count);
    IntegerVector dtm_i(token_count);
    IntegerVector dtm_j(token_count);

    for (auto doc: docs) {
      for (size_t j = 0; j < doc.doc_len; j++) {
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
    //if(col_names.empty())
    if(this->terms.empty())
      dtm.slot("Dimnames") = List::create(R_NilValue, R_NilValue);
    else
      dtm.slot("Dimnames") = List::create(R_NilValue, terms);
    return dtm;
    }


  SEXP get_dtm_ldaC() {
    List lda_c_dtm(doc_count);
    size_t j = 0;
    for(auto doc : this->docs) {
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
};

class DictCorpus: public Corpus {
public:
  // constructor
  DictCorpus() {
    token_count = 0;
    doc_count = 0;
  };

  // dictionary
  unordered_map<string, int> dict;
  // get number of inserted documents
  int get_doc_count() { return doc_count; };

  void insert_document( CharacterVector words) {
    typename unordered_map < string, int > :: const_iterator term_iterator;
    int col_index;
    // map represents pair of (word_id, word_count)
    // we add words to dictionary iteratively
    // each new word has incremented index
    unordered_map<uint32_t, int> indices;
    for (auto element : words) {
      term_iterator = dict.find(as<string>(element));
      // new element - add to dictionary
      // set index to the next int that is not used - dict.size()
      if(term_iterator == dict.end()) {
        col_index = dict.size();
        dict.insert(make_pair(as<string>(element), col_index));
        // keep word in order (we don't want to use bi-directional map)
        terms.push_back(as<string>(element));
      }
      // dictionary already contains document - get its index
      else {
        col_index = term_iterator -> second;
      }
      ++indices[col_index];
    }
    // add row - update sparse matrix values
    insert_dtm_doc(indices);
  }

  void insert_document_batch(ListOf<CharacterVector> docs_batch)  {
    for (auto it:docs_batch)
      insert_document(it);
  }

  SEXP get_dtm(IntegerVector type) {
    switch (type[0]) {
    case 0:
      return get_dtm_dgT (terms.size());
    case 1:
      return get_dtm_ldaC();
    default:
      return R_NilValue;
    }
  }
};

class HashCorpus: public Corpus {
public:
  // constructor
  HashCorpus(uint32_t size) {
    doc_count = 0;
    buckets_size = size;
  };

  int get_doc_count() { return doc_count; };
  // implements hashing trick
  void insert_document( CharacterVector   words) {
    typename unordered_map < string, int > :: const_iterator element_it;
    uint32_t col_index;
    unordered_map<uint32_t, int> indices;
    for (auto element : words) {
      col_index = hash_fn(as<string>(element)) % buckets_size;
      ++indices[col_index];
    }
    insert_dtm_doc(indices);
  }

  void insert_document_batch(ListOf < CharacterVector > docs)  {
    for (auto it:docs)
      insert_document(it);
  }

  SEXP get_dtm(int type) {
    switch (type) {
    case 0:
      return get_dtm_dgT (buckets_size);
    case 1:
      return get_dtm_ldaC();
    default:
      return R_NilValue;
    }
  }

private:
  // we use std::hash
  // investigate approaches to use murmurhash from digest package instead
  std::hash<string> hash_fn;
  uint32_t buckets_size;
};

RCPP_MODULE(DictCorpus) {
  class_< DictCorpus >( "DictCorpus" )
  .constructor()
  .field_readonly( "dict", &DictCorpus::dict )
  .method( "get_doc_count", &DictCorpus::get_doc_count )
  .method( "insert_document", &DictCorpus::insert_document )
  .method( "insert_document_batch", &DictCorpus::insert_document_batch )
  .method( "get_dtm", &DictCorpus::get_dtm )
  ;
}

RCPP_MODULE(HashCorpus) {
  class_< HashCorpus >( "HashCorpus" )
  .constructor<uint32_t>()
  .method( "get_doc_count", &HashCorpus::get_doc_count )
  .method( "insert_document", &HashCorpus::insert_document )
  .method( "insert_document_batch", &HashCorpus::insert_document_batch )
  .method( "get_dtm", &HashCorpus::get_dtm )
  ;
}
