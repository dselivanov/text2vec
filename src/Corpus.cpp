#include "tmlite.h"

void process_term_dict (const string &term,
                        unordered_map<uint32_t, int> &term_count_map,
                        unordered_map<string, int> &dict,
                        vector<string> &terms_vec
) {
  typename unordered_map < string, int > :: const_iterator term_iterator =  dict.find(term);
  // id of last observed unique term
  // we use incremented ids, so set new index dict.size()
  // and then grow dict => insert new term
  int term_id;
  // new unobserved term - add to dictionary
  if(term_iterator == dict.end()) {
    term_id = dict.size();
    dict.insert(make_pair(term, term_id));
    // keep word in order (we don't want to use bi-directional map)
    terms_vec.push_back(term);
  }
  // dictionary already contains document => get index
  else {
    term_id = term_iterator -> second;
  }
  // increment count for input term
  ++term_count_map[term_id];
}

// implements hashing trick
void process_term_hash (const string &term,
                        unordered_map<uint32_t, int> &term_count_map,
                        uint32_t buckets_size,
                        std::function<uint32_t(string)> hash_fun) {

  uint32_t term_id = hash_fun(term) % buckets_size;
  ++term_count_map[term_id];
}

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
  int doc_len;
  int doc_id;
};

class Corpus {
public:

  void insert_document( CharacterVector terms);

  void insert_document_batch( ListOf<CharacterVector> docs_batch);

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

class DictCorpus: public Corpus {
public:
  // constructor
  DictCorpus() {
    token_count = 0;
    doc_count = 0;
  };
  // total number of tokens in corpus
  int get_token_count() {return this->get_token_count();};

  // dictionary
  unordered_map<string, int> dict;

  // total number of documents in corpus
  int get_doc_count() { return doc_count; };

  void insert_document(const vector <string> &terms) {
    // map represents pair of (word_id, word_count)
    // we add terms to dictionary iteratively
    // each new term has incremented index
    unordered_map<uint32_t, int> term_count_map;
    for (auto element : terms)
      process_term_dict (element, term_count_map, this->dict, this->global_terms);
    // add row - update sparse matrix values
    insert_dtm_doc(term_count_map);
  }

  void insert_document_batch(const vector < vector <string> > &docs_batch)  {
    for (auto it:docs_batch)
      insert_document(it);
    //Rprintf("token_count = %d\n", token_count);
  }

  SEXP get_dtm(IntegerVector type) {
    switch (type[0]) {
    case 0:
      return get_dtm_dgT (global_terms.size());
    case 1:
      return get_dtm_lda_c();
    case 2:
      return get_dtm_minhash();
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
    token_count = 0;
    buckets_size = size;
    hash_fun = [=](string x) { return hash_fn(x) % buckets_size; };
  };
  // total number of tokens in corpus
  int get_token_count() {return this->get_token_count();};
  // total number of documents in corpus
  int get_doc_count() { return doc_count; };

  void insert_document(const vector<string> &terms) {

    unordered_map<uint32_t, int> term_count_map;

    // iterate trhough input global_terms
    for (auto element : terms)
      process_term_hash(element, term_count_map, this-> buckets_size, this -> hash_fun);

    insert_dtm_doc(term_count_map);
  }

  void insert_document_batch(const vector< vector <string > >  docs)  {
    for (auto it:docs)
      insert_document(it);
    //Rprintf("token_count = %d\n", token_count);
  }

  SEXP get_dtm(int type) {
    switch (type) {
    case 0:
      return get_dtm_dgT (buckets_size);
    case 1:
      return get_dtm_lda_c();
    case 2:
      return get_dtm_minhash();
    default:
      return R_NilValue;
    }
  }

private:
  // we use std::hash
  // investigate approaches to use murmurhash3 from digest package instead
  std::hash<string> hash_fn;
  std::function<uint32_t(string)> hash_fun;
  uint32_t buckets_size;
};

RCPP_MODULE(DictCorpus) {
  class_< DictCorpus >( "DictCorpus" )
  .constructor()
  .field_readonly( "dict", &DictCorpus::dict )
  .property( "token_count", &DictCorpus::get_token_count )
  .method( "get_doc_count", &DictCorpus::get_doc_count )
  .method( "insert_document", &DictCorpus::insert_document )
  .method( "insert_document_batch", &DictCorpus::insert_document_batch )
  .method( "get_dtm", &DictCorpus::get_dtm )
  ;
}

RCPP_MODULE(HashCorpus) {
  class_< HashCorpus >( "HashCorpus" )
  .constructor<uint32_t>()
  .property( "token_count", &HashCorpus::get_token_count )
  .method( "get_doc_count", &HashCorpus::get_doc_count )
  .method( "insert_document", &HashCorpus::insert_document )
  .method( "insert_document_batch", &HashCorpus::insert_document_batch )
  .method( "get_dtm", &HashCorpus::get_dtm )
  ;
}
