#include "text2vec.h"

void process_term_dict (const string &term,
                        unordered_map<uint32_t, int> &term_count_map,
                        unordered_map<string, int> &dict,
                        vector<string> &terms_vec,
                        int flag_dict_fixed) {
  typename unordered_map < string, int > :: const_iterator term_iterator;
  term_iterator = dict.find(term);
  int term_id;
  // new unobserved term
  if(term_iterator == dict.end()) {
    // here we will process term using user-supplied dictionary
    if(flag_dict_fixed) {
      // DO NOTHING!
      // because term is not in our predefined dictionary
    }
    // here we will precess term and grow dictionary
    else {
      // get new term id
      // we use incremented ids, so set new id to dict.size()
      term_id = dict.size();
      // insert term into dictionary
      dict.insert(make_pair(term, term_id));
      // keep terms in the same order we insert them into dictionary
      // (we don't want to use bi-directional map)
      terms_vec.push_back(term);
      // increment count for input term
      // actually this mean term_count_map[term_id] = 1
      // (as map initialized by zeros by default)
      ++term_count_map[term_id];
    }
  }
  // dictionary already contains term => get index
  else {
    term_id = term_iterator -> second;
    // increment count for input term
    ++term_count_map[term_id];
  }
}

// implements hashing trick
void process_term_hash (const string &term,
                        unordered_map<uint32_t, int> &term_count_map,
                        uint32_t buckets_size,
                        int signned_hash = 0) {
  uint32_t term_id = murmurhash3_hash(term) % buckets_size;

  if(signned_hash) {
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
// Document is map! (term_id -> term_count)
// but we represent it by two corresponding vectors
// this done for simplicity in further steps
// also we strore some additional metadata - see comments in code
class Document {
public:
  //contructor
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
  //global term ids (shared across Coprpus)
  vector<uint32_t> term_ids;
  // counts for each uniqe term
  vector<int> term_counts;
  // document length (number of unique terms)
  int doc_len;
  // document id
  int doc_id;
};

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

class DictCorpus: public Corpus {
public:
  // default constructor
  DictCorpus() {
    token_count = 0;
    doc_count = 0;
    flag_dict_fixed = 0;
  };
  // contructor for corpus with user-defined dictionary
  DictCorpus(CharacterVector dict_R ) {

    token_count = 0;
    doc_count = 0;
    int dict_size = dict_R.size();
    int i = 0;
    // make unordered_map<string, int> dictionary
    // from R's named integer vector
    // allocate memory
    this->global_terms.resize(dict_size);
    // we know dict size, so lets reserve buckets this number
    // and if we will lucky no rehash will needed
    this->dict.reserve(dict_size);
    //convert R dict represenation to C++ represenation
    // also fill terms in right order
    for (auto val:dict_R) {
      //grow dictionary
      dict.insert(make_pair(as<string>(val), i));
      // fill terms in order we add them in dctionary!
      this->global_terms[ i ] = as<string>(val);
      i++;
    }
    flag_dict_fixed = 1;
  };
  // total number of tokens in corpus
  int get_token_count() {return this->get_token_count();};
  CharacterVector get_dict() {
    CharacterVector dict_R(dict.size());
    for(auto i:dict)
      dict_R[ i.second ] = i.first;
    return dict_R;
  }
  // dictionary
  unordered_map<string, int> dict;
  // logical flag should we grow dictionary or use not (use user-supplied)
  // we should set up this flag in constructor call
  int flag_dict_fixed;

  // total number of documents in corpus
  int get_doc_count() { return doc_count; };

  void insert_document(const vector <string> &terms, int ngram_min, int ngram_max, const string ngram_delim = "_") {
    // map represents pair of (word_id, word_count)
    // we add terms to dictionary iteratively
    // each new term has incremented index
    unordered_map<uint32_t, int> term_count_map;

    if(ngram_min == 1 && ngram_max == 1) {
      // iterate trhough input global_terms
      for (auto term : terms) {
        process_term_dict (term, term_count_map, this->dict, this->global_terms, this->flag_dict_fixed);
      }
    }
    // harder case - n-grams
    else {
      //lamda which defines how to process each ngram term
      std::function<void(string)> process_term_fun = [&](string x) {
        process_term_dict (x, term_count_map, this->dict, this->global_terms, this->flag_dict_fixed);
      };
      ngram(terms,
            process_term_fun,
            ngram_min, ngram_max, ngram_delim);
    }
    // add row - update sparse matrix values
    insert_dtm_doc(term_count_map);
  }

  void insert_document_batch(const vector < vector <string> > &docs_batch, int ngram_min, int ngram_max, const string ngram_delim = "_") {
    for (auto it:docs_batch)
      insert_document(it, ngram_min, ngram_max, ngram_delim);
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
  HashCorpus(uint32_t size, int use_signed_hash) {
    doc_count = 0;
    token_count = 0;
    buckets_size = size;
    signed_hash = use_signed_hash;
  };
  // total number of tokens in corpus
  int get_token_count() {return this->get_token_count();};
  // total number of documents in corpus
  int get_doc_count() { return doc_count; };

  void insert_document(const vector<string> &terms, int ngram_min, int ngram_max, const string ngram_delim = "_") {

    unordered_map<uint32_t, int> term_count_map;
    // simple unigrams
    // ngram also can process unigrams,
    // but following "if" state used for performance reasons
    if(ngram_min == 1 && ngram_max == 1) {
      // iterate trhough input global_terms
      for (auto term : terms) {
        process_term_hash(term, term_count_map, this-> buckets_size, this -> signed_hash);
      }
    }
    // harder case - n-grams
    else {
      //lamda which defines how to process each ngram term
      std::function<void(string)> process_term_fun = [&](string x) {
        process_term_hash(x, term_count_map, this-> buckets_size, this -> signed_hash);
      };
      ngram(terms,
            process_term_fun,
            ngram_min, ngram_max,
            ngram_delim);
    }

    insert_dtm_doc(term_count_map);
  }

  void insert_document_batch(const vector< vector <string > >  docs, int ngram_min, int ngram_max, const string ngram_delim = "_") {
    for (auto it:docs)
      insert_document(it, ngram_min, ngram_max, ngram_delim);
    //Rprintf("token_count = %d\n", token_count);
  }
  // R's interface to document-term matrix construction
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
  uint32_t buckets_size;
  int signed_hash;
};

RCPP_MODULE(DictCorpus) {
  class_< DictCorpus >( "DictCorpus" )
  .constructor()
  .constructor<CharacterVector>()
  //.field_readonly( "dict", &DictCorpus::dict, "dictionary - unique terms with corresponding indices")
  .property( "dict", &DictCorpus::get_dict, "dictionary - unique terms")
  .property( "token_count", &DictCorpus::get_token_count, "returns number of tokens in corpus" )
  .method( "document_count", &DictCorpus::get_doc_count, "returns number of documents in corpus")
  .method( "insert_document", &DictCorpus::insert_document, "inserts new document (character vector) into corpus" )
  .method( "insert_document_batch", &DictCorpus::insert_document_batch, "inserts multiple documents (list of character vectors) into corpus" )
  .method( "get_dtm", &DictCorpus::get_dtm, "construct Document-Term matrix (various forms) from corpus" )
  ;
}

RCPP_MODULE(HashCorpus) {
  class_< HashCorpus >( "HashCorpus" )
  .constructor<uint32_t, int>()
  .property( "token_count", &HashCorpus::get_token_count, "returns number of tokens in corpus" )
  .method( "document_count", &HashCorpus::get_doc_count, "returns number of documents in corpus")
  .method( "insert_document", &HashCorpus::insert_document, "inserts new document (character vector) into corpus" )
  .method( "insert_document_batch", &HashCorpus::insert_document_batch, "inserts multiple documents (list of character vectors) into corpus" )
  .method( "get_dtm", &HashCorpus::get_dtm, "construct Document-Term matrix (various forms) from corpus" )
  ;
}
