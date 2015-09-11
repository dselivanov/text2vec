#include <Rcpp.h>
#include <unordered_map>
using namespace Rcpp;
using namespace std;
// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

class Corpus {
public:

  void insert_document( CharacterVector words);

  void insert_document_batch( ListOf<CharacterVector> docs);

  int get_doc_count();

protected:

  vector<int> i;
  vector<int> j;
  vector<int> x;
  // terms
  vector<string> terms;
  //document counter
  int doc_count;

  void insert_dtm_doc(unordered_map<uint32_t, int> &indices) {
    for (auto element : indices) {
      this -> j.push_back(element.first);
      this -> x.push_back(element.second);
      this -> i.push_back(doc_count);
    }
    // doument inserted => increase document counter
    this -> doc_count++;
  }
  SEXP get_dtm_internal(const vector <string> &col_names, int ncol) {
    SEXP dtm_col_names;

    if(col_names.empty())
      dtm_col_names = R_NilValue;
    else dtm_col_names = wrap(col_names);

    NumericVector xdouble(x.begin(), x.end());
    S4 dtm("dgTMatrix");
    dtm.slot("i") = i;
    dtm.slot("j") = j;
    dtm.slot("x") = xdouble;
    dtm.slot("Dim") = IntegerVector::create(doc_count, ncol) ;
    dtm.slot("Dimnames") = List::create(R_NilValue, dtm_col_names);
    return dtm;
  }
};

class DictCorpus: public Corpus {
public:
  // constructor
  DictCorpus() {
    doc_count = 0;
  };

  // dictionary
  unordered_map<string, int> dict;
  // get number of inserted documents
  int get_doc_count() { return doc_count; };

  void insert_document( CharacterVector words) {
    typename unordered_map < string, int > :: const_iterator word_iterator;
    int col_index;
    // map represents pair of (word_id, word_count)
    // we add words to dictionary iteratively
    // each new word has incremented index
    unordered_map<uint32_t, int> indices;
    for (auto element : words) {
      word_iterator = dict.find(as<string>(element));
      // new element - add to dictionary
      // set index to the next int that is not used - dict.size()
      if(word_iterator == dict.end()) {
        col_index = dict.size();
        dict.insert(make_pair(as<string>(element), col_index));
        // keep word in order (we don't want to use bi-directional map)
        terms.push_back(as<string>(element));
      }
      // dictionary already contains document - get its index
      else {
        col_index = word_iterator -> second;
      }
      ++indices[col_index];
    }
    // add row - update sparse matrix values
    insert_dtm_doc(indices);
  }

  void insert_document_batch(ListOf<CharacterVector> docs)  {
    for (auto it:docs)
      insert_document(it);
  }
  SEXP get_dtm() {
    return get_dtm_internal (terms, terms.size());
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
  // we use std::hash
  // investigate approaches to use murmurhash from digest package instead
  std::hash<string> hash_fn;
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

  SEXP get_dtm() {
    return get_dtm_internal (terms, buckets_size);
  }
private:
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
