#include <Rcpp.h>
#include <unordered_map>
using namespace Rcpp;
using namespace std;
// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

class DictCorpus {
public:
  // constructor
  DictCorpus(): doc_count(0) {};

  int doc_count;
  unordered_map<string, int> dict;

  void insert_document( vector< string > words) {
    typename unordered_map < string, int > :: const_iterator element_it;
    int K = words.size();
    int col_index;
    unordered_map<int, int> indices;
    for (auto element : words) {
      element_it = dict.find(element);
      if(element_it == dict.end()) {
        col_index = dict.size();
        dict.insert(make_pair(element, col_index));
        terms.push_back(element);
      } else {
        col_index = element_it -> second;
      }
      ++indices[col_index];
    }

    for (auto element : indices) {
      j.push_back(element.first);
      x.push_back(element.second);
      i.push_back(doc_count);
    }
    doc_count++;
  }
  void insert_document_batch( vector< vector< string > > docs) {
    //typename vector < string> :: const_iterator element_it;
    for (auto it:docs) {
      vector<string> words = it;
      insert_document(it);
    }
  }
  SEXP get_dtm() {
    vector<double> xdouble(x.begin(), x.end());
    S4 dtm("dgTMatrix");
    dtm.slot("i") = i;
    dtm.slot("j") = j;
    dtm.slot("x") = xdouble;
    dtm.slot("Dim") = IntegerVector::create(doc_count, terms.size()) ;
    dtm.slot("Dimnames") = List::create(R_NilValue, terms);
    return dtm;
  }
  List get_dtm_list() {
    return List::create(Named("i") = i,
                        Named("j") = j,
                        Named("x") = x,
                        Named("terms") = terms);
  }
private:
  vector<int> i;
  vector<int> j;
  vector<int> x;
  vector<string> terms;
};

RCPP_MODULE(DictCorpus) {
  class_< DictCorpus >( "DictCorpus" )
  .constructor()
  .field_readonly( "doc_count", &DictCorpus::doc_count )
  .field_readonly( "dict", &DictCorpus::dict )
  .method( "insert_document", &DictCorpus::insert_document )
  .method( "insert_document_batch", &DictCorpus::insert_document_batch )
  .method( "get_dtm", &DictCorpus::get_dtm )
  ;
}
