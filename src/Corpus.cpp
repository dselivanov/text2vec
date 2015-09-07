#include <Rcpp.h>
#include <unordered_map>
using namespace Rcpp;
using namespace std;
// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

class TmliteCorpus {
public:
  // constructor
  TmliteCorpus(): doc_count(0) {};

  int doc_count;
  unordered_map<string, int> dict;

  void insert_document(vector < vector< string > > text_list) {
    typename unordered_map < string, int > :: const_iterator element_it;
    for (auto it : text_list) {
      vector< string > current_doc = it;
      int K = current_doc.size();
      int col_index;
      unordered_map<int, int> indices;
      for (auto element : current_doc) {
        element_it = dict.find(element);
        if(element_it == dict.end()) {
          //col_index = dict.size() + 1;
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

RCPP_MODULE(TmliteCorpus) {
  class_< TmliteCorpus >( "TmliteCorpus" )
  .constructor()
  .field_readonly( "doc_count", &TmliteCorpus::doc_count )
  .field_readonly( "dict", &TmliteCorpus::dict )
  .method( "insert_document", &TmliteCorpus::insert_document )
  .method( "get_dtm", &TmliteCorpus::get_dtm )
  ;
}
