#include "Vocabulary.h"
using namespace Rcpp;
using namespace std;

RCPP_MODULE(Vocabulary) {
  class_< Vocabulary >( "Vocabulary" )
  .constructor<uint32_t, uint32_t, string>()
  .method( "insert_document", &Vocabulary::insert_document, "inserts document into corpus" )
  .method( "insert_document_batch", &Vocabulary::insert_document_batch, "inserts multiple documents into corpus" )
  .method( "vocab_stat", &Vocabulary::vocab_stat, "returns vocabulary stat matrix")
  ;
}
