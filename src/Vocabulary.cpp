#include "Vocabulary.h"
RCPP_MODULE(VocabularyBuilder) {
  class_< Vocabulary >( "VocabularyBuilder" )
  .constructor<uint32_t, uint32_t, const CharacterVector>()
  .method( "insert_document", &Vocabulary::insert_document, "inserts document into corpus" )
  .method( "insert_document_batch", &Vocabulary::insert_document_batch, "inserts multiple documents into corpus" )
  .method( "get_vocab_statistics", &Vocabulary::get_vocab_statistics, "returns vocabulary stat data.frame")
  .method( "get_document_count", &Vocabulary::get_document_count, "returns number of documents vocabulary was built" )
  ;
}
