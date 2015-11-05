#include "Vocabulary.h"
RCPP_MODULE(VocabularyBuilder) {
  class_< Vocabulary >( "VocabularyBuilder" )
  //.constructor<uint32_t, uint32_t, string>()
  .constructor<uint32_t, uint32_t>()
  .method( "insert_document", &Vocabulary::insert_document, "inserts document into corpus" )
  .method( "insert_document_batch", &Vocabulary::insert_document_batch, "inserts multiple documents into corpus" )
  .method( "get_vocab_statistics", &Vocabulary::get_vocab_statistics, "returns vocabulary stat data.frame")
  .method( "filter_vocab", &Vocabulary::filter_vocab, "filter vocabulary")
  ;
}
