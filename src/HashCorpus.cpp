#include "HashCorpus.h"

RCPP_MODULE(HashCorpus) {
  class_< HashCorpus >( "HashCorpus" )
  //.constructor<uint32_t, int, uint32_t, uint32_t, string>()
    .constructor<uint32_t, int, uint32_t, uint32_t>()
  .method( "token_count", &HashCorpus::get_token_count, "returns number of tokens in corpus" )
  .method( "document_count", &HashCorpus::get_doc_count, "returns number of documents in corpus")
  .method( "insert_document", &HashCorpus::insert_document, "inserts new document (character vector) into corpus" )
  .method( "insert_document_batch", &HashCorpus::insert_document_batch, "inserts multiple documents (list of character vectors) into corpus" )
  .method( "get_dtm", &HashCorpus::get_dtm, "construct Document-Term matrix (various forms) from corpus" )
  ;
}
