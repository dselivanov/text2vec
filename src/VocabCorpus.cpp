#include "VocabCorpus.hpp"
RCPP_MODULE(VocabCorpus) {
  class_< VocabCorpus >( "VocabCorpus" )
  .constructor()
  .constructor<CharacterVector>()
  //.field_readonly( "voacb", &VocabCorpus::voacb, "vocabulary - unique terms with corresponding indices")
    .property( "voacb", &VocabCorpus::get_vocab, "vocabulary - unique terms")
    .property( "token_count", &VocabCorpus::get_token_count, "returns number of tokens in corpus" )
    .method( "document_count", &VocabCorpus::get_doc_count, "returns number of documents in corpus")
    .method( "insert_document", &VocabCorpus::insert_document, "inserts new document (character vector) into corpus" )
    .method( "insert_document_batch", &VocabCorpus::insert_document_batch, "inserts multiple documents (list of character vectors) into corpus" )
    .method( "get_dtm", &VocabCorpus::get_dtm, "construct Document-Term matrix (various forms) from corpus" )
  ;
}
