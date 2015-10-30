#include "VocabCorpus.h"
RCPP_MODULE(VocabCorpus) {
  class_< VocabCorpus >( "VocabCorpus" )
  //<terms, ngram_min, ngram_max, ngram_delim>
  //.constructor<CharacterVector, uint32_t, uint32_t, string >()
    .constructor<CharacterVector, uint32_t, uint32_t >()
  //<terms, ngram_min, ngram_max, window_size, ngram_delim>
  //.constructor<CharacterVector, uint32_t, uint32_t, uint32_t, string >()
  .constructor<CharacterVector, uint32_t, uint32_t, uint32_t >()
  .method( "vocab", &VocabCorpus::get_vocab, "vocabulary - unique terms")
  .method( "token_count", &VocabCorpus::get_token_count, "returns number of tokens in corpus" )
  .method( "document_count", &VocabCorpus::get_doc_count, "returns number of documents in corpus")
  .method( "insert_document", &VocabCorpus::insert_document, "inserts new document (character vector) into corpus" )
  .method( "insert_document_batch", &VocabCorpus::insert_document_batch, "inserts multiple documents (list of character vectors) into corpus" )
  .method( "insert_document_cooc", &VocabCorpus::insert_document_cooc, "inserts single sentence (character vector) into glove corpus" )
  .method( "insert_document_cooc_batch", &VocabCorpus::insert_document_cooc_batch, "inserts multiple sentences (list of character vectors) into glove corpus" )
  .method( "get_tcm", &VocabCorpus::get_tcm, "construct Term-Cooccurence matrix" )
  .method( "get_dtm", &VocabCorpus::get_dtm, "construct Document-Term matrix (various forms) from corpus" )
  ;
}
