#include "HashCorpus.h"

RCPP_MODULE(HashCorpus) {
  class_< HashCorpus >( "HashCorpus" )
  //.constructor<uint32_t, int, uint32_t, uint32_t, string>()
  // <bucket_size, ngram_minm ngram_max, skipgram_window_size, flag_signed_hash>
  .constructor<uint32_t, uint32_t, uint32_t, uint32_t, int>()
  .method( "get_token_count", &HashCorpus::get_token_count, "returns number of tokens in corpus" )
  .method( "get_doc_count", &HashCorpus::get_doc_count, "returns number of documents in corpus")
  .method( "insert_document", &HashCorpus::insert_document, "inserts new document (character vector) into corpus" )
  .method( "insert_document_batch", &HashCorpus::insert_document_batch, "inserts multiple documents (list of character vectors) into corpus" )
  .method( "get_dtm", &HashCorpus::get_dtm, "construct Document-Term matrix from corpus (triplet form - dgTMatrix)" )
  .method( "get_dtm", &HashCorpus::get_tcm, "construct Term-Cooccurence matrix from corpus (triplet form - dgTMatrix)" )
  .method( "clear_tcm", &HashCorpus::clear_tcm, "construct Document-Term matrix (various forms) from corpus" )
  .method( "get_tcm_size", &HashCorpus::get_tcm_size, "returns current tcm size (number of non-zero elements)" )
  ;
}
