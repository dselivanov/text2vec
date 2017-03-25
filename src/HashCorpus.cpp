// Copyright (C) 2015 - 2016  Dmitriy Selivanov
// This file is part of text2vec
//
// text2vec is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// text2vec is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with text2vec.  If not, see <http://www.gnu.org/licenses/>.

#include "HashCorpus.h"

// RCPP_MODULE(HashCorpus) {
//   class_< HashCorpus >( "HashCorpus" )
//   //.constructor<uint32_t, int, uint32_t, uint32_t, string>()
//   // <bucket_size, ngram_minm ngram_max, flag_signed_hash>
//   .constructor<uint32_t, uint32_t, uint32_t, int>()
//   .method( "get_token_count", &HashCorpus::get_token_count, "returns number of tokens in corpus" )
//   .method( "get_doc_count", &HashCorpus::get_doc_count, "returns number of documents in corpus")
//   .method( "insert_document", &HashCorpus::insert_document, "inserts new document (character vector) into corpus" )
//   .method( "insert_document_batch", &HashCorpus::insert_document_batch, "inserts multiple documents (list of character vectors) into corpus" )
//   .method( "get_dtm", &HashCorpus::get_dtm, "construct Document-Term matrix from corpus (triplet form - dgTMatrix)" )
//   .method( "get_dtm", &HashCorpus::get_tcm, "construct Term-Cooccurence matrix from corpus (triplet form - dgTMatrix)" )
//   .method( "clear_tcm", &HashCorpus::clear_tcm, "construct Document-Term matrix (various forms) from corpus" )
//   .method( "get_tcm_size", &HashCorpus::get_tcm_size, "returns current tcm size (number of non-zero elements)" )
//   ;
// }


// [[Rcpp::export]]
SEXP cpp_hash_corpus_create(uint32_t size,
                            uint32_t n_min, uint32_t n_max,
                            int use_signed_hash) {
  HashCorpus *hc = new HashCorpus(size, n_min, n_max, use_signed_hash);
  XPtr<HashCorpus> ptr(hc, true);
  return ptr;
}

// [[Rcpp::export]]
void cpp_hash_corpus_insert_document_batch(SEXP ptr,
                                           const ListOf<const CharacterVector> &docs_batch,
                                           int grow_dtm,
                                           int context,
                                           uint32_t window_size,
                                           const NumericVector& weights) {
  Rcpp::XPtr<HashCorpus> hc(ptr);
  hc->insert_document_batch(docs_batch, grow_dtm, context, window_size, weights);
}

// [[Rcpp::export]]
S4 cpp_hash_corpus_get_tcm(SEXP ptr) {
  Rcpp::XPtr<HashCorpus> hc(ptr);
  return hc->get_tcm();
}

// [[Rcpp::export]]
S4 cpp_hash_corpus_get_dtm(SEXP ptr) {
  Rcpp::XPtr<HashCorpus> hc(ptr);
  return hc->get_dtm();
}
