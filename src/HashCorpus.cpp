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
