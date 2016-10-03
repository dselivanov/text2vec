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

#include "VocabCorpus.h"
RCPP_MODULE(VocabCorpus) {
  class_< VocabCorpus >( "VocabCorpus" )
  //<vocab, ngram_min, ngram_max, window_size = 0>
  .constructor<CharacterVector, uint32_t, uint32_t, uint32_t, const CharacterVector, const String>()
  .method( "vocab", &VocabCorpus::get_vocab, "vocabulary - unique terms")
  .method( "get_token_count", &VocabCorpus::get_token_count, "returns number of tokens in corpus" )
  .method( "get_doc_count", &VocabCorpus::get_doc_count, "returns number of documents in corpus")
  .method( "insert_document", &VocabCorpus::insert_document, "inserts new document (character vector) into corpus" )
  .method( "insert_document_batch", &VocabCorpus::insert_document_batch, "inserts multiple documents (list of character vectors) into corpus" )
  .method( "get_tcm", &VocabCorpus::get_tcm, "construct Term-Cooccurence matrix" )
  .method( "get_dtm", &VocabCorpus::get_dtm, "construct Document-Term matrix (various forms) from corpus" )
  .method( "clear_tcm", &VocabCorpus::clear_tcm, "clear Term-Cooccurence < pair<i, j>, x > unordered_map" )
  .method( "get_tcm_size", &VocabCorpus::get_tcm_size, "returns current tcm size (number of non-zero elements)" )
  ;
}
