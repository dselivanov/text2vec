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

#include "Vocabulary.h"
RCPP_MODULE(VocabularyBuilder) {
  class_< Vocabulary >( "VocabularyBuilder" )
  .constructor<uint32_t, uint32_t, const CharacterVector, const String>()
  .method( "insert_document", &Vocabulary::insert_document, "inserts document into corpus" )
  .method( "insert_document_batch", &Vocabulary::insert_document_batch, "inserts multiple documents into corpus" )
  .method( "get_vocab_statistics", &Vocabulary::get_vocab_statistics, "returns vocabulary stat data.frame")
  .method( "get_document_count", &Vocabulary::get_document_count, "returns number of documents vocabulary was built" )
  ;
}
