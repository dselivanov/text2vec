#include "Glove.hpp"

RCPP_MODULE(GloveCorpus) {
  class_< GloveCorpus >( "GloveCorpus" )
  .constructor()
  .constructor<CharacterVector>()

  .method( "insert_sentence", &GloveCorpus::insert_sentence, "inserts sentence into corpus" )
  .method( "insert_sentence_batch", &GloveCorpus::insert_sentence_batch, "inserts multiple sentences into corpus" )

  .method( "grow_dict", &GloveCorpus::grow_dict, "grows dictionary from sentence" )
  .method( "grow_dict_batch", &GloveCorpus::grow_dict_batch, "grows dictionary from batch of sentence" )

  .method( "truncate_dict", &GloveCorpus::truncate_dict, "grows dictionary from sentence" )

  .method( "get_cooc_matrix", &GloveCorpus::get_cooc_matrix, "returns cooccurence matrix")
  ;
}
