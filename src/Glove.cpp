#include "tmlite.h"

// SEXP get_cooccurence_matrix(CharacterVector sentence, int window_size, std::function<double(int)> weighting_fun = linear_inverse_decay) {

class GloveCorpus {
public:
  GloveCorpus() {};
  // constructor with dictionary
  GloveCorpus(CharacterVector dict_R) {
    int dict_size = dict_R.size(),
      i = 0;
    this->truncated_dict.reserve(dict_size);
    // convert R char_vec into C++ map
    for (auto val:dict_R) {
      //grow dictionary
      truncated_dict.insert(make_pair(as<string>(val), i));
      i++;
    }
  };

  // insert single sentence (vector<string>) into dictionary
  void grow_dict(CharacterVector sentence) {
    string s;
    typename unordered_map <  string, pair<int, int> > :: iterator word_iterator;
    for(int i = 0; i < sentence.size(); i++) {
      s = sentence[i];
      word_iterator = this->full_dict.find(s);
      if(word_iterator == this->full_dict.end())
        this->full_dict.insert(make_pair(s, make_pair( this->full_dict.size(), 1L) ) );
      else
        word_iterator->second.second++;
    }
  }
  void grow_dict_batch(ListOf<CharacterVector> sentence_batch) {
    for(auto s:sentence_batch)
      grow_dict(s);
  }
  // truncate dictionary - delete uncommon terms
  void truncate_dict(int n_min ) {
    int i = 0;
    // create only
    if(truncated_dict.empty())
      for (auto it:this->full_dict) {
        if(it.second.second >= n_min ) {
          truncated_dict.insert(make_pair(it.first, i));
          i++;
        }
      }
    else Rprintf("warning : dict already exists - do nothing\n");
  }
  // TODO add weighting_fun parameter, like this:
  // std::function<double(int)> weighting_fun = linear_inverse_decay
  void insert_sentence(CharacterVector sentence, int window_size) {
    int K = sentence.size();
    int main_word_index, context_word_index;
    double increment = 0.0;
    typename unordered_map < string, int > :: const_iterator main_word_iterator, context_word_iterator;
    // typename unordered_map < string, int > :: const_iterator context_word_iterator;
    for(int i = 0; i < K; i++) {
      main_word_iterator = this->truncated_dict.find(  as<string>(sentence[i]) );
      // if main word in dict
      if(main_word_iterator != this->truncated_dict.end()) {
        // get main word index from dict
        main_word_index = main_word_iterator->second;
        for (int j = 1; j <= window_size; j++) {
          // check sentence bounds
          if( i + j < K) {
            context_word_iterator = this->truncated_dict.find( as<string>(sentence[i + j]) );
            // if context word in dict
            if(context_word_iterator != this->truncated_dict.end()) {
              // get context word index from dict
              context_word_index = context_word_iterator->second;
              // calculate cooccurence increment for particular position j of context word
              increment = weighting_fun(j);
              this->cooc_matrix[make_pair(main_word_index, context_word_index)] += increment;
              this->cooc_matrix[make_pair(context_word_index, main_word_index)] += increment;
            }
          }
        }
      }
    }
  }
  void insert_sentence_batch(ListOf<CharacterVector> sentence_batch, int window_size) {
    for(auto s:sentence_batch)
      insert_sentence(s, window_size);
  }
  S4 get_cooc_matrix() {
    // non-zero values count
    int N = cooc_matrix.size();
    // matrix dimensions
    int dim_size = truncated_dict.size();

    // result triplet sparse matrix
    S4 triplet_cooc_matrix("dgTMatrix");
    // index vectors
    IntegerVector cooc_i(N);
    IntegerVector cooc_j(N);
    // value vector
    NumericVector cooc_x(N);

    int i = 0;
    for(auto it : cooc_matrix) {
      cooc_i[i] = it.first.first;
      cooc_j[i] = it.first.second;
      cooc_x[i] = it.second;
      i++;
    }
    // construct matrix
    triplet_cooc_matrix.slot("i") = cooc_i;
    triplet_cooc_matrix.slot("j") = cooc_j;
    triplet_cooc_matrix.slot("x") = cooc_x;
    // set dimensions
    triplet_cooc_matrix.slot("Dim") = IntegerVector::create(dim_size, dim_size);
    // set dimension names
    vector<string>  triplet_cooc_matrix_names;
    triplet_cooc_matrix_names.resize( truncated_dict.size() );
    for(auto it:truncated_dict)
      triplet_cooc_matrix_names[it.second] = it.first;
    triplet_cooc_matrix.slot("Dimnames") = List::create(triplet_cooc_matrix_names, triplet_cooc_matrix_names);
    return triplet_cooc_matrix;
  }

private:
  unordered_map< string, pair<int, int>> full_dict;
  unordered_map< string, int> truncated_dict;
  unordered_map< pair< int, int >, double > cooc_matrix;

  inline double weighting_fun(int offset) {
    return 1.0 / offset;
  }

};

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
