#include "text2vec.h"

// fast integer hashing
uint32_t fast_int_hash(uint32_t a) {
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a);
  return a;
}

namespace std {
  template <>
  struct hash<std::pair<int, int>>
  {
    inline size_t operator()(const std::pair<int, int>& k) const
    {
      return fast_int_hash(k.first) + fast_int_hash(k.second);
    }
  };
}

// SEXP get_cooccurence_matrix(CharacterVector sentence, int window_size, std::function<double(int)> weighting_fun = linear_inverse_decay) {
class GloveCorpus {
public:
  GloveCorpus(): tokens_number(0) {};
  // constructor with dictionary
  GloveCorpus(CharacterVector dict_R): tokens_number(0) {
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
  void insert_sentence(CharacterVector sentence, int window_size, int verbose = 1) {
    int K = sentence.size();
    int main_word_index, context_word_index;
    double increment = 0.0;
    // size_t non_zero_count = 0;
    typename unordered_map < string, int > :: const_iterator main_word_iterator, context_word_iterator;
    // typename unordered_map < string, int > :: const_iterator context_word_iterator;
    for(int i = 0; i < K; i++) {

      tokens_number++;
      if( verbose && tokens_number % 1000000 == 0)
        Rprintf("%d tokens processed, matrix size = %d\n", tokens_number, cooc_matrix.size() );
      main_word_iterator = this->truncated_dict.find(  as<string>(sentence[i]) );
      // if main word in dict
      if(main_word_iterator != this->truncated_dict.end()) {
        // get main word index from dict
        main_word_index = main_word_iterator->second;
        // TODO
        // replace with loop and don't count words which are not in dict ???
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
              // map stores only elements above diagonal because
              // our matrix is symmetrical
              if(main_word_index < context_word_index) {
                this->cooc_matrix[make_pair(main_word_index, context_word_index)] += increment;
              }
              else {
                // also we are not interested in context words equal to main word
                // diagonal elememts will be zeros
                if(main_word_index != context_word_index)
                  this->cooc_matrix[make_pair(context_word_index, main_word_index)] += increment;
              }
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
    size_t N = cooc_matrix.size();
    // matrix dimensions
    int dim_size = truncated_dict.size();

    // result triplet sparse matrix
    S4 triplet_cooc_matrix("dgTMatrix");
    // index vectors
    IntegerVector cooc_i(2*N);
    IntegerVector cooc_j(2*N);
    // value vector
    NumericVector cooc_x(2*N);

    int n = 0, i, j;
    double x;
    for(auto it : cooc_matrix) {
      i = it.first.first;
      j = it.first.second;
      x = it.second;
      // fill first half of our symmetric cooccurence matrix
      cooc_i[n] = i;
      cooc_j[n] = j;
      cooc_x[n] = x;
      // fill second half of our symmetric cooccurence matrix
      cooc_i[n+N] = j;
      cooc_j[n+N] = i;
      cooc_x[n+N] = x;
      n++;
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
  size_t tokens_number;
  inline double weighting_fun(int offset) {
    return 1.0 / (double)offset;
  }

};
