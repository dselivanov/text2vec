#include "Corpus.hpp"
# define TOKEN_VERBOSE 1000000
class VocabCorpus;

using namespace Rcpp;
using namespace std;

// fast integer hashing
uint32_t fast_int_hash(uint32_t a) {
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a) * 0x45d9f3b;
  a = ((a >> 16) ^ a);
  return a;
}

// for unordered_map < <int, int>, uint32_t >
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

class VocabCorpus: public Corpus {
public:
  // contructor for corpus with user-defined vocabulary
  VocabCorpus(CharacterVector vocab_R, uint32_t n_min, uint32_t n_max, string delim ) {
    verbose = 1;
    nnz = 0;
    token_count = 0;
    doc_count = 0;
    cooc_tokens_number = 0;

    ngram_min = n_min;
    ngram_max = n_max;
    // ngram concatenation delimiter
    ngram_delim = delim;

    int vocab_size = vocab_R.size();
    int i = 0;
    // we know vocab size, so lets reserve buckets this number
    // and if we will lucky no rehash will needed
    this->vocab.reserve(vocab_size);
    //convert R vocab represenation to C++ represenation
    // also fill terms in right order
    for (auto val:vocab_R) {
      //grow vocabulary
      vocab.insert(make_pair(as<string>(val), i));
      // fill terms in order we add them in dctionary!
      i++;
    }
  };

  void insert_terms (vector< string> &terms) {

    unordered_map<uint32_t, uint32_t> term_count_map;

    typename unordered_map < string, uint32_t > :: const_iterator term_iterator;

    for(auto term:terms) {
      this->token_count++;

      term_iterator = this->vocab.find(term);

      // get term index
      if(term_iterator != this->vocab.end()) {
        uint32_t term_id = term_iterator -> second;
        // increment count for input term
        ++term_count_map[term_id];
      }
    }

    this->insert_dtm_doc(term_count_map);
  }

  void insert_document(const CharacterVector terms) {
    vector< string > ngrams = get_ngrams(terms, ngram_min, ngram_max, ngram_delim);
    insert_terms(ngrams);
  }

  void insert_document_batch(const ListOf<const CharacterVector> docs_batch) {
    for (auto it:docs_batch)
      insert_document(it);
  }

  void insert_document_cooc(const CharacterVector sentence) {

    int K = sentence.size(), main_word_index, context_word_index;
    float increment = 0.0;

    typename unordered_map < string, uint32_t > :: const_iterator main_word_iterator, context_word_iterator;

    for(int i = 0; i < K; i++) {
      cooc_tokens_number++;

      if( this->verbose && cooc_tokens_number % TOKEN_VERBOSE == 0)
        Rprintf("%d M tokens processed, matrix size = %d\n", cooc_tokens_number / TOKEN_VERBOSE , cooc_matrix.size() );

      main_word_iterator = this->vocab.find(  as<string>(sentence[i]) );
      // if main word in vocab
      if(main_word_iterator != this->vocab.end()) {
        // get main word index from vocab
        main_word_index = main_word_iterator->second;
        for (int j = 1; j <= this->window_size; j++) {
          // check sentence bounds
          if( i + j < K) {
            context_word_iterator = this->vocab.find( as<string>(sentence[i + j]) );
            // if context word in vocab
            if(context_word_iterator != this->vocab.end()) {
              // get context word index from vocab
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

  void insert_document_cooc_batch(const ListOf<const CharacterVector> sentence_batch) {
    for(auto s:sentence_batch)
      insert_document_cooc(s);
  }

  // total number of tokens in corpus
  int get_token_count() {return this->get_token_count();};
  int get_doc_count() { return this->get_doc_count(); };

  CharacterVector get_vocab() {
    CharacterVector vocab_R(vocab.size());
    for(auto i:vocab)
      vocab_R[ i.second ] = i.first;
    return vocab_R;
  }

  // get term cooccurence matrix
  SEXP get_tcm() {
    // non-zero values count
    size_t N = cooc_matrix.size();
    // matrix dimensions
    int dim_size = vocab.size();

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
    triplet_cooc_matrix_names.resize( vocab.size() );
    for(auto it:vocab)
      triplet_cooc_matrix_names[it.second] = it.first;
    triplet_cooc_matrix.slot("Dimnames") = List::create(triplet_cooc_matrix_names, triplet_cooc_matrix_names);
    return triplet_cooc_matrix;
  }

  SEXP get_dtm_triplet() {
    size_t ncol = this->vocab.size();

    vector<string> terms;
    terms.resize(ncol);

    for(auto it:vocab)
      terms[it.second] = it.first;

    int i = 0;
    NumericVector dtm_x(this->nnz);
    IntegerVector dtm_i(this->nnz);
    IntegerVector dtm_j(this->nnz);

    for (auto doc: docs) {
      for (int j = 0; j < doc.doc_len; j++) {
        dtm_i[i] = doc.doc_id;
        dtm_j[i] = doc.term_ids[j];
        dtm_x[i] = doc.term_counts[j];
        i++;
      }
    }

    S4 dtm("dgTMatrix");
    dtm.slot("i") = dtm_i;
    dtm.slot("j") = dtm_j;
    dtm.slot("x") = dtm_x;
    dtm.slot("Dim") = IntegerVector::create(doc_count, ncol) ;
    dtm.slot("Dimnames") = List::create(R_NilValue, terms);

    return dtm;
  }

  SEXP get_dtm(IntegerVector type) {
    switch (type[0]) {
    case 0:
      return get_dtm_triplet ();
    case 1:
      return get_dtm_lda_c();
    case 2:
      return get_dtm_minhash();
    default:
      return R_NilValue;
    }
  }
private:
  int verbose;
  //#####Glove related
  uint64_t cooc_tokens_number;
  // vocabulary
  unordered_map<string, uint32_t> vocab;
  // term cooccurence matrix
  unordered_map< pair< int, int >, float > cooc_matrix;
  int window_size;
  size_t cooc_token_count;
  inline float weighting_fun(int offset) {
    return 1.0 / (float)offset;
  }

};
