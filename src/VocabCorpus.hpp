#include "text2vec.h"
#include "Document.hpp"
#include "Corpus.hpp"

void process_term_vocab (const string &term,
                        unordered_map<uint32_t, int> &term_count_map,
                        unordered_map<string, int> &vocab,
                        vector<string> &terms_vec,
                        int flag_vocab_fixed) {
  typename unordered_map < string, int > :: const_iterator term_iterator;
  term_iterator = vocab.find(term);
  int term_id;
  // new unobserved term
  if(term_iterator == vocab.end()) {
    // here we will process term using user-supplied vocabulary
    if(flag_vocab_fixed) {
      // DO NOTHING!
      // because term is not in our predefined vocabulary
    }
    // here we will precess term and grow vocabulary
    else {
      // get new term id
      // we use incremented ids, so set new id to vocab.size()
      term_id = vocab.size();
      // insert term into vocabulary
      vocab.insert(make_pair(term, term_id));
      // keep terms in the same order we insert them into vocabulary
      // (we don't want to use bi-directional map)
      terms_vec.push_back(term);
      // increment count for input term
      // actually this mean term_count_map[term_id] = 1
      // (as map initialized by zeros by default)
      ++term_count_map[term_id];
    }
  }
  // vocabulary already contains term => get index
  else {
    term_id = term_iterator -> second;
    // increment count for input term
    ++term_count_map[term_id];
  }
}


class VocabCorpus: public Corpus {
public:
  // default constructor
  VocabCorpus() {
    token_count = 0;
    doc_count = 0;
    flag_vocab_fixed = 0;
  };
  // contructor for corpus with user-defined vocabulary
  VocabCorpus(CharacterVector vocab_R ) {

    token_count = 0;
    doc_count = 0;
    int vocab_size = vocab_R.size();
    int i = 0;
    // make unordered_map<string, int> vocabulary
    // from R's named integer vector
    // allocate memory
    this->global_terms.resize(vocab_size);
    // we know vocab size, so lets reserve buckets this number
    // and if we will lucky no rehash will needed
    this->vocab.reserve(vocab_size);
    //convert R vocab represenation to C++ represenation
    // also fill terms in right order
    for (auto val:vocab_R) {
      //grow vocabulary
      vocab.insert(make_pair(as<string>(val), i));
      // fill terms in order we add them in dctionary!
      this->global_terms[ i ] = as<string>(val);
      i++;
    }
    flag_vocab_fixed = 1;
  };
  // total number of tokens in corpus
  int get_token_count() {return this->get_token_count();};
  CharacterVector get_vocab() {
    CharacterVector vocab_R(vocab.size());
    for(auto i:vocab)
      vocab_R[ i.second ] = i.first;
    return vocab_R;
  }
  // vocabulary
  unordered_map<string, int> vocab;
  // logical flag should we grow vocabulary or use not (use user-supplied)
  // we should set up this flag in constructor call
  int flag_vocab_fixed;

  // total number of documents in corpus
  int get_doc_count() { return doc_count; };

  void insert_document(const vector <string> &terms, int ngram_min, int ngram_max, const string ngram_delim = "_") {
    // map represents pair of (word_id, word_count)
    // we add terms to vocabulary iteratively
    // each new term has incremented index
    unordered_map<uint32_t, int> term_count_map;

    if(ngram_min == 1 && ngram_max == 1) {
      // iterate trhough input global_terms
      for (auto term : terms) {
        process_term_vocab (term, term_count_map, this->vocab, this->global_terms, this->flag_vocab_fixed);
      }
    }
    // harder case - n-grams
    else {
      //lamda which defines how to process each ngram term
      std::function<void(string)> process_term_fun = [&](string x) {
        process_term_vocab (x, term_count_map, this->vocab, this->global_terms, this->flag_vocab_fixed);
      };
      ngram(terms,
            process_term_fun,
            ngram_min, ngram_max, ngram_delim);
    }
    // add row - update sparse matrix values
    insert_dtm_doc(term_count_map);
  }

  void insert_document_batch(const vector < vector <string> > &docs_batch, int ngram_min, int ngram_max, const string ngram_delim = "_") {
    for (auto it:docs_batch)
      insert_document(it, ngram_min, ngram_max, ngram_delim);
    //Rprintf("token_count = %d\n", token_count);
  }

  SEXP get_dtm(IntegerVector type) {
    switch (type[0]) {
    case 0:
      return get_dtm_dgT (global_terms.size());
    case 1:
      return get_dtm_lda_c();
    case 2:
      return get_dtm_minhash();
    default:
      return R_NilValue;
    }
  }
};
