#include "Corpus.hpp"

void process_term_vocab (const string &term,
                        unordered_map<uint32_t, uint32_t> &term_count_map,
                        const unordered_map<string, uint32_t> &vocab) {
  typename unordered_map < string, uint32_t > :: const_iterator term_iterator;
  term_iterator = vocab.find(term);
  uint32_t term_id;
  // vocabulary already contains term => get index
  if(term_iterator != vocab.end()) {
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
  };
  // contructor for corpus with user-defined vocabulary
  VocabCorpus(CharacterVector vocab_R ) {

    token_count = 0;
    doc_count = 0;
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
  // total number of tokens in corpus
  int get_token_count() {return this->get_token_count();};
  CharacterVector get_vocab() {
    CharacterVector vocab_R(vocab.size());
    for(auto i:vocab)
      vocab_R[ i.second ] = i.first;
    return vocab_R;
  }

  // vocabulary
  unordered_map<string, uint32_t> vocab;

  // total number of documents in corpus
  int get_doc_count() { return doc_count; };

  void insert_document(const CharacterVector terms, uint32_t ngram_min, uint32_t ngram_max, const string ngram_delim = "_") {
    // map represents pair of (word_id, word_count)
    unordered_map<uint32_t, uint32_t> term_count_map;

    if(ngram_min == 1 && ngram_max == 1) {
      for (auto term : terms) {
        process_term_vocab (as<string>(term), term_count_map, this->vocab);
      }
    }
    // harder case - n-grams
    else {
      //lamda which defines how to process each ngram term
      std::function<void(string)> process_term_fun = [&](string x) {
        process_term_vocab (x, term_count_map, this->vocab);
      };
      ngram_generator(terms,
            process_term_fun,
            ngram_min, ngram_max, ngram_delim);
    }
    // add row - update sparse matrix values
    insert_dtm_doc(term_count_map);
  }

  void insert_document_batch(const ListOf<CharacterVector> docs_batch, int ngram_min, int ngram_max, const string ngram_delim = "_") {
    for (auto it:docs_batch)
      insert_document(it, ngram_min, ngram_max, ngram_delim);
    //Rprintf("token_count = %d\n", token_count);
  }
  SEXP get_dtm_dgT() {
    size_t ncol = this->vocab.size();

    vector<string> terms;
    terms.resize(ncol);

    for(auto it:vocab)
      terms[it.second] = it.first;

    int i = 0;
    NumericVector dtm_x(token_count);
    IntegerVector dtm_i(token_count);
    IntegerVector dtm_j(token_count);

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
      return get_dtm_dgT ();
    case 1:
      return get_dtm_lda_c();
    case 2:
      return get_dtm_minhash();
    default:
      return R_NilValue;
    }
  }
};
