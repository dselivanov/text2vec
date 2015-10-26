#include "text2vec.h"
using namespace Rcpp;
using namespace std;

class TermStat {
public:
  TermStat(uint32_t term_id):
  term_id(term_id), term_global_count(1), document_term_count(0) {
    // document_term_count(make_pair(document_id, ))
  };
  uint32_t term_id;
  // term count in all corpus
  uint32_t term_global_count;
  // number of documents, which contain this term
  double document_term_count;
  // share of documents, which contain this term
  //double document_share;
};


inline void term_handler (const string &term,
                        unordered_map< string, TermStat> &vocab,
                        unordered_set< string> &temp_doc_set) {
  typename unordered_map < string, TermStat > :: iterator term_iterator;
  temp_doc_set.insert(term);
  term_iterator = vocab.find(term);
  int term_id;
  // new unobserved term
  if(term_iterator == vocab.end()) {
    // here we will precess term and grow dictionary
    // get new term id
    // we use incremented ids, so set new id to dict.size()
    term_id = vocab.size();
    // insert term into dictionary
    vocab.insert(make_pair(term, TermStat( vocab.size()) ));
  }
  // dictionary already contains term => get index
  else {
    term_iterator->second.term_global_count++;
  }
}


class Vocabulary {
public:

  Vocabulary(uint32_t ngram_min,
             uint32_t ngram_max,
             const string ngram_delim = "_"):
  ngram_min(ngram_min), ngram_max(ngram_max),
  token_count(0), sentence_count(0),
  ngram_delim(ngram_delim) {
    this->insert_term = [&](const string &term) {
      this->token_count++;
      term_handler(term, this->full_vocab, this->temp_document_word_set);
    };
  }

  void insert_sentence(const CharacterVector terms) {

    this->temp_document_word_set.clear();

    ngram_generator(terms, this->insert_term, ngram_min, ngram_max, ngram_delim);

    typename unordered_map < string, TermStat > :: iterator term_iterator;

    for ( auto it: this->temp_document_word_set) {
      term_iterator = full_vocab.find(it);
      if(term_iterator != full_vocab.end())
        term_iterator->second.document_term_count++;
    }
  }

  void insert_sentence_batch(const ListOf<const CharacterVector> sentence_batch) {
   for(auto s:sentence_batch) {
     insert_sentence(s);
   }
  }

  List vocab_stat() {
    size_t N = full_vocab.size();
    size_t i = 0;
    CharacterVector terms(N);
    IntegerVector term_ids(N);
    IntegerVector term_counts(N);
    IntegerVector doc_counts(N);

    for(auto it:full_vocab) {
      terms[i] = it.first;
      term_ids[i] = it.second.term_id;
      term_counts[i] = it.second.term_global_count;
      doc_counts[i] = it.second.document_term_count;
      i++;
    }
    return List::create(_["term"] = terms,
                        _["term_id"] = term_ids,
                        _["term_count"] = term_counts,
                        _["doc_count"] = doc_counts);
  }

  // truncate vocabulary - delete uncommon terms
  //  void truncate_vocab(int n_min ) {
  //    int i = 0;
  //    // create only
  //    if(vocab.empty())
  //      for (auto it:this->full_vocab) {
  //        if(it.second.second >= n_min ) {
  //          vocab.insert(make_pair(it.first, i));
  //          i++;
  //        }
  //      }
  //      else Rprintf("warning : vocab already exists - do nothing\n");
  //  }
private:
  unordered_map< string, TermStat > full_vocab;

  uint32_t ngram_min;
  uint32_t ngram_max;
  const string ngram_delim;

  uint32_t sentence_count;
  uint32_t token_count;
  unordered_set< string > temp_document_word_set;

  std::function<void(const string)> insert_term;

};
