#include "text2vec.h"
using namespace Rcpp;
using namespace std;

class TermStat {
public:
  TermStat() {};
  TermStat(uint32_t term_id):
  term_id(term_id), term_global_count(1),
  document_term_count(0) {};

  uint32_t term_id;
  // term count in all corpus
  uint32_t term_global_count;
  // number of documents, which contain this term
  uint32_t document_term_count;
};

class Vocabulary {
public:
  Vocabulary();

  Vocabulary(uint32_t ngram_min,
             uint32_t ngram_max,
             const CharacterVector stopwords_R):
  ngram_min(ngram_min), ngram_max(ngram_max),
  document_count(0), token_count(0) {
    ngram_delim = "_";
    for(auto it:stopwords_R)
      stopwords.insert(as<string>(it));
  };

//   void init_vocabulary(const CharacterVector vocab_R,
//              uint32_t ngram_min,
//              uint32_t ngram_max) {
//     this->ngram_min = ngram_min;
//     this->ngram_max = ngram_max;
//     this->ngram_delim = "_";
//     size_t vocab_size = vocab_R.size();
//     size_t i = 0;
//     // we know vocab size, so lets reserve buckets this number
//     // and if we will lucky no rehash will needed
//     this->vocab.reserve(vocab_size);
//     //convert R vocab represenation to C++ represenation
//     // also fill terms in right order
//     for (auto term : vocab_R) {
//       //grow vocabulary
//       this->vocab.insert(make_pair(as< string >(term), i));
//       i++;
//     }
//   };

  void insert_terms (vector< string> &terms) {
    typename unordered_map < string, uint32_t > :: iterator term_iterator;
    int term_id;
    for (auto it:terms) {
      this->temp_document_word_set.insert(it);

      term_iterator = this->vocab.find(it);

      if(term_iterator == this->vocab.end()) {
        term_id = this->vocab.size();
        // insert term into vocabulary
        this->vocab.insert(make_pair(it, term_id ));
        vocab_statistics.push_back(TermStat( term_id ) );
      }
      else {
        vocab_statistics[term_iterator->second].term_global_count++;
      }
      this->token_count++;
    }
  }

  void insert_document(const CharacterVector doc) {
    this->document_count++;
    this->temp_document_word_set.clear();
    generate_ngrams(doc, this->ngram_min, this->ngram_max,
                    this->stopwords,
                    this->terms_filtered_buffer,
                    this->ngrams_buffer,
                    this->ngram_delim);
    insert_terms(this->ngrams_buffer);

    typename unordered_map < string, uint32_t > :: iterator term_iterator;
    for ( auto it: this->temp_document_word_set) {
      term_iterator = vocab.find(it);
      if(term_iterator != vocab.end())
        this->vocab_statistics[term_iterator->second].document_term_count++;
    }
  }

  void insert_document_batch(const ListOf<const CharacterVector> document_batch) {
   for(auto s:document_batch)
     insert_document(s);
  }

  int get_document_count() {return(this->document_count);};

  DataFrame get_vocab_statistics() {
    size_t N = vocab.size();
    size_t i = 0;
    CharacterVector terms(N);
    IntegerVector term_counts(N);
    IntegerVector doc_counts(N);
    NumericVector doc_prop(N);
    for(auto it:vocab) {
      terms[i] = it.first;
      term_counts[i] = vocab_statistics[it.second].term_global_count;
      doc_counts[i] = vocab_statistics[it.second].document_term_count;
      i++;
    }
    return DataFrame::create(_["terms"] = terms,
                        _["terms_counts"] = term_counts,
                        _["doc_counts"] = doc_counts,
                        _["stringsAsFactors"] = false );
  }
  void increase_token_count() {token_count++;};

private:
  vector< TermStat > vocab_statistics;
  unordered_map< string, uint32_t > vocab;

  uint32_t ngram_min;
  uint32_t ngram_max;
  string ngram_delim;

  int document_count;
  uint32_t token_count;
  // used for count word-document statistsics in vocab_statistics.
  // keep words set for document which is currently we processing
  RCPP_UNORDERED_SET< string > temp_document_word_set;
  RCPP_UNORDERED_SET< string > stopwords;
  // buffer for filtering out stopwords
  // this is to avoid memory re-allocation
  vector<string> terms_filtered_buffer;
  // buffer for ngrans
  // this is to avoid memory re-allocation
  vector< string> ngrams_buffer;

};
