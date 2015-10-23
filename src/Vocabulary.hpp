#include "text2vec.h"

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

class Vocabulary {
public:
  Vocabulary(): sentence_count(0), token_count(0) {}

// insert single sentence (vector<string>) into vocabulary
  void insert_sentence(const CharacterVector sentence) {
    // increase number of handled sentences
    this->sentence_count++;
    // clear set of terms for this sentence/document
    temp_document_word_set.clear();

    string s;
    //typename unordered_map <  string, pair<int, int> > :: iterator term_iterator;
    typename unordered_map <  string, TermStat > :: iterator term_iterator;
    for(int i = 0; i < sentence.size(); i++) {
      token_count++;
       s = sentence[i];
       // add term to document bag-of-words
       temp_document_word_set.insert(s);

       term_iterator = this->full_vocab.find(s);
       if(term_iterator == this->full_vocab.end())
         this->full_vocab.insert(make_pair(s, TermStat( this->full_vocab.size()) ) );
       else {
         term_iterator->second.term_global_count++;
       }
    }

    for ( auto it: temp_document_word_set) {
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
  unordered_map<string, TermStat> full_vocab;
  //unordered_map< string, int> vocab;
  uint32_t sentence_count;
  uint32_t token_count;
  unordered_set<string> temp_document_word_set;
};
