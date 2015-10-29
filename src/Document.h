#include "text2vec.h"
using namespace std;
// Document is map! (term_id -> term_count)
// but we represent it by two corresponding vectors
// this done for simplicity in further steps
// also we strore some additional metadata - see comments in code
class Document {
public:
  //contructor
  Document(const unordered_map<uint32_t, uint32_t> &doc_map, int doc_num) {
    doc_len = doc_map.size();

    vector<uint32_t> term(doc_len);

    vector<int> cnt(doc_len);
    int i = 0;
    for(auto it:doc_map) {
      term[i] = it.first;
      cnt[i] = it.second;
      i++;
    }
    term_ids = term;
    term_counts = cnt;
    doc_id = doc_num;
    // Rprintf("doc %d inserted, len = %d, first: %d->%d \n", doc_num, doc_len, doc_map.begin()->first, doc_map.begin()->second);
  };
  //global term ids (shared across Coprpus)
  vector<uint32_t> term_ids;
  // counts for each uniqe term
  vector<int> term_counts;
  // document length (number of unique terms)
  int doc_len;
  // document id
  int doc_id;
};
