#include <Rcpp.h>
#include <string>
#include <vector>
#include <functional>
#include <unordered_map>
#include <RcppParallel.h>

using namespace std;

void process_term_dict (const string &term,
                               unordered_map<uint32_t, int> &term_count_map,
                               unordered_map<string, int> &dict,
                               vector<string> &terms_vec);

// implements hashing trick
void process_term_hash (const string &term,
                        unordered_map<uint32_t, int> &term_count_map,
                        uint32_t buckets_size,
                        int signned_hash);

// n-gram generator
vector<string> get_ngrams(const Rcpp::CharacterVector terms,
           uint32_t ngram_min, uint32_t ngram_max,
           // delimiter for terms concatenation
           const string ngram_delim);
