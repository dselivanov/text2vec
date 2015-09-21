#include <Rcpp.h>
#include <string>
#include <vector>
#include <functional>
#include <unordered_map>
using namespace Rcpp;
using namespace std;

// borrowed from FeatureHashing package
// https://github.com/wush978/FeatureHashing/blob/ea7cc034b0dbdfa6fc742fcebaa7dc4025f5364e/src/digest.c
// seeds for hashing trick
const uint32_t MURMURHASH3_HASH_SEED = 3120602769LL;
const uint32_t MURMURHASH3_SIGN_SEED = 79193439LL;

// feature hash
uint32_t murmurhash3_hash (const string &str);
// feature sign hash
int      murmurhash3_sign (const string &str);

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
void ngram(const vector<string> &terms,
                 // function to process obtained ngram
                 std::function<void(string)> process_ngram,
                 int n_min, int n_max,
                 // delimiter for terms concatenation
                 const string delim);
