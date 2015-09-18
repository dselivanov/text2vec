#include <Rcpp.h>
#include <string>
#include <vector>
#include <unordered_map>

using namespace Rcpp;
using namespace std;

void process_term_dict (const string &term,
                               unordered_map<uint32_t, int> &term_count_map,
                               unordered_map<string, int> &dict,
                               vector<string> &terms_vec);

// implements hashing trick
void process_term_hash (const string &term,
                        unordered_map<uint32_t, int> &term_count_map,
                        uint32_t buckets_size,
                        std::function<uint32_t(string)> hash_fun);
