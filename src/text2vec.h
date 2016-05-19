#include <Rcpp.h>
#include <string>
#include <vector>
#include <functional>
#include <unordered_map>
#include <RcppParallel.h>

using namespace std;
using namespace Rcpp;

// fast integer hashing
uint32_t fast_int_hash(uint32_t a);


NumericMatrix convert2Rmat(vector<vector<float> > &mat, size_t ncol);

void fill_mat_val(vector<vector<float> > &mat, size_t ncol, float val);

void fill_mat_rand(vector<vector<float> > &mat, size_t ncol, float runif_min, float runif_max);

void fill_vec_rand(vector<float>  &vec, float runif_min, float runif_max);

void fill_vec_val(vector<float>  &vec, float val);

void generate_ngrams(CharacterVector terms_raw,
                     const uint32_t ngram_min,
                     const uint32_t ngram_max,
                     RCPP_UNORDERED_SET<string> &stopwords,
                     // pass buffer by reference to avoid memory allocation
                     // on each iteration
                     vector<string> &terms_filtered_buffer,
                     vector<string> &ngrams,
                     const string ngram_delim);
