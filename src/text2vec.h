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


NumericMatrix convert2Rmat(vector<vector<double> > &mat, size_t ncol);

void fill_mat_val(vector<vector<double> > &mat, size_t ncol, double val);

void fill_mat_rand(vector<vector<double> > &mat, size_t ncol, double runif_min, double runif_max);

void fill_vec_rand(vector<double>  &vec, double runif_min, double runif_max);

void fill_vec_val(vector<double>  &vec, double val);

void generate_ngrams(CharacterVector terms_raw,
                     const uint32_t ngram_min,
                     const uint32_t ngram_max,
                     RCPP_UNORDERED_SET<string> &stopwords,
                     // pass buffer by reference to avoid memory allocation
                     // on each iteration
                     vector<string> &terms_filtered_buffer,
                     vector<string> &ngrams,
                     const string ngram_delim);
