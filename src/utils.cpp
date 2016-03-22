#include <Rcpp.h>
#include "text2vec.h"
using namespace Rcpp;
using namespace std;

NumericMatrix convert2Rmat(vector<vector<double> > &mat, size_t ncol) {
  NumericMatrix res(mat.size(), ncol);
  for (size_t i = 0; i < mat.size(); i++)
    for (size_t j = 0; j < ncol; j++)
      res(i, j) = mat[i][j];
  return res;
}

// inline double dot_prod(vector<double> &x, vector<double> &y) {
//   double res = 0;
//   for(size_t i = 0; i < x.size(); i++) {
//     res += x[i] * y[i];
//   }
//   return res;
// }

void fill_mat_val(vector<vector<double> > &mat, size_t ncol, double val) {
  for (size_t i = 0; i < mat.size(); i++)
    for (size_t j = 0; j < ncol; j++)
      mat[i][j] = val;
}
void fill_mat_rand(vector<vector<double> > &mat, size_t ncol, double runif_min, double runif_max) {
  for (size_t i = 0; i < mat.size(); i++)
    for (size_t j = 0; j < ncol; j++)
      mat[i][j] = R::runif(runif_min, runif_max); //(double)rand() / (double)RAND_MAX - 0.5;
}

void fill_vec_rand(vector<double>  &vec, double runif_min, double runif_max) {
  for (size_t i = 0; i < vec.size(); i++)
    vec[i] = R::runif(runif_min, runif_max); //(double)rand() / (double)RAND_MAX - 0.5;
}

void fill_vec_val(vector<double>  &vec, double val) {
  for (size_t i = 0; i < vec.size(); i++)
    vec[i] = val;
}

vector<string> get_ngrams(const CharacterVector terms,
                          uint32_t ngram_min,
                          uint32_t ngram_max,
                          const string &ngram_delim,
                          const RCPP_UNORDERED_SET< string > &stopwords) {
  uint32_t original_len = terms.size();

  // filter out stopwords
  vector<string> terms_filtered;
  terms_filtered.reserve(original_len);
  string s;
  for(auto it:terms) {
    s = as<string>(it);
    if( stopwords.find(s) == stopwords.end())
      terms_filtered.push_back(s);
  }

  //special case for unigram to speed up a little bit
  if(ngram_min == 1 && ngram_max == 1) {
    return(terms_filtered);
  }

  uint32_t len = terms_filtered.size();
  // calculate res size
  size_t out_len = 0;

  uint32_t i1 = min(len, ngram_min);
  uint32_t i2 = min(len, ngram_max);

  for(size_t i = i1; i <= i2; i++)
    out_len += (len - i) + 1;

  vector< string> res;
  res.reserve(out_len);

  string k_gram, current_term;
  size_t n, k;
  // iterates through input vector by window of size = n_max and build n-grams
  // for terms ["a", "b", "c", "d"] and n_min = 1, n_max = 2
  // will build 1:3-grams in following order
  //"a"     "a_b"   "a_b_c" "b"     "b_c"   "b_c_d" "c"     "c_d"   "d"
  for(size_t j = 0; j < len; j ++ ) {
    n = 0;
    k = 0;
    while (n < ngram_max && (j + k) < len) {
      current_term = terms_filtered[j + k];

      if(stopwords.find(current_term) == stopwords.end()) {
        if( n == 0) {
          k_gram = current_term;
        }
        else
          k_gram = k_gram + ngram_delim + current_term;
        if(n >= ngram_min - 1) {
          res.push_back(k_gram);
        }
        n++;
      }
      k++;
    }
  }
  return res;
}

//' @export
//' @name ngrams
//' @title Generates ngrams
//' @description Generates ngrams character vector from tokens
//' @param terms input tokens
//' @param ngram_min min number of tokens in ngram
//' @param ngram_max max number of tokens in ngram
//' @param stopwords character vector of stopwords to filter them out
//' @param sep string separator between tokems
// [[Rcpp::export]]
CharacterVector ngrams(const CharacterVector terms,
                       uint32_t ngram_min,
                       uint32_t ngram_max,
                       const CharacterVector stopwords = CharacterVector(),
                       const String sep = "_") {
  RCPP_UNORDERED_SET<string> stopwords_set;
  for(auto it:stopwords)
    stopwords_set.insert(as<string>(it));
  return wrap(get_ngrams(terms, ngram_min, ngram_max, sep, stopwords_set));
}

// // for unordered_map < <uint32_t, uint32_t>, T >
// namespace std {
// template <>
// struct hash<std::pair<uint32_t, uint32_t>>
// {
//   inline uint64_t operator()(const std::pair<uint32_t, uint32_t>& k) const
//   {
//     //should produce no collisions
//     //http://stackoverflow.com/a/24693169/1069256
//     //return f << (CHAR_BIT * sizeof(size_t) / 2) | s;
//     //http://stackoverflow.com/questions/2768890/how-to-combine-two-32-bit-integers-into-one-64-bit-integer?lq=1
//     return (uint64_t) k.first << 32 | k.second;
//   }
// };
// }
//
// // list of dgTMatrix as input
// S4 add_triplet_matrix_list(ListOf<S4> lst) {
//   unordered_map<pair<uint32_t, uint32_t>, float> res;
//   S4 m = lst[0];
//   IntegerVector dims = m.slot("Dim");
//   List dimnames = m.slot("Dimnames");
//   size_t i;
//
//   IntegerVector I, J;
//   NumericVector X;
//   for(S4 it: lst) {
//     I = it.slot("i");
//     J = it.slot("j");
//     X = it.slot("x");
//     for( i = 0; i < I.size(); i++ ) {
//       res[make_pair(I[i], J[i])] += X[i];
//     }
//   }
//
//   size_t NNZ = res.size();
//
//   // result triplet sparse matrix
//   S4 triplet_matrix("dgTMatrix");
//
//   // index vectors
//   IntegerVector I_RES(NNZ), J_RES(NNZ);
//   // value vector
//   NumericVector X_RES(NNZ);
//
//   i = 0;
//   for(auto it : res) {
//     I_RES[i] = it.first.first;
//     J_RES[i] = it.first.second;
//     X_RES[i] = it.second;
//     i++;
//   }
//   // construct matrix
//   triplet_matrix.slot("i") = I_RES;
//   triplet_matrix.slot("j") = J_RES;
//   triplet_matrix.slot("x") = X_RES;
//   // set dimensions
//   triplet_matrix.slot("Dim") = dims;
//   // set dimension names
//   triplet_matrix.slot("Dimnames") = dimnames;
//   return triplet_matrix;
// }
