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
