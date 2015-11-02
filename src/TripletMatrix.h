#include <Rcpp.h>
#include <string>
#include <vector>
#include <unordered_map>
#include "text2vec.h"

using namespace std;
using namespace Rcpp;

// for unordered_map < <uint32_t, uint32_t>, T >
namespace std {
template <>
struct hash<std::pair<uint32_t, uint32_t>>
{
  inline size_t operator()(const std::pair<uint32_t, uint32_t>& k) const
  {
    return fast_int_hash(k.first) + fast_int_hash(k.second);
  }
};
}


template<typename T>
NumericMatrix convert2Rmat(vector<vector<T> > &mat, size_t ncol) {
  NumericMatrix res(mat.size(), ncol);
  for (size_t i = 0; i < mat.size(); i++)
    for (size_t j = 0; j < ncol; j++)
      res(i, j) = mat[i][j];
  return res;
}

template<typename T>
class TripletMatrix {
public:
  // constructor for sparse matrix
  TripletMatrix():
    sparse(1), nrow(0), ncol(0) {};

  // constructor for dense matrix
  TripletMatrix(uint32_t nrow, uint32_t ncol):
    sparse(0), nrow(nrow), ncol(ncol) {
    dense_container.resize(nrow);
    for(auto it:dense_container)
      // resize fills value with 0
      it.resize(ncol);
  };

  // constructor for dense matrix with unknown number of rows
  TripletMatrix(uint32_t ncol):
    sparse(0), nrow(0), ncol(ncol) {};

  // properties
  inline int is_sparse() {return this->sparse;};
  inline uint32_t nrows() {return this->nrow;};
  inline uint32_t ncols() {return this->ncol;};
  inline size_t size() {
    if(this -> sparse)
      return(this->sparse_container.size());
    else
      return this->nnz;
  }

  // grow matrix
  inline void add_row() {
    this->dense_container.push_back(vector<T>(ncol));
    nrow++;
  }
  // add or increment elements
  void add(uint32_t i, uint32_t j, T increment) {
    if ( this->sparse) {
      this->nrow = max(i + 1, this->nrow);
      this->ncol = max(j + 1, this->ncol);
      // simply add our increment
      this->sparse_container[make_pair(i, j)] += increment;
    }
    else {
      // out matrix has not enougth rows to insert element with row index i =>
      // grow matrix, add rows.
      while(this->nrow < i)
        add_row();
      // inserting new element for given pair of indices?
      if(dense_container[i][j] == 0)
        nnz++;
      // add our increment
      this->dense_container[i][j] += increment;
    }
  };

  NumericMatrix get_dense_matrix(vector< string>  &rownames, vector< string>  &colnames) {
    return convert2Rmat(this->dense_container, this->ncol);
  }

  S4 get_sparse_triplet_matrix(vector< string>  &rownames, vector< string>  &colnames) {
    // non-zero values count
    size_t NNZ = this->size();

    // result triplet sparse matrix
    S4 triplet_matrix("dgTMatrix");

    // index vectors
    IntegerVector I(NNZ), J(NNZ);
    // value vector
    NumericVector X(NNZ);

    uint32_t n = 0;
    double x;
    for(auto it : sparse_container) {
      // fill first half of our symmetric cooccurence matrix
      I[n] = it.first.first;
      J[n] = it.first.second;
      X[n] = it.second;
      n++;
    }
    // construct matrix
    triplet_matrix.slot("i") = I;
    triplet_matrix.slot("j") = J;
    triplet_matrix.slot("x") = X;
    // set dimensions
    triplet_matrix.slot("Dim") = IntegerVector::create(max(nrow, (uint32_t)rownames.size()), max(ncol, (uint32_t)colnames.size()));
    // set dimension names
    triplet_matrix.slot("Dimnames") = List::create(rownames, colnames);
    return triplet_matrix;
  }

private:
  // flag whether matrix is sparse
  int sparse;
  // dimensionality of matrix
  uint32_t nrow;
  uint32_t ncol;
  // number of non-zero elements in matrix
  size_t nnz;
  // container for dense matrix
  vector<vector<T>> dense_container;
  // container for sparse matrix in triplet form
  unordered_map< pair<uint32_t, uint32_t>, T >  sparse_container;
};
