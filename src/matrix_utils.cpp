// Copyright (C) 2015 - 2016  Dmitriy Selivanov
// This file is part of text2vec
//
// text2vec is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// text2vec is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with text2vec.  If not, see <http://www.gnu.org/licenses/>.

#include "text2vec.h"
using namespace Rcpp;
using namespace std;
// [[Rcpp::export]]
NumericVector colMins(const NumericMatrix x) {
  NumericVector res(x.ncol());
  for(uint32_t i = 0; i < x.ncol(); i++) {
    res[i] = Rcpp::min(x(_, i));
  }
  return res;
}
// [[Rcpp::export]]
NumericVector colMaxs(const NumericMatrix x) {
  NumericVector res(x.ncol());
  for(uint32_t i = 0; i < x.ncol(); i++) {
    res[i] = Rcpp::max(x(_, i));
  }
  return res;
}
// [[Rcpp::export]]
NumericVector rowMins(const NumericMatrix x) {
  NumericVector res(x.nrow());
  for(uint32_t i = 0; i < x.nrow(); i++) {
    res[i] = Rcpp::min(x(i, _));
  }
  return res;
}
// [[Rcpp::export]]
NumericVector rowMaxs(const NumericMatrix x) {
  NumericVector res(x.nrow());
  for(uint32_t i = 0; i < x.nrow(); i++) {
    res[i] = Rcpp::max(x(i, _));
  }
  return res;
}

// [[Rcpp::export]]
NumericMatrix euclidean_dist(const NumericMatrix x, const NumericMatrix y) {
  if(x.nrow() != y.nrow())
    stop("Matrices should have same number of rows");
  NumericMatrix res(x.ncol(), y.ncol());
  double tmp = 0.0;
  double diff;
  uint32_t inner_dim = x.nrow();
  for(uint32_t i = 0; i < x.ncol(); i++) {
    for(uint32_t j = 0; j < y.ncol(); j++) {
      tmp = 0.0;
      for(uint32_t k = 0; k < inner_dim; k++) {
        diff = x(k, i) - y(k, j);
        tmp += diff * diff;
      }
      res(i, j) = sqrt(tmp);
    }
  }
  return res;
}
