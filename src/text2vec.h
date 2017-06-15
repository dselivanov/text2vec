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
#pragma once
#ifndef TEXT2VEC_H
#define TEXT2VEC_H

#include <Rcpp.h>
#include <stdio.h>
#include <time.h>
#include <string>
#include <sstream>
#include <vector>
#include <functional>
#include <unordered_map>
#include <unordered_set>
#include <RcppParallel.h>
#include <sparsepp.h>

using namespace std;
using namespace Rcpp;

// fast integer hashing
uint32_t fast_int_hash(uint32_t a);

NumericMatrix convert2Rmat(vector<vector<float> > &mat, size_t ncol);

void fill_mat_val(vector<vector<float> > &mat, size_t ncol, float val);

void fill_mat_rand(vector<vector<float> > &mat, size_t ncol, float runif_min, float runif_max);

void fill_vec_rand(vector<float>  &vec, float runif_min, float runif_max);

void fill_vec_val(vector<float>  &vec, float val);

vector<string> generate_ngrams(const std::vector< std::string> &terms,
                               const uint32_t ngram_min,
                               const uint32_t ngram_max,
                               const string ngram_delim);
std::vector<std::string> charvec2stdvec(CharacterVector terms_raw, unordered_set<string> &stopwords);
const std::string currentDateTime();
std::vector<std::string> char_tokenizer(const std::string &s, unordered_set<string> &stopwords, char delim = ' ');
#endif /* TEXT2VEC_H */
