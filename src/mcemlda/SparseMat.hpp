#pragma once

#include <vector>
#include <iostream>
#include <functional>

using std::vector;

template<typename T,
	typename IndexType = size_t,
	typename RowIndexType = size_t,
	typename ColIndexType = size_t>
class SparseMat {
public:
	typedef T value_type;
	typedef IndexType index_t;
	typedef RowIndexType row_index_t;
	typedef ColIndexType col_index_t;
public:
	vector<T> val_; // length == nnz()
	// CSR
	vector<index_t> csr_index_; // length == nrow()+1
	vector<col_index_t> csr_col_index_; // length == nnz()
	// Indexed CSC
	vector<index_t> csc_index_; // length == ncol()+1
	vector<row_index_t> csc_row_index_; // length == nnz()
	vector<index_t> csc_val_index_; // length == nnz()
public:
	// Sizes
	ssize_t nrow() const { return static_cast<ssize_t>(csr_index_.size())-1; }
	ssize_t ncol() const { return static_cast<ssize_t>(csc_index_.size())-1; }
  // desired dimensions - in case empty last columns or rows it will
  // be different from corpus.nrow(), corpus.ncol()
  ssize_t n_col_expected, n_row_expected;

	size_t nnz() const { return val_.size(); }

	// Const reference
	// const T& at(row_index_t r, col_index_t c) cosnst {
	// 	static const T const_obj = T();
	// 	const index_t bgn = csr_index_[r];
	// 	const index_t end = csr_index_[r+1];
	// 	for(auto i=bgn; i<end; i++) {
	// 		if(csr_col_index_[i] == c)
	// 			return val_[i];
	// 	}
	// 	return const_obj;
	// }

	/**
	 * Append entries in CSR order
	 * Return true iff successfully added
	 */
	template<class... Args>
	bool append(row_index_t r, col_index_t c, Args... args) {
		if(csr_index_.size() == 0)
			csr_index_.push_back(0);
		if(r+2 > csr_index_.size())
			csr_index_.resize(r+2, csr_index_[csr_index_.size()-1]);
		if(r+2 != csr_index_.size())
			return false;
		csr_index_[r+1]++; // last element in csr_index_
		val_.emplace_back(std::forward<Args>(args)...);
		csr_col_index_.emplace_back(c);
		return true;
	}

	/**
	 * F(T& value, row_index, col_index)
	 */
	template<bool use_csc = false>
	void apply(std::function<void(T&,row_index_t,col_index_t)> f) {
		if(not use_csc) {
			for(auto r=0; r<nrow(); r++) {
				const index_t bgn = csr_index_[r];
				const index_t end = csr_index_[r+1];
				for(index_t i=bgn; i<end; i++) {
					const col_index_t c = csr_col_index_[i];
					f(val_[i], r, c);
				}
			}
		} else {
			for(auto c=0; c<ncol(); c++) {
				const index_t bgn = csc_index_[c];
				const index_t end = csc_index_[c+1];
				for(index_t i=bgn; i<end; i++) {
					const row_index_t r = csc_row_index_[i];
					f(val_[csc_val_index_[i]], r, c);
				}
			}
		}
	}

	void print_size() const {
		std::cout << "SparseMat: " << nrow() << "x" << ncol() << " nnz: " << nnz() << std::endl;
	}

	// Based on csr_index_ and csr_col_index_
	// Update csc_index_ and csc_row_index_
	void build_CSC_from_CSR(int n_row, int n_col) {
	  // desired dimensions - in case empty last columns or rows it will
	  // be different from corpus.nrow(), corpus.ncol()
	  this->n_col_expected = n_col;
	  this->n_row_expected = n_row;

		// Go through csr_col_index twice.
		// See http://crd-legacy.lbl.gov/~yunhe/cs267/final/source/utils/convert/matrix_io.c
		csc_index_.clear();
		csc_row_index_.clear();
		csc_val_index_.clear();
		// Count num of entries in each col
		for(const auto& c : csr_col_index_) {
			if(c+1 >= csc_index_.size())
				csc_index_.resize(c+2);
			csc_index_[c+1] ++;
		}
		if(csc_index_.size() == 0)
			return;
		// Sum up
		for(size_t c=0; c+1<csc_index_.size(); c++)
			csc_index_[c+1] += csc_index_[c];
		auto nz = csr_col_index_.size();
		csc_row_index_.resize(nz);
		csc_val_index_.resize(nz);
		// Fill csc_row_index_ and csc_val_index_
		for(auto r=0; r<nrow(); r++) {
			const index_t bgn = csr_index_[r];
			const index_t end = csr_index_[r+1];
			for(index_t i=bgn; i<end; i++) {
				const col_index_t c = csr_col_index_[i];
				csc_val_index_[csc_index_[c]] = i;
				csc_row_index_[csc_index_[c]] = r;
				csc_index_[c]++;
			}
		}
		// shift back
		for(int c=csc_index_.size()-2; c >= 0; c--)
			csc_index_[c+1] = csc_index_[c];
		csc_index_[0] = 0;
	}

};

