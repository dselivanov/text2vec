#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <unordered_map>
using std::vector;
using std::unordered_map;

// Row-major
template<typename T>
class DenseMat {
public:
	typedef T value_type;
protected:
	vector<T> val_;
	size_t nrow_;
	size_t ncol_;
public:
	// Entry access
	T& at(size_t r, size_t c) { return val_[ncol_*r + c]; }
	const T& at(size_t r, size_t c) const { return val_[ncol_*r + c]; }

	// Sizes
	size_t nrow() const { return nrow_; }
	size_t ncol() const { return ncol_; }

	// Modifiers
	void resize(size_t nr, size_t nc) {
		val_.resize(nr*nc);
		nrow_ = nr;
		ncol_ = nc;
	}
	void clear() {
		for(auto&& each: val_)
			each = T();
	}

};

template<typename T>
class DenseRowMat {
public:
	typedef T value_type;
protected:
	vector<unordered_map<size_t,T>> vmap_;
	size_t nrow_;
	size_t ncol_;
public:
	// Entry access
	T& at(size_t r, size_t c) { return vmap_[r][c]; }
	const T& at(size_t r, size_t c) const { return vmap_[r].find(c)->second; }

	// Sizes
	size_t nrow() const { return nrow_; }
	size_t ncol() const { return ncol_; }

	// Modifiers
	void resize(size_t nr, size_t nc) {
		vmap_.resize(nr);
		for(auto&& m : vmap_)
			m.reserve(nc);
		nrow_ = nr;
		ncol_ = nc;
	}
	void clear() {
		for(auto&& m : vmap_)
			m.clear();
	}

};

