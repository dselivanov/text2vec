#pragma once

#include <vector>
#include <algorithm>

template<typename T, class Comp>
class Heap {
	std::vector<T> v_;
	size_t max_size_;
	Comp comp_;
	
public:
	typedef typename std::vector<T>::iterator iterator;

	Heap() : max_size_(~0lu) {}
	Heap(size_t s) : max_size_(s) {}

	size_t max_size() const { return max_size_; }
	void set_max_size(size_t s) { max_size_ = s; }

	template<typename U>
	bool push(U&& t) {
		if(v_.size() >= max_size_ and comp_(v_.front(),t))
			return false;
		v_.push_back(std::forward<U>(t));
		std::push_heap<iterator,Comp>(v_.begin(), v_.end(), comp_);
		if(v_.size() > max_size_) {
			std::pop_heap<iterator,Comp>(v_.begin(), v_.end(), comp_);
			v_.pop_back();
		}
		return true;
	}

	T pop() {
		std::pop_heap<iterator,Comp>(v_.begin(), v_.end(), comp_);
		T t = v_.back();
		v_.pop_back();
		return t;
	}

	void sort() {
		std::sort_heap<iterator,Comp>(v_.begin(), v_.end(), comp_);
	}

	void clear() { v_.clear(); }

	std::vector<T>& get() { return v_; }
	const std::vector<T>& get() const { return v_; }
};

template<typename T>
class MinHeap : public Heap<T,std::less<T>> {};

template<typename T>
class MaxHeap : public Heap<T,std::greater<T>> {};

