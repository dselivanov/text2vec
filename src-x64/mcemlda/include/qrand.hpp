#pragma once

#include <cstdint>
#include <cstdlib>

namespace qlib {

class XOR64STAR {
	uint64_t s_;
public:
	explicit XOR64STAR(uint64_t s = 0xDEADBEEF) { seed(s); }

	void seed(uint64_t s) { s_ = s; }

	uint64_t sample() {
		s_ ^= s_ >> 12; // a
		s_ ^= s_ << 25; // b
		s_ ^= s_ >> 27; // c
		return s_ * 2685821657736338717ull;
	}

	double drand() {
		return static_cast<double>(sample())/(~0ull);
	}
};

class XOR128PLUS {
	uint64_t s_[2];
public:
	explicit XOR128PLUS(uint64_t s0 = 0xDEADBEEF, uint64_t s1 = 0xCAFEBABE) { seed(s0,s1); }

	void seed(uint64_t seed0, uint64_t seed1 = 0xCAFEBABE) {
		s_[0] = seed0;
		s_[1] = seed1;
	}

	uint64_t sample() {
		uint64_t x = s_[0];
		uint64_t const y = s_[1];
		s_[0] = y;
		x ^= x << 23; // a
		s_[1] = x ^ y ^ (x >> 17) ^ (y >> 26); // b, c
		return s_[1] + y;
	}

	double drand() {
		return static_cast<double>(sample())/(~0ull);
	}

};

class LCG64 {
	uint64_t s_;
public:
	explicit LCG64(uint64_t s = 0xDEADBEEF) { seed(s); }

	void seed(uint64_t s) { s_ = s; }

	uint64_t sample() {
		s_ = 18145460002477866997ull*s_ + 1;
		return s_;
	}

	double drand() {
		return static_cast<double>(sample())/(~0ull);
	}
};

template<class RNG, class T>
void shuffle(RNG& rng, T* bgn, const T* end) {
	for(auto it = bgn; it!=end; it++)
		std::swap(it[0], it[rng.sample() % (end-it)]);
}

}; // namespace qlib

