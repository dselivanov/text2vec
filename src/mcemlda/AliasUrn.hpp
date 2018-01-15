#pragma once

#include <vector>
#include <stdexcept>
#include "include/qrand.hpp"

using std::vector;

// See https://goo.gl/mLlOl0
template<typename RNG>
class AliasUrn {
public:
	struct AliasEntry {
	public:
		unsigned lo;
		unsigned hi;
		double p;
		AliasEntry(unsigned l, unsigned h, double pp) : lo(l), hi(h), p(pp) {}
	};

	vector<AliasEntry> v_;
	RNG& rng_;

public:
	AliasUrn(RNG& rng) : rng_(rng) {}

	unsigned sample() const {
		// Todo: check 0==v_.size()
		const auto& entry = v_[rng_.sample() % v_.size()];
		return rng_.drand() < entry.p ? entry.lo : entry.hi;
	}

	template<bool need_normalize = true>
	void setup(const vector<double>& prob) {
		v_.clear();
		if(prob.size()==0)
			return;
		double s = 1;
		if(need_normalize) {
			s = 0;
			for(const auto& each: prob)
				s += each;
		}
		vector<std::pair<unsigned,double>> small;
		vector<std::pair<unsigned,double>> large;
		const size_t K = prob.size();
		double thres = 1.0f/K;
		// Split into small and large
		for(size_t i=0; i < prob.size(); i++) {
			if(prob[i]/s >= thres)
				large.emplace_back(i,prob[i]/s);
			else
				small.emplace_back(i,prob[i]/s);
		}
		// Create AliasEntries
		while(large.size() > 0 and small.size() > 0) {
			auto L = large.back();
			auto S = small.back();
			large.pop_back();
			small.pop_back();
			double Lprob;
			v_.emplace_back(S.first, L.first, K*S.second);
			Lprob = L.second - (thres - S.second);
			if(Lprob >= thres)
				large.emplace_back(L.first, Lprob);
			else
				small.emplace_back(L.first, Lprob);
		}
		for(const auto& each : large) // each.second is expected to be around 1.0/K
			v_.emplace_back(each.first, each.first, 1.0);
		for(const auto& each : small) // each.second is expected to be around 1.0/K
			v_.emplace_back(each.first, each.first, 1.0);
		//check(prob);
	}

	void print() const {
		for(const auto& each : v_)
			printf("drand() < %lf ? %d : %d\n", each.p, each.lo, each.hi);
	}

	void check(const vector<double>& prob) const {
		vector<double> hist;
		const auto K = prob.size();
		hist.resize(K);
		for(const auto& each : v_) {
			hist[each.lo] += each.p;
			hist[each.hi] += (1-each.p);
		}
		for(int i=0; i<K; i++)
			hist[i] /= prob[i];
		double scale = hist[0];
		for(int i=0; i<K; i++)
			hist[i] /= scale;
		bool wrong = false;
		for(int i=0; i<K; i++)
			if(hist[i] < 1-1e-5 or 1+1e-5 < hist[i]) {
				printf("hist[%d]: %lf\n",i,hist[i]);
				wrong = true;
			}
		if(wrong)
			throw std::runtime_error("AliasUrn error!\n");
	}

};

