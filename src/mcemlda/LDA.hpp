#pragma once

#include "DenseMat.hpp"
#include "SparseMat.hpp"
#include "AliasUrn.hpp"
#include "Heap.hpp"
#include <vector>
#include <algorithm>
#include <string>
#include <assert.h>
#include <math.h>

using std::vector;

class LDA {
public:
	// Types
#if 1
	typedef uint16_t topic_index_t;
	typedef uint32_t doc_index_t;
	typedef uint32_t word_index_t;
	typedef uint64_t token_index_t;
#else
	typedef int topic_index_t;
	typedef int doc_index_t;
	typedef int word_index_t;
	typedef int token_index_t;
#endif

	struct Z {
	public:
		topic_index_t old_z;
		topic_index_t new_z;
		Z(topic_index_t o, topic_index_t n) : old_z(o), new_z(n) {}
	};

	// Parameters
	topic_index_t n_topic;
	float alpha, beta, alpha_bar, beta_bar;
	// Objects
	// These could be DenseRowMat. See DenseMat.hpp for details.
	DenseMat<int> C_doc;
	DenseMat<int> C_word;
	vector<int> C_all; // C_all[k] = #token in topic k
	vector<int> C_local; // C_all[k] = #token in topic k
	vector<int> C_local_diff; // C_local_diff[k] = #token in topic k
	SparseMat<Z,token_index_t,doc_index_t,word_index_t> corpus;

	qlib::XOR128PLUS rng;

	topic_index_t get_n_topic() const { return n_topic; }
	void set_n_topic(topic_index_t n) {
		n_topic = n;
		alpha_bar = n*alpha;
		beta_bar = n*beta;
	}
	float get_alpha() const { return alpha; }
	void set_alpha(float a) {
		alpha = a;
		alpha_bar = n_topic*a;
	}
	float get_beta() const { return beta; }
	void set_beta(float b) {
		beta = b;
		beta_bar = n_topic*b;
	}

	// Methods
	void init() {
	  C_doc.resize(corpus.n_row_expected, n_topic);
	  C_word.resize(corpus.n_col_expected, n_topic);
		C_all.resize(n_topic);
		C_local_diff.resize(n_topic);
		// Fill z randomly
		corpus.apply([&](Z& z, doc_index_t d, word_index_t w) {
			z.old_z = rng.sample() % n_topic;
			z.new_z = rng.sample() % n_topic;
		});
		// Update C_doc & C_all
		C_doc.clear();
		corpus.apply([&](Z& z, doc_index_t d, word_index_t w) {
			C_doc.at(d, z.old_z) ++;
			C_all[z.old_z] ++;
		});
		// Update C_word
		C_word.clear();
		corpus.apply<true>([&](Z& z, doc_index_t d, word_index_t w) {
			C_word.at(w, z.old_z) ++;
		});
	}

	// Accept and update by word
	void sample_by_word(bool update_topics = true) {
		const auto n_word = corpus.ncol();
		for(auto w = 0; w < n_word; w++) {
			const token_index_t bgn = corpus.csc_index_[w];
			const token_index_t end = corpus.csc_index_[w+1];
			// 0.1 Update C_word
			for(auto t=0; t<n_topic; t++)
				if(update_topics) C_word.at(w, t) = 0;
			for(auto i=bgn; i<end; i++) {
				Z& z = corpus.val_[corpus.csc_val_index_[i]];
			  if(update_topics) C_word.at(w, z.old_z) ++;
			}
			// 1. Accept new_z and update C_all on the fly
			for(auto i=bgn; i<end; i++) {
				Z& z = corpus.val_[corpus.csc_val_index_[i]];
				// accept and update
				if(z.new_z == z.old_z)
					continue;
				double accept_prob =
					(C_word.at(w, z.new_z) + beta)/(C_word.at(w, z.old_z) + beta) *
					(C_all[z.old_z] + beta_bar)/(C_all[z.new_z] + beta_bar);
				if(rng.drand() < accept_prob) {
				  if(update_topics) {
				    C_word.at(w, z.new_z) ++;
				    C_word.at(w, z.old_z) --;

				    C_all[z.new_z] ++;
				    C_all[z.old_z] --;

				    C_local_diff[z.new_z] ++;
				    C_local_diff[z.old_z] --;

				    C_local[z.new_z] ++;
				    C_local[z.old_z] --;
				  }
					z.old_z = z.new_z;
				}
			}
			// 2. Draw new_z
			AliasUrn<decltype(rng)> urn(rng);
			vector<double> prob;
			for(topic_index_t t=0; t<n_topic; t++)
				prob.push_back(C_word.at(w,t) + beta);
			urn.setup(prob);
			for(token_index_t i=bgn; i<end; i++) {
				Z& z = corpus.val_[corpus.csc_val_index_[i]];
				// draw
				z.new_z = urn.sample();
			}
		}
	}

	// Accept and update by doc
	void sample_by_doc(bool update_topics = true) {
		const auto n_doc = corpus.nrow();
		for(auto d = 0; d < n_doc; d++) {
			const token_index_t bgn = corpus.csr_index_[d];
			const token_index_t end = corpus.csr_index_[d+1];
			// 0. Update C_word
			for(auto t=0; t<n_topic; t++)
				C_doc.at(d, t) = 0;
			for(auto i=bgn; i<end; i++) {
				Z& z = corpus.val_[i];
				C_doc.at(d, z.old_z) ++;
			}
			// 1. Accept new_z and update C_all on the fly
			for(auto i=bgn; i<end; i++) {
				Z& z = corpus.val_[i];
				// accept and update
				if(z.new_z == z.old_z)
					continue;
				double accept_prob =
					(C_doc.at(d, z.new_z) + alpha)/(C_doc.at(d, z.old_z) + alpha) *
					(C_all[z.old_z] + beta_bar)/(C_all[z.new_z] + beta_bar);
				if(rng.drand() < accept_prob) {
				  if(update_topics) {

				    C_all[z.new_z] ++;
				    C_all[z.old_z] --;

				    C_local_diff[z.new_z] ++;
				    C_local_diff[z.old_z] --;

				    C_local[z.new_z] ++;
				    C_local[z.old_z] --;
				  }
					z.old_z = z.new_z;
				}
			}
			// 2. Draw new_z
			const unsigned Ld = end - bgn;
			const double position_sample_prob = Ld/(Ld + alpha_bar);
			for(token_index_t i=bgn; i<end; i++) {
				Z& z = corpus.val_[i];
				// draw
				if(rng.drand() < position_sample_prob)
					z.new_z = corpus.val_[bgn + (rng.sample() % Ld)].old_z;
				else
					z.new_z = rng.sample() % n_topic;
			}
		}
	}

	double pseudo_loglikelihood() { // indeed const
		double res = 0;
		corpus.apply<true>([&](Z& z, doc_index_t d, word_index_t w) {
			// Todo: This could be optimized with a HashMap
			res += log(C_word.at(w, z.old_z) + beta);
		});
		for(topic_index_t k = 0; k<n_topic; k++)
			res -= C_local[k] * log(C_local[k] + beta_bar);
		return res;
	}
};

