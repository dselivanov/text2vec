#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <algorithm>
#include <vector>
#include <assert.h>

#include "splda.h"

#include <Rcpp.h>

// nonzero entry for spvec_t
template<class T>
struct entry_t{ // {{{
	unsigned idx;
	T value;
	entry_t(unsigned idx_=0, T value_=T()): idx(idx_), value(value_){}
	bool operator<(const entry_t& other) const { return (idx < other.idx);}
	bool operator==(const entry_t& other) const {return idx == other.idx;}
}; // }}}
template<class T>
struct spidx_t: public std::vector<T>{ // {{{
	typedef typename std::vector<T> super;
	typedef size_t idx_t;
	std::vector<idx_t> nz_idx;
	spidx_t(size_t size_=0): super(size_) {}
	void gen_nz_idx() {
		nz_idx.clear();
		for(auto t = 0U; t < this->size(); t++)
			if((*this)[t] > 0) nz_idx.push_back((idx_t)t);
	}
	void push(idx_t idx) {
		size_t len = nz_idx.size();
		if(len)  {
			nz_idx.push_back(idx); // nz_idx.size() = len+1 now.
			idx_t *start = nz_idx.data(), *end=start+len;
			idx_t *ptr = std::lower_bound(start, end, idx);
			if(ptr != end) {
				memmove(ptr+1, ptr, (len - (ptr-start))*sizeof(idx_t));
				*ptr = idx;
			}
		} else {
			nz_idx.push_back(idx);
		}
	}
	void pop(idx_t idx) {
		idx_t *start = nz_idx.data(), *end = start+nz_idx.size();
		idx_t *ptr = std::lower_bound(start, end, idx);
		if(ptr != end) remove_ptr(ptr);
	}
	void remove_ptr(idx_t *ptr) {
		size_t cnt = nz_idx.size()-(ptr+1-nz_idx.data());
		if(cnt) memmove(ptr, ptr+1, cnt*sizeof(idx_t));
		nz_idx.pop_back();
	}
}; //}}}
typedef std::vector<spidx_t<unsigned>> spmat_t;

// struct alias_table { // {{{
// 	size_t size;
// 	std::vector<size_t> alias;
// 	std::vector<double> prob;
// 	std::vector<double> P;
// 	std::vector<size_t> L,S;
// 	alias_table(size_t size_ = 0, const double *p = NULL): size(size_){build_table(size_, p);}
// 	alias_table(std::vector<double>& p): alias_table(p.size(), p.data()) {}
// 	void build_table(size_t size_, const double *p) {
// 		size = size_;
// 		if(size) {
// 			alias.resize(size);
// 			prob.resize(size);
// 			P.resize(size);
// 			L.resize(size);
// 			S.resize(size);
// 		} else return;
// 		if(p==NULL) return;
// 		//std::vector<double>P(size);
// 		double sum = 0.0;
// 		for(size_t i = 0; i < size; ++i) sum+=p[i];
// 		for(size_t i = 0; i < size; ++i) P[i] = p[i]*size/sum;
// 		//std::vector<size_t>L(size), S(size);
// 		int nS = 0, nL = 0;
// 		for(size_t i = 0; i < size; ++i) {
// 			if(P[i] < 1) S[nS++] = i;
// 			else L[nL++] = i;
// 		}
// 		while( nS && nL ) {
// 			size_t a = S[--nS]; // Schwarz's l
// 			size_t g = L[--nL]; // Schwarz's g
// 			prob[a] = P[a];
// 			alias[a] = g;
// 			P[g] = P[g] + P[a] - 1;
// 			if ( P[g] < 1 ) S[nS++] = g;
// 			else L[nL++] = g;
// 		}
// 		while(nL) prob[L[--nL]] = 1;
// 		while(nS) prob[S[--nS]] = 1;
// 	}
// 	void build_table(std::vector<double>& p) { build_table(p.size(), p.data()); }
// 	size_t sample(double urnd1, double urnd2) {
// 		size_t idx = (size_t)floor(urnd1*size);
// 		return urnd2 < prob[idx] ? idx : alias[idx];
// 	}
// 	size_t sample(double urnd2) {
// 		size_t idx = rand()%size;
// 		return urnd2 < prob[idx] ? idx : alias[idx];
// 	}
// }; // }}}
// F+Tree Implementation
// struct htree_t{ // {{{
// 	size_t size;     // real size of valid
// 	size_t elements; // 2^ceil(log2(size))
// 	std::vector<double> val;
// 	double *true_val;
// 	size_t init_time, update_time, sample_time;
// 	double& operator[](size_t idx) { assert(idx < elements); return true_val[idx]; }
// 	const double& operator[] (size_t idx) const { assert(idx < elements); return true_val[idx]; }
// 	double get_init_time() {return (double)init_time/CLOCKS_PER_SEC;}
// 	double get_update_time() {return (double)update_time/CLOCKS_PER_SEC;}
// 	double get_sample_time() {return (double)sample_time/CLOCKS_PER_SEC;}
// 	void init_dense(){
// 		for(size_t pos = elements-1; pos > 0; --pos)
// 			val[pos] = val[pos<<1] + val[(pos<<1)+1];
// 	}
// 	void update_parent(size_t idx, double delta) { // {{{
// 		idx = (idx+elements)>>1;
// 		while(idx) {
// 			val[idx] += delta;
// 			idx >>= 1;
// 		}
// 	} // }}}
// 	void update(size_t idx) { // {{{
// 		double value = val[idx+=elements]; // delta update
// 		value -= (val[idx>>1]-val[idx^1]);
// 		while(idx) {
// 			val[idx] += value;
// 			idx >>= 1;
// 		}
// 	} // }}}
// 	void set_value(size_t idx, double value) { // {{{
// 		value -= val[idx+=elements]; // delta update
// 		while(idx) {
// 			val[idx] += value;
// 			idx >>= 1;
// 		}
// 	} // }}}
// 	// urnd: uniformly random number between [0,1]
// 	size_t log_sample(double urnd) { // {{{
// 		//urnd *= val[1];
// 		size_t pos = 1;
// 		while(pos < elements) {
// 			pos <<= 1;
// 			if(urnd >= val[pos])
// 				urnd -= val[pos++];
// 			/*
// 			double tmp = urnd - val[pos];
// 			if(tmp >= 0) {
// 				urnd = tmp;
// 				pos++;
// 			}*/
// 			/*
// 			if(urnd < val[pos*2])
// 				pos = pos*2;
// 			else {
// 				urnd -= val[pos*2];
// 				pos = pos*2+1;
// 			}
// 			*/
// 		}
// 		return pos-elements;
// 	} // }}}
// 	size_t linear_sample(double urnd) { // {{{
// 		urnd = urnd*val[1];
// 		size_t pos = elements;
// 		while(urnd > 0)
// 			urnd -= val[pos++];
// 		return pos-elements;
// 	} // }}}
// 	double total_sum() {return val[1];}
// 	htree_t(size_t size=0): init_time(0),update_time(0),sample_time(0) { resize(size); }
// 	void resize(size_t size_ = 0) { // {{{
// 		size = size_;
// 		if(size == 0) val.resize(0);
// 		elements = 1;
// 		while(elements < size) elements *= 2;
// 		val.clear(); val.resize(2*elements, 0.0);
// 		true_val = &val[elements];
// 	} //}}}
// 	void clear() { for(auto &v: val) v = 0; }
// }; // }}}
void lda_read_data(const char* srcdir, lda_smat_t &training_set, lda_smat_t &test_set, int nr_threads, smat_t::format_t fmt){//{{{
	size_t nr_docs, nr_words, nnz, Z_len;
	char filename[1024], buf[1024], suffix[12];
	FILE *fp;
	sprintf(filename,"%s/meta",srcdir);
	fp = fopen(filename,"r");
	if(fscanf(fp, "%lu", &nr_words) != 1) {
		fprintf(stderr, "Error: corrupted meta in line 1 of %s\n", srcdir);
		return;
	}

	if(fscanf(fp, "%lu %lu %lu %s", &nr_docs, &nnz, &Z_len, buf) != 4) {
		fprintf(stderr, "Error: corrupted meta in line 2 of %s\n", srcdir);
		return;
	}
	if(fmt == smat_t::TXT)
		strcpy(suffix, "");
	else if(fmt == smat_t::PETSc)
		strcpy(suffix, ".petsc");
	else
		printf("Error: fmt %d is not supported.", fmt);
	sprintf(filename,"%s/%s%s", srcdir, buf, suffix);

	training_set.load_data(nr_docs, nr_words, nnz, filename, fmt);

	if(fscanf(fp, "%lu %lu %lu %s", &nr_docs, &nnz, &Z_len, buf) != EOF){
		sprintf(filename,"%s/%s%s", srcdir, buf, suffix);
		test_set.load_data(nr_docs, nr_words, nnz, filename, fmt);
	}
	fclose(fp);
	return ;
}//}}}

// Sparse-sampling with 3 terms (used in Yahoo LDA)
//void run_splda(lda_smat_t &training, lda_smat_t &test, int k, int iters, double alpha, double beta) { // {{{
Rcpp::List run_splda(lda_smat_t &training, lda_smat_t &test, int k, int iters, double alpha, double beta) { // {{{
	// initialize count vectors
	std::vector<size_t> Nt(k, 0);
	spmat_t Nwt(training.nr_words, spidx_t<unsigned>(k));
	spmat_t Ndt(training.nr_docs, spidx_t<unsigned>(k));
	training.initialize_Z_docwise(k);

	for(auto d = 0U; d <training.nr_docs; d++) {
		for(auto idx = training.doc_ptr[d]; idx != training.doc_ptr[d+1]; idx++) {
			auto w = training.word_idx[idx];
			for(auto zidx = training.Z_ptr[idx]; zidx != training.Z_ptr[idx+1]; zidx++) {
				auto t = training.Zval[zidx];
				Nt[t]++;
				Ndt[d][t]++;
				Nwt[w][t]++;
			}
		}
	}
	for(auto &Nw: Nwt) Nw.gen_nz_idx();
	for(auto &Nd: Ndt) Nd.gen_nz_idx();

	double alphabar = alpha*k;
	double betabar = beta*training.nr_words;

	auto compute_training_LL = [&]()->double { // {{{
		double LL = 0;
		for(auto doc = 0U; doc < training.nr_docs; doc++) {
			auto &Nd = Ndt[doc];
			size_t sum_Nd = 0;
			for(auto t : Nd.nz_idx) {
				LL += lgamma(alpha+Nd[t])-lgamma(alpha);
				sum_Nd += Nd[t];
			}
			LL += lgamma(alphabar) - lgamma(alphabar+sum_Nd);
		}
		size_t nonZeroTypeTopics = 0;
		for(auto word = 0U; word < training.nr_words; word++) {
			auto &Nw = Nwt[word];
			for(auto t : Nw.nz_idx) {
				nonZeroTypeTopics++;
				LL += lgamma(beta+Nw[t]);
			}
		}
		size_t valid_topics = 0;
		for(auto t = 0; t < k; t++) if(Nt[t] != 0) {
			LL -= lgamma(betabar+Nt[t]);
			valid_topics++;
		}
		LL += valid_topics*lgamma(betabar)-nonZeroTypeTopics*lgamma(beta);
		return LL;
	}; // }}}

	std::vector<double> Cstatic(k);
	std::vector<entry_t<double> > C(k);
	size_t total_cputime = 0;
	for(int iter = 1; iter <= iters; iter++) {
		size_t nr_A=0, nr_B=0, nr_C=0;
		std::clock_t start_time = clock();
		for(auto d = 0U; d < training.nr_docs; d++) {
			if(training.doc_ptr[d] == training.doc_ptr[d+1])
				continue;
			// per-document variables
			double Asum=0, Bsum=0, Csum=0;
			auto &Nd = Ndt[d];
			for(auto t = 0; t < k; t++) {
				register double reg_denom = Nt[t]+betabar;
				register size_t reg_ndt = Nd[t];
				//Asum += beta*alpha/reg_denom;
				Asum += 1.0/reg_denom;
				if(reg_ndt==0)
					Cstatic[t] = alpha/reg_denom;
				else {
					Cstatic[t] = (reg_ndt+alpha)/reg_denom;
					//Bsum += beta*reg_ndt/reg_denom;
					Bsum += reg_ndt/reg_denom;
				}
			}
			Asum *= beta*alpha;
			Bsum *= beta;

			for(auto idx = training.doc_ptr[d]; idx != training.doc_ptr[d+1]; idx++) {
				auto w = training.word_idx[idx];
				auto &Nw = Nwt[w];
				for(auto Zidx = training.Z_ptr[idx]; Zidx != training.Z_ptr[idx+1]; Zidx++) {
					// handle each occurrence of word w
					register size_t old_topic = training.Zval[Zidx];
					register double reg_denom = Nt[old_topic]+betabar;
					register size_t reg_ndt = Nd[old_topic]--;

					// removal old_topic
					Asum -= beta*alpha/reg_denom;
					if(reg_ndt)
						Bsum -= beta*reg_ndt/reg_denom;
					--reg_ndt;
					--reg_denom;
					--Nt[old_topic];
					Asum += beta*alpha/reg_denom;
					if(reg_ndt==0) {
						Nd.pop(old_topic);
						Cstatic[old_topic] = alpha/reg_denom;
					} else {
						Bsum += beta*reg_ndt/reg_denom;
						Cstatic[old_topic] = (reg_ndt+alpha)/reg_denom;
					}
					// Csum requires re-computation everytime.
					Csum = 0.0;
					size_t nz_C = 0;
					size_t* ptr_old_topic = NULL;
					for(auto &t: Nw.nz_idx) {
						if(t == old_topic) {
							Nw[t]--;
							if(Nw[t]==0)
								ptr_old_topic = &t;
						}
						if(Nw[t]>0) {
							C[nz_C].idx = t;
							C[nz_C].value = Cstatic[t]*Nw[t];
							Csum += C[nz_C].value;
							nz_C++;
						}
					}
					if(ptr_old_topic)
						Nw.remove_ptr(ptr_old_topic);

					register int new_topic=-1;
					double sample = drand48()*(Asum+Bsum+Csum);
					// sampling new topic // {{{
					if(sample < Csum) {
						auto *ptr = C.data();
						while((sample-=ptr->value) > 0)
							ptr++;
						new_topic = ptr->idx;
						nr_C++;
					} else {
						sample -= Csum;
						if(sample < Bsum) {
							sample /= beta;
							for(auto &t : Nd.nz_idx) {
								sample -= Nd[t]/(Nt[t]+betabar);
								if(sample < 0) {
									new_topic = t;
									break;
								}
							}
							nr_B++;
						} else {
							sample -= Bsum;
							sample /= (alpha*beta);
							for(auto t = 0; t < k; t++) {
								sample -= 1.0/(betabar+Nt[t]);
								if(sample < 0) {
									new_topic = t;
									break;
								}
							}
							nr_A++;
						}
					} // }}}
					training.Zval[Zidx] = new_topic;

					// Add counts for the new_topic
					reg_denom = Nt[new_topic]+betabar;
					reg_ndt = Nd[new_topic]++;

					Asum -= beta*alpha/reg_denom;
					if(reg_ndt)
						Bsum -= beta*reg_ndt/reg_denom;
					++reg_ndt;
					++reg_denom;
					++Nt[new_topic];
					++Nw[new_topic];
					if(reg_ndt==1)
						Nd.push(new_topic);
					if(Nw[new_topic]==1)
						Nw.push(new_topic);
					Asum += beta*alpha/reg_denom;
					Bsum += beta*reg_ndt/reg_denom;
					Cstatic[new_topic] = (reg_ndt+alpha)/reg_denom;
				}
			}
		}
		size_t singleiter_time = (std::clock()-start_time);
		total_cputime += singleiter_time;
		double all = (double)(nr_A+nr_B+nr_C);
		printf("iter %d nr_A %.2f nr_B %.2f nz_C %.2f LL %.6g cputime %.2f iter-time %.2f\n", iter, (double)nr_A/all, (double)nr_B/all, (double)nr_C/all, compute_training_LL(), (double)total_cputime/CLOCKS_PER_SEC, (double)singleiter_time/CLOCKS_PER_SEC);
		fflush(stdout);
	}

	Rcpp::IntegerMatrix doc_topic_distr(training.nr_docs, k);
	Rcpp::IntegerMatrix topic_word_distr(training.nr_words, k);
	for(uint32_t i = 0; i < training.nr_docs; i++)
	  for(uint32_t j = 0; j < k; j++)
	    doc_topic_distr(i, j) = Ndt[i][j];
	for(uint32_t i = 0; i < training.nr_words; i++)
	  for(uint32_t j = 0; j < k; j++)
	    topic_word_distr(i, j) = Nwt[i][j];
  return Rcpp::List::create(Rcpp::_["doc_topic_distr"]  = doc_topic_distr,
                            Rcpp::_["topic_word_distr"] = topic_word_distr);
                            //Rcpp::_["a"] = training.word_count);
} // }}}
// F+LDA Sampling with 2 terms doc-by-doc (best for now) _test
// void run_splda_fast_2_test(lda_smat_t &training, lda_smat_t &test, int k, int iters, double alpha, double beta) { // {{{
// 	// initialize count vectors
// 	typedef std::vector<spidx_t<unsigned>> spmat_t;
// 	std::vector<size_t> Nt(k, 0);
// 	spmat_t Nwt(training.nr_words, spidx_t<unsigned>(k));
// 	spmat_t Ndt(training.nr_docs, spidx_t<unsigned>(k));
// 	training.initialize_Z_docwise(k);
// 	std::vector<size_t> nnz_w(training.nr_words), nnz_d(training.nr_docs);
// 	size_t	nnz_t = 0;
//
// 	for(auto d = 0U; d <training.nr_docs; d++) {
// 		for(auto idx = training.doc_ptr[d]; idx != training.doc_ptr[d+1]; idx++) {
// 			auto w = training.word_idx[idx];
// 			for(auto zidx = training.Z_ptr[idx]; zidx != training.Z_ptr[idx+1]; zidx++) {
// 				auto t = training.Zval[zidx];
// 				Nt[t]++;
// 				Ndt[d][t]++;
// 				Nwt[w][t]++;
// 				nnz_w[w]++;
// 				nnz_d[d]++;
// 				nnz_t++;
// 			}
// 		}
// 	}
// 	for(auto &Nw: Nwt) Nw.gen_nz_idx();
// 	for(auto &Nd: Ndt) Nd.gen_nz_idx();
//
// 	double alphabar = alpha*k;
// 	double betabar = beta*training.nr_words;
//
// 	auto compute_training_LL = [&]()->double { // {{{
// 		double LL = 0;
// 		for(auto doc = 0U; doc < training.nr_docs; doc++) {
// 			auto &Nd = Ndt[doc];
// 			size_t sum_Nd = 0;
// 			for(auto t = 0; t < k; t++) if(Nd[t]){
// 				LL += lgamma(alpha+Nd[t])-lgamma(alpha);
// 				sum_Nd += Nd[t];
// 			}
// 			LL += lgamma(alphabar) - lgamma(alphabar+sum_Nd);
// 		}
// 		size_t nonZeroTypeTopics = 0;
// 		for(auto word = 0U; word < training.nr_words; word++) {
// 			auto &Nw = Nwt[word];
// 			for(auto t = 0; t < k; t++) if(Nw[t]){
// 				nonZeroTypeTopics++;
// 				LL += lgamma(beta+Nw[t]);
// 			}
// 		}
// 		size_t valid_topics = 0;
// 		for(auto t = 0; t < k; t++) if(Nt[t] != 0) {
// 			LL -= lgamma(betabar+Nt[t]);
// 			valid_topics++;
// 		}
// 		LL += valid_topics*lgamma(betabar)-nonZeroTypeTopics*lgamma(beta);
// 		return LL;
// 	}; // }}}
//
// auto compute_perplexity = [&]()->double {//{{{
// 	return 1;
// 	auto dim = k;
// 	auto nr_words = training.nr_words;
// 	size_t nr_samples = 20; //param.perplexity_samples;
//
// 	// assume param is a cummulated sum
// 	auto cat_sample = [&](double_vec_t &param, unsigned short seed[3]) -> int{
// 		double randnum = erand48(seed) * param[dim-1];
// 		auto it = std::upper_bound(param.begin(), param.end(), randnum);
// 		return it - param.begin();
// 	};
//
// 	auto logsumexp = [&](double_vec_t& x) -> double{
// 		auto largest = *std::max_element(x.begin(), x.end());
// 		double result = 0;
// 		for(auto &v : x)
// 			result += exp(v - largest);
// 		return log(result)+largest;
// 	};
//
// 	/*
// 	std::vector<size_t> Nt(dim);
// 	for(auto word = 0U; word < nr_words; word++) {
// 		auto &Nw = Nwt[word];
// 		for(auto t = 0U; t < dim; t++)
// 			Nt[t] += Nw[t];
// 	}
// 	*/
// 	double_mat_t topic_dist_params(nr_words, double_vec_t(dim,0.0));
// 	for(auto word = 0U; word < nr_words; word++) {
// 		auto &Nw = Nwt[word];
// 		auto &param = topic_dist_params[word];
// 		auto sum = 0;
// 		for(auto t = 0U; t < dim; t++) {
// 			param[t] = (sum+=((double)Nw[t]+beta)/((double)Nt[t]+betabar));
// 		}
// 	}
//
// 	//auto nr_threads = omp_get_max_threads();
// 	auto nr_threads = 1;
// 	std::vector<size_t>totalw_pool(nr_threads);
// 	std::vector<double>perplexity_pool(nr_threads);
// 	std::vector<std::vector<size_t>>hist_pool(nr_threads, std::vector<size_t>(dim));
// 	std::vector<double_vec_t>logweights_pool(nr_threads, double_vec_t(nr_samples));
//
// 	std::vector<std::array<uint16_t,3>> seed_pool(nr_threads);
// 	for(auto s = 0; s < nr_threads; s++) {
// 		seed_pool[s][0] = (unsigned short) (s | nr_threads);
// 		seed_pool[s][1] = (unsigned short) (s<<1 | nr_threads);
// 		seed_pool[s][2] = (unsigned short) (s<<2 | nr_threads);
// 	}
// 	/*
//
// % Draw samples from the q-distribution
// samples = zeros(Nd, num_samples);
// for n = 1:Nd
//     samples(n, :) = discreternd(num_samples, qq(:, n))'; % Nd x num_samples
// end
//
// % Evaluate P(z, v) at samples and compare to q-distribution
// Nk = histc(samples, 1:T, 1); % T x num_samples
// log_pz = sum(gammaln(bsxfun(@plus, Nk, topic_prior')), 1) ...
//         + gammaln(topic_alpha) - sum(gammaln(topic_prior)) ...
//         - gammaln(Nd + topic_alpha); % 1 x num_samples
// log_w_given_z = zeros(1, num_samples);
// for n = 1:Nd
//     log_w_given_z = log_w_given_z + log(topics(samples(n,:), words(n)))';
// end
// log_joint = log_pz + log_w_given_z;
// %
// log_qq = zeros(1, num_samples);
// for n = 1:Nd
//     log_qq = log_qq + log(qq(samples(n,:), n))';
// end
// log_weights = log_joint - log_qq;
// log_evidence = logsumexp(log_weights(:)) - log(length(log_weights));
// */
//
// 	auto computer_doc_perplexity = [&](size_t doc) { // {{{
// 		//auto thread_id = omp_get_thread_num();
// 		auto thread_id = 1;
// 		auto &totalw = totalw_pool[thread_id];
// 		auto &perplexity = perplexity_pool[thread_id];
// 		auto &hist = hist_pool[thread_id];
// 		auto &logweights = logweights_pool[thread_id];
// 		auto *seed = &seed_pool[thread_id][0];
// 		size_t cnt = 0;
// 		for(auto s = 0U; s < nr_samples; s++) {
// 			for(auto &t: hist) t = 0;
// 			double log_qq = 0.0, log_joint = 0.0;
// 			cnt = 0;
// 			for(auto idx = test.doc_ptr[doc]; idx != test.doc_ptr[doc+1]; idx++) {
// 				auto word = test.word_idx[idx];
// 				auto occurrence = test.val_t[idx];
// 				if(topic_dist_params[word][dim-1] == 0) continue;
// 				auto &topic_param = topic_dist_params[word];
// 				for(auto o = 0; o < occurrence; o++) {
// 					auto t = cat_sample(topic_param, seed);
// 					hist[t]++;
// 					double norm_const = topic_param[dim-1];
// 					double param_t = topic_param[t];
// 					if(t != 0) param_t -= topic_param[t-1];
// 					log_qq += log(param_t/norm_const);
// 					log_joint += log(param_t);
// 					cnt++;
// 				}
// 			}
// 			log_joint += lgamma(alphabar)-lgamma(alphabar+cnt);
// 			for(auto t = 0U; t < dim; t++)
// 				log_joint += lgamma(hist[t]+alpha)-lgamma(alpha);
// 			logweights[s] = log_joint - log_qq;
// 		}
// 		printf("perplexity(%ld) %g %g\n",doc, logsumexp(logweights) , log(nr_samples));
// 		perplexity += logsumexp(logweights) - log(nr_samples);
// 		totalw += cnt;
// 	}; //}}}
//
// 	//omp_set_num_threads(nr_threads);
// //#pragma omp parallel for
// 	for(auto doc = test.start_doc; doc < test.end_doc; doc++)
// 		computer_doc_perplexity(doc);
//
// 	size_t totalw_local = 0, totalw = 0;
// 	for(auto &w: totalw_pool) totalw_local += w;
// 	//MPI_Barrier(MPI_COMM_WORLD);
// 	//MPI_Allreduce(&totalw_local, &totalw, 1, MPI_LONG_LONG, MPI_SUM, MPI_COMM_WORLD);
//
// 	double perplexity_local = 0.0, perplexity = 0.0;
// 	for(auto &p: perplexity_pool) perplexity_local += p;
// 	//MPI_Allreduce(&perplexity_local, &perplexity, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
// 	return exp(-perplexity_local/totalw_local);
// 	return exp(-perplexity/totalw);
// };//}}}
//
// 	htree_t B(k), D(k);
// 	//std::vector<entry_t<double> > C(k); // for inner product
// 	std::vector<double> C(k); // for inner product
// 	std::vector<size_t> C_idx(k); // for inner product
// 	std::vector<double>A(k,0.0);
// 	//size_t threshold = (size_t) floor(2.0*k/(log2((double)k)));
// 	size_t threshold = (size_t) floor(15.0*k/(log2((double)k)));
// 	size_t total_cputime = 0;
// 	//size_t inner_product_time = 0, inner_sample_time = 0;
// 	std::clock_t tmp_start = clock();
// 	for(auto t = 0; t < k; t++)
// 		D[t] = alpha/(Nt[t]+betabar);
// 	D.init_dense();
// 	total_cputime += clock() - tmp_start;
// 	for(int iter = 1; iter <= iters; iter++) {
// 		size_t nr_A=0, nr_B=0, nr_C=0, nr_D=0;
// 		std::clock_t start_time = clock();
// 		for(auto d = 0U; d < training.nr_docs; d++) {
// 			if(training.doc_ptr[d] == training.doc_ptr[d+1])
// 				continue;
// 			// per-document variables (D)
// 			auto &Nd = Ndt[d];
// 			if(Nd.nz_idx.size() > threshold) {
// 				for(auto t: Nd.nz_idx)
// 					D.true_val[t] = (alpha+Nd[t])/(Nt[t]+betabar);
// 				D.init_dense();
// 			} else {
// 				for(auto t: Nd.nz_idx)
// 					D.set_value(t, (alpha+Nd[t])/(Nt[t]+betabar));
// 			}
// 			/*
// 				D.true_val[t] = (alpha+Nd[t])/(Nt[t]+betabar);
// 			D.init_sparse(Nd.nz_idx.data(), Nd.nz_idx.size());
// 			*/
//
// 			for(auto idx = training.doc_ptr[d]; idx != training.doc_ptr[d+1]; idx++) {
// 				auto w = training.word_idx[idx];
// 				auto &Nw = Nwt[w];
// 				for(auto Zidx = training.Z_ptr[idx]; Zidx != training.Z_ptr[idx+1]; Zidx++) {
// 					// handle each occurrence of word w
// 					// Remove counts for the old_topic
// 					register size_t old_topic = training.Zval[Zidx];
// 					register double reg_denom = 1.0/((--Nt[old_topic])+betabar);
// 					--Nw[old_topic];--Nd[old_topic];
// 					double D_old = D.true_val[old_topic];
// 					double D_new = reg_denom*(alpha+Nd[old_topic]);
// 					D.true_val[old_topic] = D_new;
// 					bool updated = false;
//
// 					// Handle Inner Product (Part C) {{{
// 					size_t nz_C = 0;
// 					size_t *ptr_Nw_old_topic = NULL;
// 					double Csum = 0;
// 					for(auto &t : Nw.nz_idx) {
// 						if(Nw[t]) {
// 							/*
// 							C[nz_C].idx = t;
// 							//C[nz_C].value = Nw[t]*(Nd[t]+alpha)*D[t];
// 							//C[nz_C].value = Nw[t]*(B[t]+alpha*D[t]);
// 							//C[nz_C].value = Nw[t]*(B[t]+D[t]);
// 							//C[nz_C].value = Nw[t]*(B[t]);
// 							//C[nz_C].value = A[t]*Nd[t];
// 							C[nz_C].value = Nw[t]*D.true_val[t];
// 							Csum += C[nz_C].value;
// 							*/
// 							C[nz_C] = (Csum += Nw[t]*D.true_val[t]);
// 							/*
// 							C[nz_C] = Nw[t]*D.true_val[t];
// 							Csum += C[nz_C];
// 							*/
// 							C_idx[nz_C] = t;
// 							nz_C++;
// 						} else {
// 							ptr_Nw_old_topic = &t;
// 						}
// 					}
// 	//				if(ptr_Nw_old_topic) Nw.remove_ptr(ptr_Nw_old_topic);
// 					// }}}
//
// 					register int new_topic = -1;
// 					double Dsum = Csum+beta*(D.total_sum()-D_old+D_new);
// 					//double Dsum = Csum+beta*D.total_sum();
// 					double sample = drand48()*Dsum;
// 					//printf("sample %g Csum %g Asum %g Bsum %g Dsum %g\n", sample, Csum, Asum, Bsum, Dsum);
// 					if(sample < Csum) { // {{{
// 						auto *ptr = C.data();
// 						new_topic = C_idx[std::upper_bound(ptr, ptr+nz_C, sample)-ptr];
// 						/*
// 						while((sample-=*ptr)>0)
// 							ptr++;
// 						new_topic = C_idx[ptr-C.data()];
// 						*/
// 						/*
// 						while((sample-=ptr->value) > 0)
// 							ptr++;
// 						new_topic = ptr->idx;
// 						*/
// 					//	nr_C++;
// 					} else {
// 						D.update_parent(old_topic, D_new-D_old);
// 						updated = true;
// 						sample = (sample-Csum)/beta;
// 						new_topic = D.log_sample(sample);
// 					//	nr_D++;
// 					} // }}}
// 					training.Zval[Zidx] = new_topic;
// 					assert(new_topic != -1 && new_topic < k);
//
// 					// Add counts for the new_topic
// 					reg_denom = 1./((++Nt[new_topic])+betabar);
// 					++Nd[new_topic]; ++Nw[new_topic];
//
// 					if((int)old_topic != new_topic) {
// 						if(Nd[old_topic]==0) Nd.pop(old_topic);
// 						if(ptr_Nw_old_topic) Nw.remove_ptr(ptr_Nw_old_topic);
// 						if(Nw[new_topic]==1) Nw.push(new_topic);
// 						if(Nd[new_topic]==1) Nd.push(new_topic);
// 						if(not updated) D.update_parent(old_topic, D_new-D_old);
// 						D.set_value(new_topic, reg_denom*(alpha+Nd[new_topic]));
// 					} else {
// 						//if(not updated) D.update_parent(old_topic, D_new-D_old);
// 						//D.set_value(new_topic, reg_denom*(beta+Nw[new_topic]));
// 						if(updated) D.set_value(old_topic, D_old);
// 						else D.true_val[old_topic] = D_old;
// 					}
//
// 				}
// 			}
// 			if(Nd.nz_idx.size() > threshold) {
// 				for(auto t: Nd.nz_idx)
// 					D.true_val[t] = (alpha)/(Nt[t]+betabar);
// 				D.init_dense();
// 			} else {
// 				for(auto t: Nd.nz_idx)
// 					D.set_value(t, (alpha)/(Nt[t]+betabar));
// 			}
//
// 			/*
// 				D.true_val[t] = alpha/(Nt[t]+betabar);
// 			D.init_sparse(Nd.nz_idx.data(), Nd.nz_idx.size());
// 			*/
// 		}
// 		size_t singleiter_time = (std::clock()-start_time);
// 		total_cputime += singleiter_time;
// 		double all = (double)(nr_A+nr_B+nr_C+nr_D);
// 		printf("iter %d nr_C %.2f nr_D %.2f LL %.6g perplexity %.6g cputime %.2f iter-time %.2f\n",
// 				iter, (double)nr_C/all, (double)nr_D/all,
// 				compute_training_LL(), compute_perplexity(), (double)total_cputime/CLOCKS_PER_SEC, (double)singleiter_time/CLOCKS_PER_SEC);
// 		fflush(stdout);
// 	}
// } // }}}
// // F+LDA Sampling with 2 terms word-by-word(best for now) _test
// void run_splda_fast_2_word_test(lda_smat_t &training, lda_smat_t &test, int k, int iters, double alpha, double beta) { // {{{
// 	// initialize count vectors
// 	typedef std::vector<spidx_t<unsigned>> spmat_t;
// 	std::vector<size_t> Nt(k, 0);
// 	spmat_t Nwt(training.nr_words, spidx_t<unsigned>(k));
// 	spmat_t Ndt(training.nr_docs, spidx_t<unsigned>(k));
// 	training.initialize_Z_wordwise(k);
// 	std::vector<size_t> nnz_w(training.nr_words), nnz_d(training.nr_docs);
// 	size_t	nnz_t = 0;
//
// 	for(auto w = 0U; w < training.nr_words; w++) {
// 		for(auto idx = training.word_ptr[w]; idx != training.word_ptr[w+1]; idx++) {
// 			auto d = training.doc_idx[idx];
// 			for(auto zidx = training.Z_ptr[idx]; zidx != training.Z_ptr[idx+1]; zidx++) {
// 				auto t = training.Zval[zidx];
// 				Nt[t]++;
// 				Ndt[d][t]++;
// 				Nwt[w][t]++;
// 			}
// 		}
// 	}
// 	for(auto &Nw: Nwt) Nw.gen_nz_idx();
// 	for(auto &Nd: Ndt) Nd.gen_nz_idx();
//
// 	double alphabar = alpha*k;
// 	double betabar = beta*training.nr_words;
//
// 	auto compute_training_LL = [&]()->double { // {{{
// 		double LL = 0;
// 		for(auto doc = 0U; doc < training.nr_docs; doc++) {
// 			auto &Nd = Ndt[doc];
// 			size_t sum_Nd = 0;
// 			for(auto t = 0; t < k; t++) if(Nd[t]){
// 				LL += lgamma(alpha+Nd[t])-lgamma(alpha);
// 				sum_Nd += Nd[t];
// 			}
// 			LL += lgamma(alphabar) - lgamma(alphabar+sum_Nd);
// 		}
// 		size_t nonZeroTypeTopics = 0;
// 		for(auto word = 0U; word < training.nr_words; word++) {
// 			auto &Nw = Nwt[word];
// 			for(auto t = 0; t < k; t++) if(Nw[t]){
// 				nonZeroTypeTopics++;
// 				LL += lgamma(beta+Nw[t]);
// 			}
// 		}
// 		size_t valid_topics = 0;
// 		for(auto t = 0; t < k; t++) if(Nt[t] != 0) {
// 			LL -= lgamma(betabar+Nt[t]);
// 			valid_topics++;
// 		}
// 		LL += valid_topics*lgamma(betabar)-nonZeroTypeTopics*lgamma(beta);
// 		return LL;
// 	}; // }}}
//
// 	htree_t B(k), D(k);
// 	//std::vector<entry_t<double> > C(k); // for inner product
// 	std::vector<double> C(k); // for inner product
// 	std::vector<size_t> C_idx(k); // for inner product
// 	std::vector<double>A(k,0.0);
// 	//size_t threshold = (size_t) floor(2.0*k/(log2((double)k)));
// 	size_t threshold = (size_t) floor(15.0*k/(log2((double)k)));
// 	size_t total_cputime = 0;
// 	//size_t inner_product_time = 0, inner_sample_time = 0;
// 	std::clock_t tmp_start = clock();
// 	for(auto t = 0; t < k; t++)
// 		D[t] = beta/(Nt[t]+betabar);
// 	D.init_dense();
// 	total_cputime += clock() - tmp_start;
// 	for(int iter = 1; iter <= iters; iter++) {
// 		size_t nr_A=0, nr_B=0, nr_C=0, nr_D=0;
// 		std::clock_t start_time = clock();
// 		for(auto w = 0U; w < training.nr_words; w++) {
// 			if(training.word_ptr[w] == training.word_ptr[w+1])
// 				continue;
// 			// per-word variables (D)
// 			auto &Nw = Nwt[w];
// 			if(Nw.nz_idx.size() > threshold){
// 				for(auto t: Nw.nz_idx) {
// 					//double denom = 1.0/(Nt[t]+betabar); D.true_val[t] = (beta+Nw[t])*denom;
// 					D.true_val[t] = (beta+Nw[t])/(Nt[t]+betabar);
// 				}
// 				D.init_dense();
// 			} else  {
// 				for(auto t: Nw.nz_idx) {
// 					//double denom = 1.0/(Nt[t]+betabar); D.set_value(t, (beta+Nw[t])*denom);
// 					D.set_value(t, (beta+Nw[t])/(Nt[t]+betabar));
// 				}
// 			}
// 			/*
// 				D.true_val[t] = (alpha+Nd[t])/(Nt[t]+betabar);
// 			D.init_sparse(Nd.nz_idx.data(), Nd.nz_idx.size());
// 			*/
//
// 			for(auto idx = training.word_ptr[w]; idx != training.word_ptr[w+1]; idx++) {
// 				auto d = training.doc_idx[idx];
// 				auto &Nd = Ndt[d];
// 				for(auto Zidx = training.Z_ptr[idx]; Zidx != training.Z_ptr[idx+1]; Zidx++) {
// 					// handle each occurrence of doc d
// 					// Remove counts for the old_topic
// 					register size_t old_topic = training.Zval[Zidx];
// 					register double reg_denom = 1.0/((--Nt[old_topic])+betabar);
// 					--Nw[old_topic];--Nd[old_topic];
// 					double D_old = D.true_val[old_topic];
// 					double D_new = reg_denom*(beta+Nw[old_topic]);
// 					D.true_val[old_topic] = D_new;
// 					bool updated = false;
// 					//D.set_value(old_topic, reg_denom*(beta+Nw[old_topic]));
// 					//D.update_parent(old_topic, D_new-D_old);
// 					//if(Nw[old_topic]==0) Nw.pop(old_topic);
//
// 					// Handle Inner Product (Part C) {{{
// 					size_t nz_C = 0;
// 					size_t *ptr_Nd_old_topic = NULL;
// 					double Csum = 0;
// 					for(auto &t : Nd.nz_idx) {
// 						if(Nd[t]) {
// 							/*
// 							C[nz_C].idx = t;
// 							//C[nz_C].value = Nw[t]*(Nd[t]+alpha)*D[t];
// 							//C[nz_C].value = Nw[t]*(B[t]+alpha*D[t]);
// 							//C[nz_C].value = Nw[t]*(B[t]+D[t]);
// 							//C[nz_C].value = Nw[t]*(B[t]);
// 							//C[nz_C].value = A[t]*Nd[t];
// 							C[nz_C].value = Nd[t]*D.true_val[t];
// 							Csum += C[nz_C].value;
// 							*/
// 							C[nz_C] = (Csum += Nd[t]*D.true_val[t]);
// 							C_idx[nz_C] = t;
// 							nz_C++;
// 						} else {
// 							ptr_Nd_old_topic = &t;
// 						}
// 					}
// 					//if(ptr_Nd_old_topic) Nd.remove_ptr(ptr_Nd_old_topic);
// 					// }}}
//
// 					register int new_topic = -1;
// 					double Dsum = Csum+alpha*(D.total_sum()-D_old+D_new);
// 					//double Dsum = Csum+alpha*D.total_sum();
// 					double sample = drand48()*Dsum;
// 					//printf("sample %g Csum %g Asum %g Bsum %g Dsum %g\n", sample, Csum, Asum, Bsum, Dsum);
// 					if(sample < Csum) { // {{{
// 						auto *ptr = C.data();
// 						new_topic = C_idx[std::upper_bound(ptr, ptr+nz_C, sample)-ptr];
// 						/*
// 						while((sample-=ptr->value) > 0)
// 							ptr++;
// 						new_topic = ptr->idx;
// 						*/
// 						nr_C++;
// 					} else {
// 						sample = (sample-Csum)/alpha;
// 						D.update_parent(old_topic, D_new-D_old);
// 						updated = true;
// 						new_topic = D.log_sample(sample);
// 						nr_D++;
// 					} // }}}
// 					training.Zval[Zidx] = new_topic;
// 					//assert(new_topic != -1 && new_topic < k);
//
// 					// Add counts for the new_topic
// 					reg_denom = 1./((++Nt[new_topic])+betabar);
// 					++Nd[new_topic]; ++Nw[new_topic];
// 					//if(not updated) D.update_parent(old_topic, D_new-D_old);
// 					//D.set_value(new_topic, reg_denom*(beta+Nw[new_topic]));
// 					if((int)old_topic != new_topic) {
// 						if(Nw[old_topic]==0) Nw.pop(old_topic);
// 						if(ptr_Nd_old_topic) Nd.remove_ptr(ptr_Nd_old_topic);
// 						if(Nw[new_topic]==1) Nw.push(new_topic);
// 						if(Nd[new_topic]==1) Nd.push(new_topic);
// 						if(not updated) D.update_parent(old_topic, D_new-D_old);
// 						D.set_value(new_topic, reg_denom*(beta+Nw[new_topic]));
// 					} else {
// 						//if(not updated) D.update_parent(old_topic, D_new-D_old);
// 						//D.set_value(new_topic, reg_denom*(beta+Nw[new_topic]));
// 						if(updated) D.set_value(old_topic, D_old);
// 						else D.true_val[old_topic] = D_old;
// 					}
// 				}
// 			}
// 			if(Nw.nz_idx.size() > threshold){
// 				for(auto t: Nw.nz_idx)
// 					D.true_val[t] = beta/(Nt[t]+betabar);
// 				D.init_dense();
// 			} else  {
// 				for(auto t: Nw.nz_idx)
// 					D.set_value(t, beta/(Nt[t]+betabar));
// 			}
//
// 			/*
// 				D.true_val[t] = alpha/(Nt[t]+betabar);
// 			D.init_sparse(Nd.nz_idx.data(), Nd.nz_idx.size());
// 			*/
// 		}
// 		size_t singleiter_time = (std::clock()-start_time);
// 		total_cputime += singleiter_time;
// 		double all = (double)(nr_A+nr_B+nr_C+nr_D);
// 		printf("iter %d nr_C %.2f nr_D %.2f LL %.6g cputime %.2f iter-time %.2f init(%.2f) update(%.2f) sample(%.2f)\n",
// 				iter, (double)nr_C/all, (double)nr_D/all,
// 				compute_training_LL(), (double)total_cputime/CLOCKS_PER_SEC,
// 				(double)singleiter_time/CLOCKS_PER_SEC,
// 				D.get_init_time(),  D.get_update_time(), D.get_sample_time());
// 		fflush(stdout);
// 	}
// } // }}}
// // Normal sampling doc-by-doc
// void run_lda(lda_smat_t &training, lda_smat_t &test, int k, int iters, double alpha, double beta){ //{{{
// 	typedef std::vector<unsigned> vec_t;
// 	typedef std::vector<vec_t> mat_t;
// 	vec_t Nt(k, 0);
// 	mat_t Nwt(training.nr_words, vec_t(k));
// 	mat_t Ndt(training.nr_docs, vec_t(k));
// 	training.initialize_Z_docwise(k);
//
// 	for(auto d = 0U; d <training.nr_docs; d++) {
// 		for(auto idx = training.doc_ptr[d]; idx != training.doc_ptr[d+1]; idx++) {
// 			auto w = training.word_idx[idx];
// 			for(auto zidx = training.Z_ptr[idx]; zidx != training.Z_ptr[idx+1]; zidx++) {
// 				auto t = training.Zval[zidx];
// 				Nt[t]++;
// 				Ndt[d][t]++;
// 				Nwt[w][t]++;
// 			}
// 		}
// 	}
//
// 	double alphabar = alpha*k;
// 	double betabar = beta*training.nr_words;
// 	auto compute_training_LL = [&]()->double { // {{{
// 		double LL = 0;
// 		for(auto doc = 0U; doc < training.nr_docs; doc++) {
// 			auto &Nd = Ndt[doc];
// 			size_t sum_Nd = 0;
// 			for(auto t = 0; t < k; t++) if(Nd[t]){
// 				LL += lgamma(alpha+Nd[t])-lgamma(alpha);
// 				sum_Nd += Nd[t];
// 			}
// 			LL += lgamma(alphabar) - lgamma(alphabar+sum_Nd);
// 		}
// 		size_t nonZeroTypeTopics = 0;
// 		for(auto word = 0U; word < training.nr_words; word++) {
// 			auto &Nw = Nwt[word];
// 			for(auto t = 0; t < k; t++) if(Nw[t]){
// 				nonZeroTypeTopics++;
// 				LL += lgamma(beta+Nw[t]);
// 			}
// 		}
// 		size_t valid_topics = 0;
// 		for(auto t = 0; t < k; t++) if(Nt[t] != 0) {
// 			LL -= lgamma(betabar+Nt[t]);
// 			valid_topics++;
// 		}
// 		LL += valid_topics*lgamma(betabar)-nonZeroTypeTopics*lgamma(beta);
// 		return LL;
// 	}; // }}}
// 	std::vector<double> params(k);
// 	auto sampler = [&](double sum)->int {
// 		double urnd = drand48()*sum; int t = 0;
// 		while(1) {
// 			urnd -= params[t];
// 			if(urnd < 0) break;
// 			t++;
// 		}
// 		return t;
// 	};
//
// 	printf("init LL %.6g\n", compute_training_LL());
// 	size_t total_cputime = 0;
// 	for(int iter = 1; iter <= iters; iter++) {
// 		std::clock_t start_time = clock();
// 		for(auto d = 0U; d < training.nr_docs; d++) {
// 			auto &Nd = Ndt[d];
// 			for(auto idx = training.doc_ptr[d]; idx != training.doc_ptr[d+1]; idx++) {
// 				auto w = training.word_idx[idx];
// 				auto &Nw = Nwt[w];
// 				for(auto Zidx = training.Z_ptr[idx]; Zidx != training.Z_ptr[idx+1]; Zidx++) {
// 					size_t old_topic = training.Zval[Zidx];
// 					Nw[old_topic]--;
// 					Nd[old_topic]--;
// 					Nt[old_topic]--;
// 					double sum = 0;
// 					for(auto t = 0; t < k; t++) {
// 						params[t] = (alpha+Nd[t])*(beta+Nw[t])/(betabar+Nt[t]);
// 						sum += params[t];
// 					}
// 					size_t new_topic = sampler(sum);
// 					training.Zval[Zidx] = new_topic;
// 					Nw[new_topic]++;
// 					Nd[new_topic]++;
// 					Nt[new_topic]++;
// 				}
// 			}
// 		}
// 		size_t singleiter_time = std::clock()-start_time;
// 		total_cputime += singleiter_time;
// 		printf("iter %d LL %.6g cputime %.2f iter-time %.2f\n", iter, compute_training_LL(), (double)total_cputime/CLOCKS_PER_SEC, (double)singleiter_time/CLOCKS_PER_SEC);
// 		fflush(stdout);
// 	}
// } // }}}
// // Normal sampling word-by-word
// void run_lda_word(lda_smat_t &training, lda_smat_t &test, int k, int iters, double alpha, double beta){ //{{{
// 	typedef std::vector<size_t> vec_t;
// 	typedef std::vector<vec_t> mat_t;
// 	vec_t Nt(k, 0);
// 	mat_t Nwt(training.nr_words, vec_t(k,0));
// 	mat_t Ndt(training.nr_docs, vec_t(k,0));
// 	training.initialize_Z_wordwise(k);
//
// 	for(auto w = 0U; w < training.nr_words; w++) {
// 		for(auto idx = training.word_ptr[w]; idx != training.word_ptr[w+1]; idx++) {
// 			auto d = training.doc_idx[idx];
// 			for(auto zidx = training.Z_ptr[idx]; zidx != training.Z_ptr[idx+1]; zidx++) {
// 				auto t = training.Zval[zidx];
// 				Nt[t]++;
// 				Ndt[d][t]++;
// 				Nwt[w][t]++;
// 			}
// 		}
// 	}
//
// 	double alphabar = alpha*k;
// 	double betabar = beta*training.nr_words;
// 	auto compute_training_LL = [&]()->double { // {{{
// 		double LL = 0;
// 		for(auto doc = 0U; doc < training.nr_docs; doc++) {
// 			auto &Nd = Ndt[doc];
// 			size_t sum_Nd = 0;
// 			for(auto t = 0; t < k; t++) if(Nd[t]){
// 				LL += lgamma(alpha+Nd[t])-lgamma(alpha);
// 				sum_Nd += Nd[t];
// 			}
// 			LL += lgamma(alphabar) - lgamma(alphabar+sum_Nd);
// 		}
// 		size_t nonZeroTypeTopics = 0;
// 		for(auto word = 0U; word < training.nr_words; word++) {
// 			auto &Nw = Nwt[word];
// 			for(auto t = 0; t < k; t++) if(Nw[t]){
// 				nonZeroTypeTopics++;
// 				LL += lgamma(beta+Nw[t]);
// 			}
// 		}
// 		size_t valid_topics = 0;
// 		for(auto t = 0; t < k; t++) if(Nt[t] != 0) {
// 			LL -= lgamma(betabar+Nt[t]);
// 			valid_topics++;
// 		}
// 		LL += valid_topics*lgamma(betabar)-nonZeroTypeTopics*lgamma(beta);
// 		return LL;
// 	}; // }}}
// 	std::vector<double> params(k);
// 	auto sampler = [&](double sum)->int {
// 		double urnd = drand48()*sum; int t = 0;
// 		while(1) {
// 			urnd -= params[t];
// 			if(urnd < 0) break;
// 			t++;
// 		}
// 		return t;
// 	};
//
// 	printf("init LL %.6g\n", compute_training_LL());
// 	size_t total_cputime = 0;
// 	for(int iter = 1; iter <= iters; iter++) {
// 		std::clock_t start_time = clock();
// 		for(auto w = 0U; w < training.nr_words; w++) {
// 			auto &Nw = Nwt[w];
// 			for(auto idx = training.word_ptr[w]; idx != training.word_ptr[w+1]; idx++) {
// 				auto d = training.doc_idx[idx];
// 				auto &Nd = Ndt[d];
// 				for(auto Zidx = training.Z_ptr[idx]; Zidx != training.Z_ptr[idx+1]; Zidx++) {
// 					size_t old_topic = training.Zval[Zidx];
// 					Nw[old_topic]--;
// 					Nd[old_topic]--;
// 					Nt[old_topic]--;
// 					double sum = 0;
// 					for(auto t = 0; t < k; t++) {
// 						params[t] = (alpha+Nd[t])*(beta+Nw[t])/(betabar+Nt[t]);
// 						sum += params[t];
// 					}
// 					size_t new_topic = sampler(sum);
// 					training.Zval[Zidx] = new_topic;
// 					Nw[new_topic]++;
// 					Nd[new_topic]++;
// 					Nt[new_topic]++;
// 				}
// 			}
// 		}
// 		total_cputime += std::clock()-start_time;
// 		printf("iter %d LL %.6g cputime %.2f\n", iter, compute_training_LL(), (double)total_cputime/CLOCKS_PER_SEC);
// 		fflush(stdout);
// 	}
// } // }}}
// // Alias LDA
// struct sample_pool_t{ //  {{{
// 	std::vector<unsigned> pool;
// 	std::vector<double> qw;
// 	double Qw;
// 	size_t capacity;
// 	sample_pool_t(size_t capacity=0): capacity(capacity){
// 		pool.reserve(capacity); qw.reserve(capacity); Qw=0;
// 	}
// 	void reserve(size_t capacity=0){pool.reserve(capacity); qw.resize(capacity);}
// 	bool next_sample(unsigned &sample) {
// 		if(pool.empty()) return false;
// 		sample = pool.back(); pool.pop_back();
// 		return true;
// 	}
// 	unsigned next_sample() {
// 		double sample = pool.back(); pool.pop_back();
// 		return sample;
// 	}
// 	bool empty() {return pool.empty();}
// 	void refill(alias_table& A) {
// 		A.build_table(qw);
// 		Qw = 0; for(auto &v: qw) Qw += v;
// 		pool.clear();
// 		while(pool.size() < capacity)
// 			pool.push_back(A.sample(drand48(),drand48()));
// 	}
// }; // }}}
// void run_alias_lda(lda_smat_t &training, lda_smat_t &test, int k, int iters, double alpha, double beta){ //{{{
// 	typedef std::vector<spidx_t<unsigned>> spmat_t;
// 	typedef std::vector<unsigned> vec_t;
// 	typedef std::vector<vec_t> mat_t;
// 	vec_t Nt(k, 0);
// 	mat_t Nwt(training.nr_words, vec_t(k));
// 	spmat_t Ndt(training.nr_docs, spidx_t<unsigned>(k));
// 	std::vector<sample_pool_t> sample_pools(training.nr_words, sample_pool_t(k));
// 	training.initialize_Z_docwise(k);
//
// 	for(auto d = 0U; d <training.nr_docs; d++) {
// 		for(auto idx = training.doc_ptr[d]; idx != training.doc_ptr[d+1]; idx++) {
// 			auto w = training.word_idx[idx];
// 			for(auto zidx = training.Z_ptr[idx]; zidx != training.Z_ptr[idx+1]; zidx++) {
// 				auto t = training.Zval[zidx];
// 				Nt[t]++;
// 				Ndt[d][t]++;
// 				Nwt[w][t]++;
// 			}
// 		}
// 	}
//
// 	for(auto &pool : sample_pools)
// 		pool.reserve(k);
// 	double alphabar = alpha*k;
// 	double betabar = beta*training.nr_words;
// 	auto compute_training_LL = [&]()->double { // {{{
// 		double LL = 0;
// 		for(auto doc = 0U; doc < training.nr_docs; doc++) {
// 			auto &Nd = Ndt[doc];
// 			size_t sum_Nd = 0;
// 			for(auto t = 0; t < k; t++) if(Nd[t]){
// 				LL += lgamma(alpha+Nd[t])-lgamma(alpha);
// 				sum_Nd += Nd[t];
// 			}
// 			LL += lgamma(alphabar) - lgamma(alphabar+sum_Nd);
// 		}
// 		size_t nonZeroTypeTopics = 0;
// 		for(auto word = 0U; word < training.nr_words; word++) {
// 			auto &Nw = Nwt[word];
// 			for(auto t = 0; t < k; t++) if(Nw[t]){
// 				nonZeroTypeTopics++;
// 				LL += lgamma(beta+Nw[t]);
// 			}
// 		}
// 		size_t valid_topics = 0;
// 		for(auto t = 0; t < k; t++) if(Nt[t] != 0) {
// 			LL -= lgamma(betabar+Nt[t]);
// 			valid_topics++;
// 		}
// 		LL += valid_topics*lgamma(betabar)-nonZeroTypeTopics*lgamma(beta);
// 		return LL;
// 	}; // }}}
// 	std::vector<double> params(k);
// 	auto sampler = [&](double sum)->int { // {{{
// 		double urnd = drand48()*sum; int t = 0;
// 		while(1) {
// 			urnd -= params[t];
// 			if(urnd < 0) break;
// 			t++;
// 		}
// 		return t;
// 	}; // }}}
//
// 	for(auto &Nd: Ndt) Nd.gen_nz_idx();
//
// 	std::vector<unsigned> pdw_idx(k);
// 	std::vector<double> pdw(k), pdw_real(k), qwqw(k);
// 	alias_table pdw_sampler(k), A(k);
// 	alias_table qw_sampler(k);
//
// 	printf("init LL %.6g\n", compute_training_LL());
// 	size_t nr_MH_steps = 2;
// 	size_t total_cputime = 0;
// 	for(int iter = 1; iter <= iters; iter++) {
// 		size_t nr_p = 0, nr_q = 0;
// 		size_t total_MH_steps = 0, rejected_MH_steps = 0;
// 		std::clock_t start_time = clock();
// 		for(auto d = 0U; d < training.nr_docs; d++) {
// 			auto &Nd = Ndt[d];
// 			for(auto idx = training.doc_ptr[d]; idx != training.doc_ptr[d+1]; idx++) {
// 				auto w = training.word_idx[idx];
// 				auto &Nw = Nwt[w];
// 				auto &pool = sample_pools[w];
// 				for(auto Zidx = training.Z_ptr[idx]; Zidx != training.Z_ptr[idx+1]; Zidx++) {
// 					size_t old_topic = training.Zval[Zidx];
// 					Nw[old_topic]--;
// 					Nd[old_topic]--;
// 					Nt[old_topic]--;
// 					size_t new_topic = 0;
//
// 					if(1) {
// 						// construct Pdw, pdw, pdw_idx {{{
// 						double Pdw = 0.0;
// 						size_t nz_p = 0;
// 						size_t* ptr_old_topic = NULL;
// 						for(auto &t: Nd.nz_idx) {
// 							if(Nd[t]>0) {
// 								pdw_idx[nz_p] = t;
// 								pdw[nz_p] = Nd[t]*(Nw[t]+beta)/(Nt[t]+betabar);
// 								Pdw += pdw[nz_p];
// 								pdw_real[t] = pdw[nz_p];
// 								nz_p++;
// 							} else
// 								ptr_old_topic = &t;
// 						}
// 						if(ptr_old_topic)
// 							Nd.remove_ptr(ptr_old_topic);
// 						auto get_pdw_real = [&](size_t t)->double { // {{{
// 							auto begin = Nd.nz_idx.begin();
// 							auto end = Nd.nz_idx.end();
// 							auto it = std::lower_bound(begin,end, t);
// 							if(it != end)
// 								return pdw[it-begin];
// 							else
// 								return 0.0;
// 						}; // }}}
// 						//}}}
// 						pdw_sampler.build_table(nz_p, pdw.data());
//
// 						// Perform MH steps {{{
// 						auto &qw = pool.qw;
// 						double &Qw = pool.Qw;
// 						int s = old_topic;
// 						double p_s = (Nw[s]+beta)*(Nd[s]+alpha)/(Nt[s]+betabar);;
// 						total_MH_steps += nr_MH_steps;
// 						for(auto step = 1; step <= nr_MH_steps; step++) {
// 							if(pool.empty()) {
// 								qw.resize(k);
// 								for(size_t t = 0U; t < k; t++) {
// 									qw[t] = alpha*(Nw[t]+beta)/(Nt[t]+betabar);
// 								}
// 								pool.refill(A);
// 							}
// 							size_t t;
// 							double rnd = drand48()*(Pdw+Qw);
// 							if(rnd <= Pdw)
// 								t = pdw_idx[pdw_sampler.sample(rnd/Pdw, drand48())];
// 								//t = Nd.nz_idx[pdw_sampler.sample(rnd/Pdw, drand48())];
// 							else
// 								t = pool.next_sample();
// 							double p_t = (Nw[t]+beta)*(Nd[t]+alpha)/(Nt[t]+betabar);
// 							if(step == 0){
// 								s = t;
// 								p_s = (Nw[s]+beta)*(Nd[s]+alpha)/(Nt[s]+betabar);
// 								continue;
// 							}
// 							double pi = (double)(p_t*(pdw_real[s]+qw[s]))/(double)(p_s*(pdw_real[t]+qw[t]));
// 							//double pi = (double)(p_t*(get_pdw_real(s)+qw[s]))/(double)(p_s*(get_pdw_real(t)+qw[t]));
// 							if(drand48() <= pi) {
// 								p_s = p_t;
// 								s = t;
// 							} else {
// 								rejected_MH_steps++;
// 							}
// 						}
// 						new_topic = s;
// 						// }}}
// 						for(auto &t: Nd.nz_idx) pdw_real[t] = 0;
// 					}
// 					else if (0){  // {{{
// 						double sum = 0;
// 						for(auto t = 0; t < k; t++)
// 							params[t] = sum+=(alpha+Nd[t])*(beta+Nw[t])/(betabar+Nt[t]);
// 						new_topic = std::upper_bound(params.begin(), params.end(), sum*drand48()) - params.begin();
// 					} else if (0) {
// 						auto &qw = qwqw;
// 						double sum = 0;
// 						for(auto t = 0; t < k; t++) {
// 							qw[t] = (alpha+Nd[t])*(beta+Nw[t])/(betabar+Nt[t]);
// 							sum += qw[t];
// 						}
// 						qw_sampler.build_table(qw);
// 						new_topic = qw_sampler.sample(drand48(), drand48());
// 						//new_topic = std::upper_bound(qw.begin(), qw.end(), sum*drand48()) - qw.begin();
// 					} else if (0) {
// 						auto &qw = qwqw;
// 						double Qw = 0;
// 						double Pdw = 0;
// 						qw.resize(k);
// 						for(size_t t = 0U; t < k; t++) {
// 							qw[t] = alpha*(Nw[t]+beta)/(Nt[t]+betabar);
// 							pdw[t] = Nd[t]*(Nw[t]+beta)/(Nt[t]+betabar);
// 							Qw += qw[t];
// 							Pdw += pdw[t];
// 						}
// 						qw_sampler.build_table(qw);
// 						pdw_sampler.build_table(pdw);
// 						double rnd = drand48()*(Pdw+Qw);
// 						if(rnd <= Pdw) {
// 							rnd = rnd/Pdw;
// 							//new_topic = pdw_idx[pdw_sampler.sample(rnd, drand48())];
// 							new_topic = pdw_sampler.sample(rnd, drand48());
// 							nr_p++;
// 						} else {
// 							new_topic = qw_sampler.sample((rnd-Pdw)/Qw, drand48());
// 							nr_q++;
// 						}
// 					} else if (0) {
//
// 						// construct Pdw, pdw, pdw_idx {{{
// 						double Pdw = 0.0;
// 						size_t nz_p = 0;
// 						size_t* ptr_old_topic = NULL;
// 						for(auto &t: Nd.nz_idx) {
// 							if(Nd[t]>0) {
// 								pdw_idx[nz_p] = t;
// 								pdw[nz_p] = Nd[t]*(Nw[t]+beta)/(Nt[t]+betabar);
// 								Pdw += pdw[nz_p];
// 								pdw_real[t] = pdw[nz_p];
// 								nz_p++;
// 							} else
// 								ptr_old_topic = &t;
// 						}
// 						if(ptr_old_topic)
// 							Nd.remove_ptr(ptr_old_topic);
// 						//}}}
// 						pdw_sampler.build_table(nz_p, pdw.data());
//
// 						auto &qw = qwqw;
// 						double Qw = 0;
// 						qw.resize(k);
// 						for(size_t t = 0U; t < k; t++) {
// 							qw[t] = alpha*(Nw[t]+beta)/(Nt[t]+betabar);
// 							Qw += qw[t];
// 						}
// 						qw_sampler.build_table(qw);
// 						double rnd = drand48()*(Pdw+Qw);
// 						if(rnd <= Pdw) {
// 							new_topic = pdw_idx[pdw_sampler.sample(rnd/Pdw, drand48())];
// 							nr_p++;
// 						} else {
// 							new_topic = qw_sampler.sample((rnd-Pdw)/Qw, drand48());
// 							nr_q++;
// 						}
// 					} else if(0) {
// 						// construct Pdw, pdw, pdw_idx {{{
// 						double Pdw = 0.0;
// 						size_t nz_p = 0;
// 						size_t* ptr_old_topic = NULL;
// 						for(auto &t: Nd.nz_idx) {
// 							if(Nd[t]>0) {
// 								pdw_idx[nz_p] = t;
// 								pdw[nz_p] = Nd[t]*(Nw[t]+beta)/(Nt[t]+betabar);
// 								Pdw += pdw[nz_p];
// 								pdw_real[t] = pdw[nz_p];
// 								nz_p++;
// 							} else
// 								ptr_old_topic = &t;
// 						}
// 						if(ptr_old_topic)
// 							Nd.remove_ptr(ptr_old_topic);
// 						//}}}
// 						pdw_sampler.build_table(nz_p, pdw.data());
//
// 						auto &qw = pool.qw;
// 						double &Qw = pool.Qw;
//
// 						qw.resize(k);
// 						Qw = 0;
// 						for(size_t t = 0U; t < k; t++) {
// 							qw[t] = alpha*(Nw[t]+beta)/(Nt[t]+betabar);
// 							Qw += qw[t];
// 						}
// 						qw_sampler.build_table(qw);
//
// 						int s = old_topic;
// 						double p_s = (Nw[s]+beta)*(Nd[s]+alpha)/(Nt[s]+betabar);;
// 						for(auto step = 0; step <= nr_MH_steps; step++) {
// 							size_t t;
// 							double rnd = drand48()*(Pdw+Qw);
// 							if(rnd <= Pdw)
// 								t = pdw_idx[pdw_sampler.sample(rnd/Pdw, drand48())];
// 							else
// 								t = qw_sampler.sample((rnd-Pdw)/Qw, drand48());
// 							double p_t = (Nw[t]+beta)*(Nd[t]+alpha)/(Nt[t]+betabar);
// 							if(step == 0){
// 								s = t;
// 								p_s = (Nw[s]+beta)*(Nd[s]+alpha)/(Nt[s]+betabar);
// 								continue;
// 							}
// 							double pi = (double)(p_t*(pdw_real[s]+qw[s]))/(double)(p_s*(pdw_real[t]+qw[t]));
// 							if(drand48() <= pi) {
// 								p_s = p_t;
// 								s = t;
// 							} else {
// 								rejected_MH_steps++;
// 							}
// 						}
// 						for(auto &t: Nd.nz_idx)
// 							pdw_real[t] = 0;
// 						new_topic = s;
//
// 					} // }}}
// 					training.Zval[Zidx] = new_topic;
// 					Nw[new_topic]++;
// 					Nd[new_topic]++;
// 					Nt[new_topic]++;
// 					if(Nd[new_topic]==1) Nd.push(new_topic);
// 				}
// 			}
// 		}
// 		size_t singleiter_time = std::clock()-start_time;
// 		total_cputime += singleiter_time;
// 		double ac_rate = (double) (total_MH_steps-rejected_MH_steps)/(total_MH_steps);
// 		printf("iter %d LL %.6g cputime %.2f iter-time %.2f ac_rate %.2f\n", iter, compute_training_LL(),
// 				(double)total_cputime/CLOCKS_PER_SEC,
// 				(double)singleiter_time/CLOCKS_PER_SEC,
// 				ac_rate);
// 		fflush(stdout);
// 	}
// } // }}}


// 		puts("solver: 0 = Normal LDA");
// 		puts("solver: 1 = Sparse LDA");
// 		puts("solver: 8 = Alias LDA");
// 		puts("solver: 9 = F+LDA - word-by-word");
// 		puts("solver: 10 = F+LDA - doc-by-doc");

// [[Rcpp::export]]
Rcpp::List ftree_lda(int n_topic, double alpha, double eta, int n_iter, std::string data_dir, int solver) {
  	int k = n_topic;
  	int iters = n_iter;
  	const char *src = data_dir.c_str();
  	double beta = eta;
  	//double alpha = 0.1, beta = 0.1;
  	// srand(time(NULL));
  	// srand48(time(NULL));

  	lda_smat_t training_set, test_set;
  	//lda_read_data(src, training_set, test_set, 1, smat_t::PETSc);
  	lda_read_data(src, training_set, test_set, 1, smat_t::TXT);
  	Rprintf("read done \n");
  	return run_splda(training_set, test_set, k, iters, alpha, beta);
}

// 		switch (solver) {
// 			case 0 :
// 				run_lda(training_set, test_set, k, iters, alpha, beta);
// 				break;
// 			case 1 :
// 				run_splda(training_set, test_set, k, iters, alpha, beta);
// 				break;
// 			case 8 :
// 				run_alias_lda(training_set, test_set, k, iters, alpha, beta);
// 				break;
// 			case 9 :
// 				run_splda_fast_2_word_test(training_set, test_set, k, iters, alpha, beta);
// 				break;
// 			case 10 :
// 				run_splda_fast_2_test(training_set, test_set, k, iters, alpha, beta);
// 				break;
// 		}
//}
// int main(int argc, char *argv[]) {
// 	//test(); return 0;
// 	if(argc != 5) {
// 		puts("[Usage]");
// 		puts(" $ ./splda nr_topics max_iterations data_dir solver");
// 		puts("solver: 0 = Normal LDA");
// 		puts("solver: 1 = Sparse LDA");
// 		puts("solver: 8 = Alias LDA");
// 		puts("solver: 9 = F+LDA - word-by-word");
// 		puts("solver: 10 = F+LDA - doc-by-doc");
// 		return -1;
// 	}
// 	int k = atoi(argv[1]);
// 	int iters = atoi(argv[2]);
// 	char *src = argv[3];
// 	int solver = atoi(argv[4]);
// 	double alpha = 50.0/k, beta = 0.01;
// 	//double alpha = 0.1, beta = 0.1;
// 	// srand(time(NULL));
// 	// srand48(time(NULL));
//
// 	lda_smat_t training_set, test_set;
// 	lda_read_data(src, training_set, test_set, 1, smat_t::PETSc);
// 	switch (solver) {
// 		case 0 :
// 			run_lda(training_set, test_set, k, iters, alpha, beta);
// 			break;
// 		case 1 :
// 			run_splda(training_set, test_set, k, iters, alpha, beta);
// 			break;
// 		case 8 :
// 			run_alias_lda(training_set, test_set, k, iters, alpha, beta);
// 			break;
// 		case 9 :
// 			run_splda_fast_2_word_test(training_set, test_set, k, iters, alpha, beta);
// 			break;
// 		case 10 :
// 			run_splda_fast_2_test(training_set, test_set, k, iters, alpha, beta);
// 			break;
// 	}
//
// 	return 0;
// }
