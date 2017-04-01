// Copyright (C) 2015 - 2017  Dmitriy Selivanov
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


#include "mcemlda/LDA.hpp"
#include <Rcpp.h>
using namespace Rcpp;

//extend base LDA class with R related methods
class R_LDA:public LDA {
public:

  void init(const IntegerVector &z_old, const IntegerVector &z_new) {

    rng.seed((uint64_t)R::runif(1, pow(2.0, 32)), (uint64_t)R::runif(1, pow(2.0, 32)) );

    C_doc.resize(corpus.nrow(), n_topic);
    C_word.resize(corpus.ncol(), n_topic);
    C_all.resize(n_topic);
    C_all_local.resize(n_topic, 0);
    size_t j = 0;
    corpus.apply([&](Z& z, doc_index_t d, word_index_t w) {
      z.old_z = z_old[j];
      z.new_z = z_new[j];
      j++;
    });
    // Update C_doc & C_all
    C_doc.clear();
    corpus.apply([&](Z& z, doc_index_t d, word_index_t w) {
      C_doc.at(d, z.old_z) ++;
      C_all[z.old_z] ++;
      // C_all_local[z.old_z] ++;
    });
    // Update C_word
    C_word.clear();
    corpus.apply<true>([&](Z& z, doc_index_t d, word_index_t w) {
      C_word.at(w, z.old_z) ++;
    });
  }

  void set_topic_word_count(const IntegerMatrix &topic_word_count) {
    C_word.resize(topic_word_count.ncol(), topic_word_count.nrow());
    // Update C_word
    C_word.clear();
    for(word_index_t w = 0; w < C_word.nrow(); w++)
      for(topic_index_t t = 0; t < C_word.ncol(); t++)
        //topic_word_count matrix is n_topics * n_words, so indices "t", "w" swapped
        C_word.at(w, t) = topic_word_count(t, w);

  }

  // read in document-term matrix CSR format
  void r_read_corpus(const S4 &m_csr) {
    IntegerVector dims = m_csr.slot("Dim");
    int nr = dims[0];

    IntegerVector PP = m_csr.slot("p");
    int *P = PP.begin();
    IntegerVector JJ = m_csr.slot("j");
    int *J = JJ.begin();
    NumericVector XX = m_csr.slot("x");
    double *X = XX.begin();

    for(int r=0; r < nr; r++) {
      size_t p1 = P[r];
      size_t p2 = P[r + 1];
      for(int j=p1; j < p2; j++) {
        unsigned idx = J[j];
        unsigned cnt = X[j];
        for(int k=0; k<cnt; k++)
          corpus.append(r,idx,0,0);
      }
    }
    corpus.build_CSC_from_CSR();
  }
  IntegerMatrix get_topic_word_count() {
    // size(C_word)  =  n_words * n_topics
    // we need n_topics * n_words
    int n_words = C_word.nrow();
    int n_topics = C_word.ncol();
    IntegerMatrix topic_word_count(n_topics, n_words);

    for(word_index_t w = 0; w < n_words; w++)
      for(topic_index_t t = 0; t < n_topics; t++)
        // topics and words swapped - we return matrix of size n_topics * n_words
        topic_word_count(t, w) = C_word.at(w, t);

    return topic_word_count;
  }

  IntegerMatrix get_doc_topic_count() {

    IntegerMatrix doc_topic_count(C_doc.nrow(), C_doc.ncol());

    for(doc_index_t d = 0; d < C_doc.nrow(); d++)
      for(topic_index_t t = 0; t < C_doc.ncol(); t++)
        doc_topic_count(d, t) = C_doc.at(d, t);

    return doc_topic_count;
  }

  IntegerVector get_c_all_local() {
    return wrap(C_all_local);
  }

  IntegerVector get_c_all() {
    return wrap(C_all);
  }

  void set_c_all(const IntegerVector &r_c_all) {
    for(int i = 0; i < r_c_all.size(); i++)
      C_all[i] = r_c_all[i];
  }

  void reset_c_all_local() {
    for(int i = 0; i < C_all_local.size(); i++)
      C_all_local[i] = 0;
  }
};

//----------------------------------------------------------------------------------------
// R exports
//----------------------------------------------------------------------------------------

// [[Rcpp::export]]
SEXP warplda_create(int n_topics, double doc_topic_prior, double topic_word_prior) {
  R_LDA *lda_model = new R_LDA();
  lda_model->set_n_topic(n_topics);
  lda_model->set_alpha(doc_topic_prior);
  lda_model->set_beta(topic_word_prior);
  XPtr< R_LDA> ptr(lda_model, true);
  return ptr;
}

// [[Rcpp::export]]
void warplda_init_dtm(SEXP ptr, const S4 &m, const IntegerVector &z_old, const IntegerVector &z_new) {
  Rcpp::XPtr<R_LDA> lda_model(ptr);
  lda_model->r_read_corpus(m);
  lda_model->init(z_old, z_new);
}

// [[Rcpp::export]]
void warplda_set_topic_word_count(SEXP ptr, const Rcpp::IntegerMatrix  &topic_word_count) {
  Rcpp::XPtr<R_LDA> lda_model(ptr);
  lda_model->set_topic_word_count(topic_word_count);
}

// [[Rcpp::export]]
double run_one_iter(SEXP ptr, bool update_topics = true, bool calc_ll = true) {
  Rcpp::XPtr<R_LDA> lda_model(ptr);
  lda_model->sample_by_doc(update_topics);
  lda_model->sample_by_word(update_topics);
  if(calc_ll)
    return lda_model->pseudo_loglikelihood();
  else
    return 0.0;
}

// [[Rcpp::export]]
IntegerMatrix warplda_get_doc_topic_count(SEXP ptr) {
  Rcpp::XPtr<R_LDA> lda_model(ptr);
  return lda_model->get_doc_topic_count();
}

// [[Rcpp::export]]
IntegerMatrix warplda_get_topic_word_count(SEXP ptr) {
  Rcpp::XPtr<R_LDA> lda_model(ptr);
  return lda_model->get_topic_word_count();
}

// [[Rcpp::export]]
IntegerVector warplda_get_c_all_local(SEXP ptr) {
  Rcpp::XPtr<R_LDA> lda_model(ptr);
  return lda_model->get_c_all_local();
}

// [[Rcpp::export]]
IntegerVector warplda_get_c_all(SEXP ptr) {
  Rcpp::XPtr<R_LDA> lda_model(ptr);
  return lda_model->get_c_all();
}

// [[Rcpp::export]]
void warplda_set_c_all(SEXP ptr, const IntegerVector &c_all) {
  Rcpp::XPtr<R_LDA> lda_model(ptr);
  lda_model->set_c_all(c_all);
}

// [[Rcpp::export]]
void warplda_reset_c_all_local(SEXP ptr) {
  Rcpp::XPtr<R_LDA> lda_model(ptr);
  lda_model->reset_c_all_local();
}
