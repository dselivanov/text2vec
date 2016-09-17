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

// Code below is based on code from R lda package: https://github.com/slycoder/R-lda-deprecated
// All credits should go to Jonathan Chang - https://github.com/slycoder
// Original code is under LGPL license
#include "text2vec.h"
using namespace Rcpp;

// IntegerMatrix subtract_matrices(IntegerMatrix m1, IntegerMatrix m2) {
//   IntegerMatrix res(m1.nrow(), m1.ncol());
//   for(uint32_t i = 0; i < m1.length(); i++ )
//     res[i] = m1[i] - m2[i];
//   return(res);
// }

double docs_likelihood(IntegerMatrix M, double prior) {
  double ll = 0;
  int n = M.nrow();
  for (uint32_t i_doc = 0; i_doc < M.ncol(); ++i_doc) {
    double sum = prior * n;
    for (int kk = 0; kk < n; ++kk) {
      ll += R::lgammafn(M[n * i_doc + kk] + prior);
      sum += M[n * i_doc + kk];
    }
    ll -= R::lgammafn(sum);
  }
  return ll;
}

double topics_likelihood(IntegerMatrix M, double prior) {
  double ll = 0;
  int n_topics = M.nrow();
  int vocab_size = M.ncol();
  for (int kk = 0; kk < n_topics; ++kk) {
    double sum = prior * vocab_size;
    for (int ii = 0; ii < vocab_size; ++ii) {
      ll += R::lgammafn(M[kk + n_topics * ii] + prior);
      sum += M[kk + n_topics * ii];
    }
    ll -= R::lgammafn(sum);
  }
  return ll;
}

// [[Rcpp::export]]
double total_likelihood(IntegerMatrix topic_distr, IntegerMatrix doc_distr,
                        double topic_prior, double doc_prior) {
  int n_topics = topic_distr.nrow();
  int vocab_size = topic_distr.ncol();
  int n_documents = doc_distr.ncol();
  //log B(\alpha)
  double doc_prior_ll = (n_topics * R::lgammafn(doc_prior) - R::lgammafn(doc_prior * n_topics)) * n_documents;
  //log B(\eta)
  double topic_prior_ll = (vocab_size * R::lgammafn(topic_prior) - R::lgammafn(topic_prior * vocab_size)) * n_topics;
  double topic_ll = topics_likelihood(topic_distr, topic_prior);
  double doc_ll = docs_likelihood(doc_distr, doc_prior);
  return(doc_ll - doc_prior_ll + topic_ll - topic_prior_ll);
}

// [[Rcpp::export]]
List collapsedGibbsSampler(ListOf<IntegerMatrix> documents,
                           int n_topics,
                           int vocab_size,
                           int n_iter,
                           double alpha,
                           double eta,
                           List initial,
                           double convergence_tol = 0.005,
                           int check_convergence_every_n = 0,
                           int trace = 2,
                           int freeze_topics = 0) {
  // int burnin = -1,
  GetRNGstate();
  uint32_t i_doc;
  int kk;
  uint32_t n_documents = documents.size();

  double doc_ll;
  double topic_ll;

  //log B(\alpha)
  const double const_prior = (n_topics * R::lgammafn(alpha) - R::lgammafn(alpha * n_topics)) * n_documents;
  //log B(\eta)
  const  double const_ll = (vocab_size * R::lgammafn(eta) - R::lgammafn(eta * vocab_size)) * n_topics;


  uint32_t total_words = 0;
  for(uint32_t i = 0; i < n_documents; i++ )
    total_words += sum(documents[i].row(1));
  uint32_t initial_topic_total_words = total_words;
  // Rprintf("total_words = %d\n", total_words);

  IntegerMatrix document_topic_distr;
  List assignements;
  std::vector<double> log_likelihood;
  std::vector<double> perpl;

  IntegerMatrix topics_word_distr;
  IntegerVector topic_sums;
  IntegerMatrix initial_document_topic_distr;
  int have_initial_word_topic_distr =
    initial.containsElementNamed("topics_word_distr") &&
    initial.containsElementNamed("topic_sums");
  int have_initial_assignements = 0;

//   IntegerMatrix document_expects;
//   if (burnin > -1) {
//     IntegerMatrix document_expects(n_topics, n_documents);
//   }

  if (have_initial_word_topic_distr) {
    topics_word_distr = as<IntegerMatrix>(initial["topics_word_distr"]);
    topic_sums = as<IntegerVector>(initial["topic_sums"]);
    initial_topic_total_words = std::accumulate(topic_sums.begin(), topic_sums.end(), 0);
    topic_ll = topics_likelihood(topics_word_distr, eta) - const_ll;;

  } else {
    topics_word_distr = IntegerMatrix(n_topics, vocab_size);
    topic_sums = IntegerVector(n_topics);
  }
  document_topic_distr = IntegerMatrix(n_topics, n_documents);
  assignements = List(n_documents);
  for(i_doc = 0; i_doc < n_documents; i_doc++)
    assignements[i_doc] = IntegerVector(documents[i_doc].ncol(), -1);

  vector<double> p(n_topics);

  size_t j = 0;
  for (int iteration = 1; iteration <= n_iter; ++iteration) {
    R_CheckUserInterrupt();
    for (i_doc = 0; i_doc < n_documents; ++i_doc) {
      IntegerVector zs = assignements[i_doc];
      IntegerMatrix document = documents[i_doc];
      uint32_t nw = document.ncol();
      IntegerVector initial_d;
      if (have_initial_assignements) {
        initial_d = assignements[i_doc];
      }
      for (uint32_t i_word = 0; i_word < nw; ++i_word) {
        int z = zs[i_word];
        long word = document(0, i_word);
        int count = document(1, i_word);
        long i_wk = z + n_topics * word;
        long i_dk = n_topics * i_doc + z;

        if (z != -1) {
          if(!freeze_topics) {
            topics_word_distr[i_wk] -= count;
            topic_sums[z] -= count;
          }
          document_topic_distr[i_dk] -= count;

          if (topics_word_distr[i_wk] < 0 || topic_sums[z] < 0 || document_topic_distr[i_dk] < 0) {
            stop("Counts became negative for word (%ld): (%d, %d, %d)",
                  word, topics_word_distr[i_wk], topic_sums[z], document_topic_distr[i_dk]);
          }
        }

        double p_sum = 0.0;
        for (kk = 0; kk < n_topics; ++kk) {
          if (z == -1) {
            if (have_initial_assignements) {
              if (initial_d[i_word] == kk) {
                p[kk] = 1;
              } else {
                p[kk] = 0;
              }
            } else {
              p[kk] = 1;
            }
          } else {
            p[kk] = (document_topic_distr[n_topics * i_doc + kk] + alpha);
            p[kk] *= (topics_word_distr[kk + n_topics * word] + eta);
            p[kk] /= (topic_sums[kk] + vocab_size * eta);
          }
          p_sum += p[kk];
        }

        double r = unif_rand();
        zs[i_word] = -1;
        for (kk = 0; kk < n_topics; ++kk) {
          if (r < p[kk] / p_sum) {
            zs[i_word] = kk;
            break;
          }
          r -= p[kk] / p_sum;
        }

        if (zs[i_word] == -1) {
          for (kk = 0; kk < n_topics; ++kk) Rprintf("%g\n", p[kk]);
          stop("This should not have happened (%g).", r);
        }

        if(!freeze_topics) {
          topics_word_distr[zs[i_word] + n_topics * word] += count;
          topic_sums[zs[i_word]] += count;
        }
        document_topic_distr[n_topics * i_doc + zs[i_word]] += count;
//         if (burnin > -1 && iteration >= burnin)
//           document_expects[n_topics * i_doc + zs[i_word]] += count;
      }
    }
    //Compute the likelihoods, check convergence
    if(check_convergence_every_n > 0 && iteration % check_convergence_every_n == 0) {
      doc_ll = docs_likelihood(document_topic_distr, alpha) - const_prior;
      // calculate topics_likelihood only when we don't have it initial fixed topics_word_distr
      if(!(have_initial_word_topic_distr && freeze_topics))
        topic_ll = topics_likelihood(topics_word_distr, eta) - const_ll;

      log_likelihood.push_back(doc_ll + topic_ll );
      perpl.push_back( exp ( (-doc_ll) / total_words + (-topic_ll) / initial_topic_total_words) );
      if (trace >= 1) {
        // Rprintf("\r%s iteration %d, perplexity: %0.2f likelihood %0.2f doc_topic_ll %0.2f topic_word_ll %0.2f\n",
        //         currentDateTime().c_str(), iteration, perpl[j], log_likelihood[j],
        //         doc_ll, topic_ll);
        Rprintf("\r%s iteration %d, perplexity: %0.2f likelihood %0.2f\n",
                currentDateTime().c_str(), iteration, perpl[j], log_likelihood[j]);
        R_FlushConsole();
      }
      // check convergence
      if(j > 0) {
        double perpl_improvement =  perpl[j - 1] / perpl[j] - 1;
        if(perpl_improvement < convergence_tol) {
          Rprintf("Early stopping on %d iteradion. Perplexity improvement for last iteration was %0.2f%%\n",
                  iteration, perpl_improvement * 100);
          break;
        }
      }
      j++;
    } else {
      if (trace >= 1) {
        Rprintf("\r%s iteration %d", currentDateTime().c_str(), iteration);
        R_FlushConsole();
      }
    }
  }

  PutRNGstate();
  return List::create(_["topics_word_distr"] = topics_word_distr,
                      _["topic_sums"] = topic_sums,
                      _["document_topic_distr"] = document_topic_distr,
                      _["log_likelihood"] = wrap(log_likelihood));
                      // _["assignements"] = assignements);

}
