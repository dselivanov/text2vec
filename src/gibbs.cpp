// Code below is based on code from R lda package: https://github.com/slycoder/R-lda-deprecated
// All credits to Jonathan Chang - https://github.com/slycoder
// Original code lda is under LGPL license

#include "text2vec.h"
using namespace Rcpp;

IntegerMatrix subtract_matrices(IntegerMatrix m1, IntegerMatrix m2) {
  IntegerMatrix res(m1.nrow(), m1.ncol());
  for(size_t i = 0; i < m1.length(); i++ )
    res[i] = m1[i] - m2[i];
  return(res);
}

double docs_likelihood(IntegerMatrix M, double prior) {
  double ll = 0;
  int n = M.nrow();
  for (size_t i_doc = 0; i_doc < M.ncol(); ++i_doc) {
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
  size_t i_doc;
  int kk;

  int n_documents = documents.size();

  IntegerMatrix document_topic_distr;
  List assignements;
  std::vector<double> log_likelihood;

  IntegerMatrix topics_word_distr;
  IntegerVector topic_sums;
  IntegerMatrix initial_topics_word_distr;
  IntegerVector initial_topic_sums;
  IntegerMatrix initial_document_topic_distr;
  int have_initial_word_topic_distr = (initial.size() > 0);
  int have_initial_document_topic_distr = 0;
  int have_initial_assignements = 0;
  if(initial.containsElementNamed("document_topic_distr")) have_initial_document_topic_distr = 1;
  if(initial.containsElementNamed("assignements")) have_initial_assignements = 1;

  // List initial_assignements(n_documents);

//   IntegerMatrix document_expects;
//   if (burnin > -1) {
//     IntegerMatrix document_expects(n_topics, n_documents);
//   }
  if (have_initial_word_topic_distr) {
    initial_topics_word_distr = as<IntegerMatrix>(initial["topics_word_distr"]);
    initial_topic_sums = as<IntegerVector>(initial["topic_sums"]);
    // make deep copies, since we will modify by reference below
    topics_word_distr = clone(initial_topics_word_distr);
    topic_sums = clone(initial_topic_sums);

    if(have_initial_document_topic_distr) {
      initial_document_topic_distr = as<IntegerMatrix>(initial["document_topic_distr"]);
      // make deep copies, since we will modify by reference below
      document_topic_distr = clone(initial_document_topic_distr);
    } else {
      document_topic_distr = IntegerMatrix(n_topics, n_documents);
    }
    if(have_initial_assignements)
      assignements = initial["assignements"];
    else {
      assignements = List(n_documents);
      for(i_doc = 0; i_doc < n_documents; i_doc++)
        assignements[i_doc] = IntegerVector(documents[i_doc].ncol(), -1);
    }

  } else {
    topics_word_distr = IntegerMatrix(n_topics, vocab_size);
    document_topic_distr = IntegerMatrix(n_topics, n_documents);
    topic_sums = IntegerVector(n_topics);
    assignements = List(n_documents);
    for(i_doc = 0; i_doc < n_documents; i_doc++)
      assignements[i_doc] = IntegerVector(documents[i_doc].ncol(), -1);
  }

  // NumericVector p(n_topics);
  vector<double> p(n_topics);

  double const_prior = 0;
  double const_ll = 0;

  //log B(\alpha)
  const_prior = (n_topics * R::lgammafn(alpha) - R::lgammafn(alpha * n_topics)) * n_documents;
  //log B(\eta)
  const_ll = (vocab_size * R::lgammafn(eta) - R::lgammafn(eta * vocab_size)) * n_topics;

  size_t j = 0;
  for (int iteration = 1; iteration <= n_iter; ++iteration) {
    R_CheckUserInterrupt();
    for (i_doc = 0; i_doc < n_documents; ++i_doc) {
      IntegerVector zs = assignements[i_doc];
      IntegerMatrix document = documents[i_doc];
      int nw = document.ncol();
      IntegerVector initial_d;
      if (have_initial_assignements) {
        initial_d = assignements[i_doc];
      }
      for (uint32_t i_word = 0; i_word < nw; ++i_word) {
        int z = zs[i_word];
        long word = -1;
        int count = 1;
        int* topic_wk;
        int* topic_k;
        int* document_k;

        word  = document(0, i_word);
        count = document(1, i_word);
        if (z != -1) {
          topic_wk = &topics_word_distr[z + n_topics * word];
          topic_k = &topic_sums[z];

          if(!freeze_topics) {
            *topic_wk -= count;
            *topic_k -= count;
          }
          document_k = &document_topic_distr[n_topics * i_doc + z];
          *document_k -= count;

          if (*topic_wk < 0 || *topic_k < 0 || *document_k < 0) {
            stop("Counts became negative for word (%ld): (%d, %d, %d)",
                  word, *topic_wk, *topic_k, *document_k);
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
      double doc_ll;
      if(have_initial_document_topic_distr)
        doc_ll = docs_likelihood(subtract_matrices(document_topic_distr, initial_document_topic_distr), alpha);
      else
        doc_ll = docs_likelihood(document_topic_distr, alpha);
        // IntegerMatrix temp = initial_document_topic_distr - document_topic_distr;
        // doc_ll = docs_likelihood(initial_document_topic_distr - document_topic_distr, alpha);

      double topic_ll;
      if(have_initial_word_topic_distr)
        topic_ll = topics_likelihood(subtract_matrices(topics_word_distr, initial_topics_word_distr), eta);
      else
        topic_ll = topics_likelihood(topics_word_distr, eta);

      log_likelihood.push_back(doc_ll - const_prior + topic_ll - const_ll);
      if (trace >= 1) {
        Rprintf("\r%s iteration %d, likelihood: %0.2f\n",
                currentDateTime().c_str(),
                iteration,
                log_likelihood[j]
        );
        R_FlushConsole();
      }

      // check convergence
      if(j > 0) {
        double ll_improvement =  log_likelihood[j - 1] / log_likelihood[j] - 1;
        if(ll_improvement < convergence_tol) {
          Rprintf("Early stopping on %d iteradion. Likelihood improve for last iteration was %0.2f%%\n",
                  iteration, ll_improvement * 100);
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
                      _["log_likelihood"] = wrap(log_likelihood),
                      _["assignements"] = assignements);

}
