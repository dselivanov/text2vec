# helper functions
# ------------------------------------------
dirichlet_expectation_1d <- function(alpha) {
  digamma(alpha) - digamma(sum(alpha))
}

dirichlet_expectation_2d <- function(alpha) {
  digamma(alpha) - digamma(rowSums(alpha))
}

# calculate log-likelihood
loglikelihood <- function(prior, distr, dirichlet_distr, size) {
  score = sum((prior - distr) * dirichlet_distr) +
    sum(lgamma(distr) - lgamma(prior)) +
    sum(lgamma(prior * size) - lgamma(rowSums(distr)))
  return(score)
}

# ------------------------------------------
approx_bound <- function(dtm_minibatch,
                         doc_topic_distr,
                         topic_word_distr,
                         doc_topic_prior,
                         topic_word_prior,
                         total_samples = NULL) {

  n_features = ncol(topic_word_distr)
  n_topics = nrow(topic_word_distr)

  sub_sampling = FALSE
  if (!is.null(total_samples))
    sub_sampling = TRUE

  dirichlet_doc_topic = dirichlet_expectation_2d(doc_topic_distr)
  dirichlet_topic_word = dirichlet_expectation_2d(topic_word_distr)

  score = 0.0

  # E[log p(docs | theta, beta)]
  for (i in 1:length(dtm_minibatch)) {
    # add 1 because lda_c matrices have 0-based indices
    ids = dtm_minibatch[[i]][1, ] + 1
    cts = dtm_minibatch[[i]][2, ]
    norm_phi = numeric(length(ids))
    k <- 1
    for (id in ids) {
      temp = dirichlet_doc_topic[i, ] + topic_word_distr[, id]
      norm_phi[[k]] = log(sum(exp(temp)))
      k <- k + 1
    }
    score = score + sum(cts * norm_phi)
#     temp = dirichlet_doc_topic[i, ] + topic_word_distr[, ids]
#     norm_phi = log(sum(exp(temp)))
#     score = score + sum(cts * norm_phi)
  }

  # E[log p(theta | alpha) - log q(theta | doc_topic_distr)]
  score = score + loglikelihood(doc_topic_prior,
                                doc_topic_distr,
                                dirichlet_doc_topic,
                                n_topics)

  # Compensate for the subsampling of the population of documents
  if (sub_sampling) {
    doc_ratio = total_samples / length(dtm_minibatch)
    score = score * doc_ratio
  }

  # E[log p(beta | eta) - log q (beta | lambda)]
  score = score + loglikelihood(topic_word_prior,
                                topic_word_distr,
                                dirichlet_topic_word,
                                n_features)
  message(score)
  return(score)
}

perplexity <- function(X,
                       doc_topic_distr,
                       topic_word_distr,
                       doc_topic_prior,
                       topic_word_prior,
                       total_samples = NULL) {
  sub_sampling = FALSE
  if (!is.null(total_samples))
    sub_sampling = TRUE

  # X in lda-c format
  n_docs = length(X)
  # numer of words in X
  n_word = sum(vapply(X, function(x) sum(x[2,]), 0L))

  bound = approx_bound(X, doc_topic_distr,
                       topic_word_distr,
                       doc_topic_prior,
                       topic_word_prior,
                       total_samples)

  if (sub_sampling)
    word_cnt = n_word * (total_samples / n_docs)
  else
    word_cnt = n_word

  exp(-1.0 * (bound / word_cnt))
}
# ------------------------------------------

online_LDA <- function(n_topics,
                      vocabulary,
                      # alpha
                      doc_topic_prior = 1 / n_topics,
                      # eta
                      topic_word_prior = 1 / n_topics,
                      learning_decay = 0.7,
                      # tau
                      learning_offset = 10,
                      batch_size = 128,
                      topic_word_distr = NULL,
                      total_samples = 1e6,
                      evaluate_every = 0,
                      mean_change_tol = 1e-3,
                      max_doc_update_iter = 100,
                      verbose = FALSE) {
  # ------------------------------------------
  # init
  # ------------------------------------------
  EPS = 1e-20
  .n_topics = n_topics
  .doc_topic_prior = doc_topic_prior
  .topic_word_prior = topic_word_prior
  .learning_decay = learning_decay
  .learning_offset = learning_offset
  .max_doc_update_iter = max_doc_update_iter
  .batch_size = batch_size
  .evaluate_every = evaluate_every
  .total_samples = total_samples
  # perp_tol = perp_tol
  .mean_change_tol = mean_change_tol
  .max_doc_update_iter = max_doc_update_iter
  .verbose = verbose
  .bound = NULL
  .update_count = 0
  # ------------------------------------------
  .vocab_size = length(vocabulary)
  .topic_word_distr =
    if (is.null(topic_word_distr)) {
      matrix(rgamma(n_topics * .vocab_size, shape =  100, scale = 1 / 100),
             nrow = n_topics,
             ncol = .vocab_size,
             dimnames = list(NULL, vocabulary))
    } else topic_word_distr

  .exp_topic_word_distr = exp(dirichlet_expectation_2d(.topic_word_distr))
  # ------------------------------------------
  get_params <- function() {
    list(topic_word_distr = .topic_word_distr, bound = .bound)
  }
  # ------------------------------------------
  do_e_step <- function(dtm_minibatch, calc_suff_stat = TRUE) {
    batch_len = length(dtm_minibatch)
    doc_topic_distr = matrix(rgamma(batch_len * .n_topics, shape =  100, scale = 1 / 100),
                             nrow = batch_len,
                             ncol = .n_topics)

    # In the literature, this is `exp(E[log(theta)])`
    exp_doc_topic = exp(dirichlet_expectation_2d(doc_topic_distr))
    if (calc_suff_stat)
      suff_stats = matrix(0, nrow = nrow(.topic_word_distr), ncol = ncol(.topic_word_distr))

    # TODO: can be parallelized with foreach
    for (i in 1:length(dtm_minibatch)) {
      d = dtm_minibatch[[i]]
      # add 1 because lda_c matrices have 0-based indices
      ids = d[1, ] + 1
      counts = d[2, ]
      doc_topic_d = doc_topic_distr[i, ]
      exp_doc_topic_d = exp_doc_topic[i, , drop = F]
      exp_topic_word_d = .exp_topic_word_distr[, ids, drop = F]
      # The optimal phi_{dwk} is proportional to
      # exp(E[log(theta_{dk})]) * exp(E[log(beta_{dw})]).

      phinorm = exp_doc_topic_d %*% exp_topic_word_d + EPS
      for (it in 1:.max_doc_update_iter) {
        last_doc_topic_d = doc_topic_d
        # We represent phi implicitly to save memory and time.
        # Substituting the value of the optimal phi back into
        # the update for doc_topic_distr gives this update. Cf. Lee&Seung 2001.
        doc_topic_d = .doc_topic_prior + exp_doc_topic_d * tcrossprod(counts / phinorm,  exp_topic_word_d)
        exp_doc_topic_d = exp(dirichlet_expectation_1d(doc_topic_d))

        phinorm = exp_doc_topic_d %*% exp_topic_word_d + EPS
        # If doc_topic_distr hasn't changed much, we're done.
        meanchange = mean(abs(doc_topic_d - last_doc_topic_d))
        if (meanchange < .mean_change_tol)
          break
      }
      doc_topic_distr[i, ] = doc_topic_d
      if (calc_suff_stat)
        suff_stats[, ids] = suff_stats[, ids] + crossprod(exp_doc_topic_d, counts / phinorm)
    }
    if (calc_suff_stat) {
      suff_stats = suff_stats * .exp_topic_word_distr
      list(doc_topic_distr = doc_topic_distr, suff_stats = suff_stats)
    } else
      doc_topic_distr
  }
  # ------------------------------------------
  update_doc_distribution <- function(dtm_minibatch) {
    # learning_decay = rhot will be between 0 and 1, and says how much to weight
    # the information we got from this mini-batch.
    learning_decay = (.learning_offset + .update_count) ^ (-.learning_decay)
    .learning_decay <<- learning_decay
    # Do an E step to update doc_topic_distr, phi | topic_word_distr for this
    # mini-batch. This also returns the information about phi that
    # we need to update topic_word_distr.
    e_step = do_e_step(dtm_minibatch)

    # Estimate held-out likelihood for current values of topic_word_distr.

    bound = approx_bound(dtm_minibatch,
                         e_step$doc_topic_distr,
                         .topic_word_distr,
                         .doc_topic_prior,
                         .topic_word_prior,
                         .total_samples)
    .bound <<- c(.bound, bound)
    # message(paste("bound", bound))

    # Update topic_word_distr based on documents.
    .topic_word_distr <<- .topic_word_distr * (1 - learning_decay) +
      .learning_decay * (.topic_word_prior + .total_samples * e_step$suff_stats / length(dtm_minibatch))

    topic_word_distr = dirichlet_expectation_2d(.topic_word_distr)

    .exp_topic_word_distr <<- exp(topic_word_distr)

    .update_count <<- .update_count + 1
  }
  # ------------------------------------------

  fit <- function(X, n_iter, ...) {
    if (inherits(X, 'sparseMatrix'))
      X = to_lda_c(X)
    ndocs = length(X)
    i1 <- 1
    i2 <- min(ndocs, .batch_size)
    while (i1 <= ndocs) {

      update_doc_distribution(X[ i1:i2 ])
#       if (evaluate_every > 0 && (.update_count + 1) %% evaluate_every == 0) {
        perp = perplexity(X, transform(X),
                   .topic_word_distr,
                   .doc_topic_prior,
                   .topic_word_prior,
                   total_samples = NULL)
        if (.verbose) {
          message(sprintf('iteration: %d, perplexity: %.4f', .update_count + 1, perp))
        }

      # }

      i1 = i1 + .batch_size
      i2 = min(ndocs, i2 + .batch_size)
    }
    self()
  }
  partial_fit <- function(X, ...) {
    if (inherits(X, 'sparseMatrix'))
      X = to_lda_c(X)
    ndocs = length(X)
    i1 <- 1
    i2 <- min(ndocs, .batch_size)
    while (i1 <= ndocs) {
      update_doc_distribution(X[ i1:i2 ])
      i1 = i1 + .batch_size
      i2 = min(ndocs, i2 + .batch_size)
    }
  }
  transform <- function(X, norm = FALSE) {
    doc_topic_distr = do_e_step(X, calc_suff_stat = FALSE)
    # normalize doc_topic_distr
    doc_topic_distr / rowSums(doc_topic_distr)
  }

  self <- function() {
    model = list(fit = fit,
                 partial_fit = partial_fit,
                 transform = transform,
                 get_params = get_params)
    class(model) <- c('text2vec_model', 'online_LDA')
    model
  }

  self()
}

#---------------
# library(iterators)
# library(foreach)
# library(lda)
# data(cora.documents)
# data(cora.vocab)
# doc_topic_prior = 0.1
# topic_word_prior = 0.1
# topic_word_distr = NULL
# n_word = sum(vapply(cora.documents, function(x) sum(x[2,]), 0L))
# model = online_LDA(n_topics = 20,
#                    verbose = T,
#                    evaluate_every = 1,
#                    vocabulary = cora.vocab,
#                    doc_topic_prior = doc_topic_prior,
#                    topic_word_prior = topic_word_prior,
#                    batch_size = 256,
#                    topic_word_distr = topic_word_distr)
#
# fit2 = fit(model, cora.documents)
#
# res = fit2$get_params()
#
# docs = transform(fit2, cora.documents)
#
# doc_topic_distr = t(ldafit$document_sums)
# doc_topic_distr = doc_topic_distr / rowSums(doc_topic_distr) + 1e-1
# topic_word_distr = (ldafit$topics)
# topic_word_distr = t(t(topic_word_distr) / rowSums(t(topic_word_distr))) + 1e-1
#
# loglikelihood(0.1, doc_topic_distr, dirichlet_expectation_2d(doc_topic_distr), ncol(doc_topic_distr)) +
#   loglikelihood(0.1, topic_word_distr, dirichlet_expectation_2d(topic_word_distr), ncol(topic_word_distr))
#
# exp(1000151.09 / n_word)
#
# doc_topic_distr = doc_topic_distr / rowSums(doc_topic_distr)
# topic_word_distr = t((ldafit$topics)) + 1e-1
# topic_word_distr = t(topic_word_distr / rowSums(topic_word_distr))
#
# perp = perplexity(X = cora.documents,
#                   doc_topic_distr = doc_topic_distr,
#                   topic_word_distr = topic_word_distr,
#                   doc_topic_prior,
#                   topic_word_prior,
#                   total_samples = length(cora.documents))
# perp
#
# topic_word_distr = t(fit2$get_params()$topic_word_distr)
# topic_word_distr = t(topic_word_distr / rowSums(topic_word_distr))
#
# bound = approx_bound(cora.documents,
#                      doc_topic_distr = docs,
#                      topic_word_distr = topic_word_distr,
#                      doc_topic_prior,
#                      topic_word_prior,total_samples = length(cora.documents))
# n_word = sum(vapply(cora.documents, function(x) sum(x[2,]), 0L))
# perword_bound = bound / n_word
#
# exp(-1.0 * perword_bound)
#
#
# ldafit = lda.collapsed.gibbs.sampler(cora.documents, 20, cora.vocab, num.iterations = 50, doc_topic_prior,
#                                      topic_word_prior, compute.log.likelihood = T,
#                                      trace = 0L, freeze.topics = FALSE)
#
# doc_topic_distr = t(ldafit$document_sums) + 1e-5
# topic_word_distr = ldafit$topics + 1e-5
#
# perplexity(cora.documents,
#            doc_topic_distr,
#            topic_word_distr,
#            0.1,
#            0.1,
#            length(cora.documents))
#
# b = approx_bound(cora.documents,
#                  doc_topic_distr,
#                  topic_word_distr,
#                  0.1,
#                  0.1,
#                  length(cora.documents))
# exp(-b / sum(vapply(cora.documents, function(x) sum(x[2,]), 0L)))
#
# topic_word_distr = t(ldafit$topics)
# topic_word_distr = t(topic_word_distr / rowSums(topic_word_distr))
# dirichlet_topic_word = dirichlet_expectation_2d(topic_word_distr)
#
# l2 = loglikelihood(0.1, topic_word_distr, dirichlet_topic_word, length(length(cora.documents)))
#
# exp( - (l1 + l2) / sum(vapply(cora.documents, function(x) sum(x[2,]), 0L)))
#
# exp(-t(ldafit$log.likelihoods) / sum(vapply(cora.documents, function(x) sum(x[2,]), 0L)))
#
#
#
# topic_word_distr = ldafit$topics
# doc_topic_distr = t(ldafit$document_sums) / rowSums(t(ldafit$document_sums))
# perplexity(cora.documents, doc_topic_distr, topic_word_distr, 0.1, 0.1)
#
