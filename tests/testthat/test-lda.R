context("WarpLDA")
#------------------------------------------------------------------------------
train_ind = 1:200
ids = movie_review$id[train_ind]

txt = tolower(movie_review[['review']][train_ind])
names(txt) = ids

tokens = word_tokenizer(txt)

it = itoken(tokens, progressbar = F, ids = ids)
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 5, doc_proportion_min = 0.02)

vctrz = vocab_vectorizer(vocab)
dtm = create_dtm(it, vectorizer = vctrz)
#------------------------------------------------------------------------------
n_topic = 10

test_that("LDA, perplexity", {

  model = LDA$new(n_topic, doc_topic_prior = 0.1, topic_word_prior = 0.01)

  expect_warning(model$components)
  expect_error(suppressWarnings(model$topic_word_distribution))
  expect_null(model$doc_topic_distribution)

  set.seed(1)
  n_iter = 10
  doc_topic_distr_10  = model$fit_transform(dtm, n_iter = n_iter, n_check_convergence = 1,
                                            convergence_tol = -1, progressbar = FALSE)
  expect_equal(nrow(attr(doc_topic_distr_10, "likelihood")), n_iter)
  topic_word_distr_10 = model$topic_word_distribution

  perpl_10 = perplexity(dtm, topic_word_distr_10, doc_topic_distr_10)

  expect_equal(dim(doc_topic_distr_10), c(nrow(dtm), n_topic))
  expect_equivalent(rowSums(doc_topic_distr_10), rep(1, nrow(dtm)))
  expect_equal(dim(model$components), c(n_topic, ncol(dtm)))
  expect_equivalent(rowSums(topic_word_distr_10), rep(1, n_topic))

  set.seed(1)
  n_iter = 20
  doc_topic_distr_20  = model$fit_transform(dtm, n_iter = n_iter, n_check_convergence = 4,
                                           convergence_tol = -1, progressbar = FALSE)
  topic_word_distr_20 = model$topic_word_distribution
  expect_equal(nrow(attr(doc_topic_distr_20, "likelihood")), n_iter / 4)

  perpl_20 = perplexity(dtm, topic_word_distr_20, doc_topic_distr_20)

  expect_gt(perpl_10, perpl_20)

  expect_error(perplexity(dtm, doc_topic_distr_20, topic_word_distr_20))
  # check that perplexity() checks input dimensions
  expect_error(perplexity(dtm[1:50, ], doc_topic_distr_20, topic_word_distr_20))
  expect_error(perplexity(dtm, doc_topic_distr_20[1:5, ], topic_word_distr_20))
  expect_error(perplexity(dtm, doc_topic_distr_20[, 1:5], topic_word_distr_20))
  expect_error(perplexity(dtm, doc_topic_distr_20, topic_word_distr_20[, 1:5]))
  expect_error(perplexity(dtm, doc_topic_distr_20, topic_word_distr_20[1:5, ]))

  # check that perplexity() checks that inpputs are actual distributions
  expect_error(perplexity(dtm, doc_topic_distr_20 + 1, topic_word_distr_20))
  expect_error(perplexity(dtm, doc_topic_distr_20, topic_word_distr_20 + 1))

  # check model transform new data correctly
  dtm_test = dtm
  dtm_predict = model$transform(dtm_test, n_iter = 1)
  expect_equal(dim(dtm_predict), c(nrow(dtm_test), n_topic))
  expect_equivalent(rowSums(dtm_predict), rep(1, nrow(dtm_test)))
})
