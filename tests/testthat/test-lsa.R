context("lsa model")
N = 100
n_topics = 20
train_ind = 1:N

tokens = tolower(movie_review[['review']][train_ind])
tokens = word_tokenizer(tokens)
ids = movie_review[['id']][train_ind]
it = itoken(tokens, ids = ids, progressbar = FALSE)

vocab = create_vocabulary(it) %>%
  prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.5)

dtm = create_dtm(it, vocab_vectorizer(vocab))

test_that("LSA", {

  model = LatentSemanticAnalysis$new(n_topics)
  set.seed(1L)
  m1 = model$fit_transform(dtm)
  m2 = model$transform(dtm)
  expect_equal(m1, m2, tolerance = 1e-8)
  expect_equal(rownames(m2), ids)
  expect_equal(dim(model$components), c(n_topics, ncol(dtm)))
})

