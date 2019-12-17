context("lsa model")
N = 100
n_topics = 20
train_ind = 1:N

tokens = tolower(movie_review[['review']][train_ind])
tokens = word_tokenizer(tokens)
ids = movie_review[['id']][train_ind]
it = itoken(tokens, ids = ids, progressbar = FALSE)

vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 5, doc_proportion_max = 0.5)

dtm = create_dtm(it, vocab_vectorizer(vocab))

# Variance explained by component over total variance of original matrix
proportion_var_explained = function(dtm, decomp){
  apply(decomp, 2, var) / (sum(apply(dtm, 2, var)))
}

test_that("LSA", {

  model = LatentSemanticAnalysis$new(n_topics)
  set.seed(1L)
  m1 = model$fit_transform(dtm)
  m2 = model$transform(dtm)
  expect_equal(m1, m2, tolerance = 1e-8)
  expect_equal(rownames(m2), ids)
  expect_equal(dim(model$components), c(n_topics, ncol(dtm)))
})

test_that("LSA decomposition quality", {
  max_size = min(ncol(dtm), nrow(dtm)) - 1
  model = LatentSemanticAnalysis$new(max_size)

  m1 = model$fit_transform(dtm)

  manual_decomp = rsparse::soft_svd(dtm, max_size)

  m2 = dtm %*% manual_decomp$v

  expect_equal(dim(m1), dim(m2), info = "Dimensions sanity check")

  expect_equal(sum(model$get_explained_variance_ratio()),
               sum(proportion_var_explained(dtm, m2)), tolerance = 1e-5,
               info = "Proportion of variance explained should match")

  expect_equal(sum(model$get_explained_variance_ratio()), 1.0, tolerance = 1e-3,
               info = "When doing non-truncated SVD, total variance explained should be ~1.0")
})
