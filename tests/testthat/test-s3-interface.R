context("s3 model interface")
N = 100
n_topics = 10
train_ind = 1:N

txt = movie_review[['review']][train_ind] %>% tolower %>% word_tokenizer
ids = movie_review[['id']][train_ind]
it = itoken(txt, ids = ids, progressbar = FALSE)
vocab = create_vocabulary(it) %>%
  prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.5)

dtm = create_dtm(it, vocab_vectorizer(vocab))

test_that("S3 LSA", {
  lsa = LatentSemanticAnalysis$new(n_topics)
  fit(dtm, lsa)
  d1 = dtm %>% transform(lsa)
  expect_equal(rownames(d1), ids)
})

test_that("S3 LDA", {
  convergence_tol = -1
  n_iter = 10
  lda = LDA$new(n_topics, vocab, 0.1, 0.1)
  fit(dtm, lda, n_iter = n_iter)
  d1 = dtm %>% transform(lda, n_iter = n_iter, convergence_tol = convergence_tol)
  d2 = dtm %>% fit_transform(lda, n_iter = n_iter, convergence_tol = convergence_tol)
  expect_equal(rownames(d1), ids)
  expect_equal(rownames(d2), ids)
  expect_equal(dim(d2), c(N, n_topics))
})

test_that("S3 tf-idf", {
  tfidf = TfIdf$new()
  fit(dtm, tfidf)
  d1 = dtm %>% transform(tfidf)
  d2 = dtm %>% fit_transform(tfidf)
  expect_equal(rownames(d1), ids)
  expect_equal(d1, d2)
  expect_equal(dim(d1), dim(dtm))
  expect_equal(dimnames(d1), dimnames(dtm))
  expect_equal(d1['5814_8', 'alone' ], 0.01453148, tolerance = .0001)
})
