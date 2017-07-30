context("s3 model interface")
N = 100
n_topics = 10
train_ind = 1:N

txt = tolower(movie_review[['review']][train_ind])
txt = word_tokenizer(txt)
ids = movie_review[['id']][train_ind]
it = itoken(txt, ids = ids, progressbar = FALSE)
vocab = create_vocabulary(it)
vocab = prune_vocabulary(vocab, term_count_min = 5, doc_proportion_max = 0.5)

dtm = create_dtm(it, vocab_vectorizer(vocab))

test_that("S3 LSA", {
  lsa = LatentSemanticAnalysis$new(n_topics)
  d1 = dtm %>% fit_transform(lsa)
  expect_equal(rownames(d1), ids)
})

test_that("S3 LDA", {
  convergence_tol = -1
  n_iter = 10
  lda = LDA$new(n_topics)
  d2 = dtm %>% fit_transform(lda, n_iter = n_iter, convergence_tol = convergence_tol, progressbar = FALSE)
  expect_equal(rownames(d2), ids)
  expect_equal(dim(d2), c(N, n_topics))
})

test_that("S3 tf-idf", {
  tfidf = TfIdf$new()
  d2 = dtm %>% fit_transform(tfidf)
  expect_equal(rownames(d2), ids)
  expect_equal(dim(d2), dim(dtm))
  expect_equal(dimnames(d2), dimnames(dtm))
})
