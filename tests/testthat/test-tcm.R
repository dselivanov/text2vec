context("tcm construction")

train_ind = 1:100

txt = movie_review[['review']][train_ind]
ids = movie_review[['id']][train_ind]

tokens = txt %>% tolower %>% word_tokenizer
it = itoken(tokens, progressbar = FALSE, ids = ids)

test_that("tcm with foreach", {
  v = create_vocabulary(it, c(1L, 1L) )
  v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.5)

  vectorizer = vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 1L,
                                skip_grams_window_context = "symmetric")
  tcm = create_tcm(it, vectorizer)

  expect_equal(tcm["you", "are"], 6)
  expect_true(Matrix::isTriangular(tcm, upper = TRUE))

  vectorizer_right = vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 1L,
                                skip_grams_window_context = "right")
  tcm_right = create_tcm(it, vectorizer_right)
  expect_equal(tcm_right["you", "are"], 5)
  expect_equal(tcm_right["are", "you"], 1)

  vectorizer_left = vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 1L,
                                      skip_grams_window_context = "left")
  tcm_left = create_tcm(it, vectorizer_left)
  expect_equal(tcm_left["you", "are"], 1)
  expect_equal(tcm_left["are", "you"], 5)
})
