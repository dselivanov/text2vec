context("tcm construction")
doParallel::registerDoParallel(2)

train_ind = 1:100

txt = movie_review[['review']][train_ind]
ids = movie_review[['id']][train_ind]

tokens = tolower(txt)
tokens = word_tokenizer(tokens)
it = itoken(tokens, progressbar = FALSE, ids = ids)

it_par = itoken_parallel(txt, preprocessor = tolower, tokenizer = word_tokenizer, ids = ids, n_chunks = 2)

test_that("tcm", {
  v = create_vocabulary(it, c(1L, 1L) )
  v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.5)
  v = as.data.frame(v)
  v = v[rev(order(v$term)), ]
  # attributes(v) = v_attr
  vectorizer = vocab_vectorizer(v)
  tcm = create_tcm(it, vectorizer, skip_grams_window = 1L,
                   skip_grams_window_context = "symmetric")

  tcm_par = create_tcm(it_par, vectorizer, skip_grams_window = 1L,
                   skip_grams_window_context = "symmetric")

  expect_identical(Matrix::uniqTsparse(tcm), Matrix::uniqTsparse(tcm_par))
  expect_equal(tcm["you", "are"], 6)
  expect_true(Matrix::isTriangular(tcm, upper = TRUE))

  vectorizer_right = vocab_vectorizer(v)
  tcm_right = create_tcm(it, vectorizer_right, skip_grams_window = 1L,
                         skip_grams_window_context = "right")
  expect_equal(tcm_right["you", "are"], 5)
  expect_equal(tcm_right["are", "you"], 1)

  vectorizer_left = vocab_vectorizer(v)
  tcm_left = create_tcm(it, vectorizer_left, grow_dtm = FALSE, skip_grams_window = 1L,
                        skip_grams_window_context = "left")
  expect_equal(tcm_left["you", "are"], 1)
  expect_equal(tcm_left["are", "you"], 5)
})
