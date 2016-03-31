context("tcm construction")

train_ind <- 1:1000
N_WORKER <- 4

txt <- movie_review[['review']][train_ind]
ids <- movie_review[['id']][train_ind]

txt_splits <- split_into(txt, N_WORKER)


test_that("tcm with foreach", {
  TERMS_LIMIT = 3000L
  jobs <- lapply(txt_splits, itoken, tolower, word_tokenizer, chunks_number = 1)

  v <- create_vocabulary(jobs, c(1L, 1L) ) %>%
    prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.5, max_number_of_terms = TERMS_LIMIT)

  vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)

  jobs <- lapply(txt_splits, itoken, tolower, word_tokenizer)

  tcm <- create_tcm(jobs, vectorizer)
  expect_true(tcm['his', 'he'] - 42 < 1e-5)
  expect_equal(dim(tcm),  c(TERMS_LIMIT, TERMS_LIMIT))
  expect_true(isTriangular(tcm, upper = TRUE))
})
