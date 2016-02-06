context("tcm construction")

split_into <- function(vec, nparts) {
  max_part_len <- ceiling(length(vec) / nparts)
  suppressWarnings( split(vec, rep(1:nparts, each = max_part_len)) )
}

train_ind <- 1:1000
N_WORKER <- 4

txt <- movie_review[['review']][train_ind]
ids <- movie_review[['id']][train_ind]

txt_splits <- split_into(txt, N_WORKER)


test_that("tcm with foreach", {
  TERMS_LIMIT = 3000L
  jobs <- lapply(txt_splits, itoken, tolower, word_tokenizer)

  v <- vocabulary(jobs, c(1L, 1L) ) %>%
    prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.5, max_number_of_terms = TERMS_LIMIT)

  vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)

  jobs <- lapply(txt_splits, itoken, tolower, word_tokenizer)

  tcm <- create_tcm(jobs, vectorizer)
  expect_equal(tcm['from', 'like'],  10)
  expect_equal(dim(tcm),  c(TERMS_LIMIT, TERMS_LIMIT))
  expect_true(isTriangular(tcm, upper = TRUE))
})
