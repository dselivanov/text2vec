context("hash-corpus construction")

train_ind <- 1:1000

txt <- movie_review[['review']][train_ind]

get_test_iterator <- function()
  itoken(txt,
         preprocess_function = tolower,
         tokenizer = word_tokenizer,
         progessbar = F)

test_that("Unigram Hash Corpus construction", {
  h_size = 2 ^ 14;
  hcorpus <- create_hash_corpus(get_test_iterator(),
                                feature_hasher = feature_hasher(h_size, c(1L, 1L)))
  m <- hcorpus$get_dtm()
  expect_equal( dim(m)[[1]], length(train_ind))
  expect_equal( dim(m)[[2]], h_size)
  expect_equal( length(m@x), 140615L)

  fit <- glmnet::cv.glmnet(x = m, y = movie_review[['sentiment']][train_ind],
                           family = 'binomial',
                           type.measure = "auc",
                           nfolds = 4)
  expect_gt(max(fit$cvm), 0.8)
})

test_that("trigram hash-corpus construction", {
  h_size = 2 ^ 18;
  hcorpus <- create_hash_corpus(get_test_iterator(),
                                feature_hasher = feature_hasher(h_size, c(1L, 3L)))
  m <- hcorpus$get_dtm()
  expect_equal( dim(m)[[1]], length(train_ind))
  expect_equal( dim(m)[[2]], h_size)
  expect_equal( length(m@x), 591549L)
})
