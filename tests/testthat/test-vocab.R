context("Vocabulary construction")
data("movie_review")

train_ind <- 1:1000

test_that("Vocabulary construction", {

  prep_fun <- function(x) x %>% tolower %>%  str_split(boundary("word"))
  lst <- movie_review[['review']][train_ind] %>% prep_fun

  vocab <- new(Vocabulary)
  vocab$insert_sentence_batch(lst)
  vocab_stat <- vocab$vocab_stat()

  expect_equal(length(vocab_stat), 4)
  expect_equal(length(vocab_stat$term), 19297)
  expect_equal( vocab_stat$term [ which.max(vocab_stat$doc_count) ], 'the')
  expect_equal( max(vocab_stat$doc_count), 992)
  expect_equal( max(vocab_stat$term_count), 13224)
})
