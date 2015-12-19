context("text2vec utils - tokenization, etc.")

txt <- tolower(movie_review$review[[1]])

txt_first_10 <- c("with", "all", "this", "stuff",
             "going", "down", "at", "the",
             "moment", "with")

test_that("word_tokenizer ", {
  tokens <- word_tokenizer(txt)[[1]]
  expect_equal(length(tokens), 438)
  expect_equal(tokens[1:10], txt_first_10)
  # non ASCII symbols
  tokens <- word_tokenizer("one, two. Three! four")[[1]]
  expect_equal(tokens, c("one", "two", "Three", "four"))
})
