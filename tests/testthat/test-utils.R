context("text2vec utils - tokenization, etc.")
data(movie_review)

txt <- tolower(movie_review[['review']][[1]])

txt_first_10 <- c("with", "all", "this", "stuff",
             "going", "down", "at", "the",
             "moment", "with")

test_that("regexp_tokenizer ", {
  tokens <- regexp_tokenizer(txt)[[1]]
  expect_equal(length(tokens), 438)
  expect_equal(tokens[1:10], txt_first_10)
  # non ASCII symbols
  tokens <- regexp_tokenizer("Ёлка для ёжикаб йод. Яркое солнце")[[1]]
  expect_equal(tokens, c("Ёлка", "для", "ёжикаб", "йод", "Яркое", "солнце"))
})
