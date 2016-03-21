context("text2vec iterators")

N <- 100
########################################
# create temp files and dirs
########################################
txt_1 <- movie_review$review[1:N]
txt_2 <- movie_review$review[(N + 1):(2 * N)]

tmp_dir <- tempdir()
temp_file_1 <- tempfile(tmpdir = tmp_dir)
temp_file_2 <- tempfile(tmpdir = tmp_dir)

writeLines(txt_1, con = temp_file_1)
writeLines(txt_2, con = temp_file_2)

########################################
# test
########################################
test_that("ifiles", {
  it <- ifiles(c(temp_file_1, temp_file_2))
  it2 <- itoken(it, preprocess_function = tolower, tokenizer = word_tokenizer)
  v <- create_vocabulary(it2)
  expect_equal(nrow(v$vocab), 7448)
})

test_that("idir", {
  it <- idir(path = tmp_dir)
  it2 <- itoken(it, preprocess_function = tolower, tokenizer = word_tokenizer)
  v <- create_vocabulary(it2)
  expect_equal(nrow(v$vocab), 7448)
})

test_that("ilines", {
  it <- ilines(con = temp_file_1, n = 10)
  it2 <- itoken(it, preprocess_function = tolower, tokenizer = word_tokenizer)
  v <- create_vocabulary(it2)
  expect_equal(nrow(v$vocab), 4562)
})

test_that("itoken character", {
  it2 <- itoken(txt_1, preprocess_function = tolower, tokenizer = word_tokenizer)
  v <- create_vocabulary(it2)
  expect_equal(nrow(v$vocab), 4562)
})
