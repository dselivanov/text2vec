context("text2vec iterators")

N = 100
########################################
# create temp files and dirs
########################################
txt_1 = movie_review$review[1:N]
txt_2 = movie_review$review[(N + 1):(2 * N)]

tmp_dir = tempdir()
temp_file_1 = tempfile(tmpdir = tmp_dir)
temp_file_2 = tempfile(tmpdir = tmp_dir)

writeLines(txt_1, con = temp_file_1)
writeLines(txt_2, con = temp_file_2)

########################################
# test
########################################
test_that("ifiles", {
  it = ifiles(c(temp_file_1, temp_file_2))
  it2 = itoken(it, preprocessor = tolower, tokenizer = word_tokenizer, progressbar = FALSE)
  v = create_vocabulary(it2)
  expect_equal(nrow(v), 7261)
  v2 = create_vocabulary(it2)
  expect_equal(v, v2)
  dtm = create_dtm(it2, hash_vectorizer())
  expected_rownames = c(paste(basename(temp_file_1), seq_along(txt_1), sep = "_"),
                        paste(basename(temp_file_2), seq_along(txt_2), sep = "_"))
  expect_equal(rownames(dtm), expected_rownames)
})

test_that("ifiles_parallel", {
  it = ifiles_parallel(c(temp_file_1, temp_file_2))
  it2 = itoken_parallel(it, preprocessor = tolower, tokenizer = word_tokenizer, progressbar = FALSE)
  expect_warning(v <- create_vocabulary(it2))
  expect_equal(nrow(v), 7261)
  v2 = create_vocabulary(it2)
  expect_equal(v, v2)
  dtm = create_dtm(it2, hash_vectorizer())
  expected_rownames = c(paste(basename(temp_file_1), seq_along(txt_1), sep = "_"),
                        paste(basename(temp_file_2), seq_along(txt_2), sep = "_"))
  expect_equal(rownames(dtm), expected_rownames)
})


test_that("idir", {
  it = idir(path = tmp_dir)
  it2 = itoken(it, preprocessor = tolower, tokenizer = word_tokenizer, progressbar = FALSE)
  v = create_vocabulary(it2)
  expect_equal(nrow(v), 7261)
})

# test_that("ilines", {
#   it = ilines(con = temp_file_1, n = 10)
#   it2 = itoken(it, preprocessor = tolower, tokenizer = word_tokenizer)
#   v = create_vocabulary(it2)
#   expect_equal(nrow(v), 4464)
# })

test_that("itoken character", {
  it2 = itoken(txt_1, preprocessor = tolower, tokenizer = word_tokenizer, progressbar = FALSE)
  v = create_vocabulary(it2)
  expect_equal(nrow(v), 4464)
})

test_that("itoken character parallel", {
  it2 = itoken_parallel(txt_1, preprocessor = tolower, tokenizer = word_tokenizer)
  v = create_vocabulary(it2)
  expect_equal(nrow(v), 4464)
})

########################################
# test that iterators are immutable
########################################
test_that("immutable input iterators", {
  tokens = tolower(movie_review[seq_len(N),]$review)
  tokens = space_tokenizer(tokens)
  it = itoken(tokens, progressbar = FALSE)
  v = create_vocabulary(it)
  dtm1 = create_dtm(it, vocab_vectorizer(v))
  dtm2 = create_dtm(it, vocab_vectorizer(v))
  expect_identical(dtm1, dtm2)
  dtm1 = create_dtm(it, hash_vectorizer(2**8))
  dtm2 = create_dtm(it, hash_vectorizer(2**8))
  expect_identical(dtm1, dtm2)
})
