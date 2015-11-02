context("Vocabulary Corpus construction")
data("movie_review")

train_ind <- 1:1000

prep_fun <- function(x) x %>% tolower %>%  regexp_tokenizer
lst <- movie_review[['review']][train_ind] %>% prep_fun

test_that("Unigran Vocabulary Corpus construction", {
  # Vocabulary construction
  vocab <- new(Vocabulary, 1, 1)
  vocab$insert_document_batch(lst)
  vocab_stat <- vocab$get_vocab_statistics()
  # Vocabulary stats
  expect_equal(length(vocab_stat$term), 17604)
  expect_equal( vocab_stat$term [ which.max(vocab_stat$doc_count) ], 'the')
  expect_equal( max(vocab_stat$doc_count), 906)
  expect_equal( max(vocab_stat$term_count), 11407)
  # VocabCorpus construction
  vcorpus <- new(VocabCorpus, vocab_stat$term, 1, 1)
  vcorpus$insert_document_batch(lst)
  # dtm
  m <- vcorpus$get_dtm()
  expect_equal( dim(m)[[1]], length(train_ind))
  expect_equal( dim(m)[[2]], length(vocab_stat$term))
  expect_equal( length(m@x), 123508L)
})

test_that("Bigram Vocabulary Corpus construction", {
  # unigram + bigram VocabCorpus construction
  vocab <- new(Vocabulary, 2, 2)
  vocab$insert_document_batch(lst)
  vocab_stat <- vocab$get_vocab_statistics()

  expect_equal(sum(grepl("_", vocab_stat$term, fixed = T)), 106781L)
  expect_equal(length(vocab_stat$term), 106781L)
  # VocabCorpus construction
  vcorpus <- new(VocabCorpus, vocab_stat$term, 2, 2)
  vcorpus$insert_document_batch(lst)
  # dtm
  m <- vcorpus$get_dtm()
  expect_equal( dim(m)[[1]], length(train_ind))
  expect_equal( dim(m)[[2]], length(vocab_stat$term))
  expect_equal( length(m@x), 190083L)
})

test_that("Unigram + Bigram Vocabulary Corpus construction", {
  # unigram + bigram VocabCorpus construction
  vocab <- new(Vocabulary, 1, 2)
  vocab$insert_document_batch(lst)
  vocab_stat <- vocab$get_vocab_statistics()
  expect_equal(length(vocab_stat$term), 124385L)
  # VocabCorpus construction
  vcorpus <- new(VocabCorpus, vocab_stat$term, 1, 2)
  vcorpus$insert_document_batch(lst)
  # dtm
  m <- vcorpus$get_dtm()
  expect_equal( dim(m)[[1]], length(train_ind))
  expect_equal( dim(m)[[2]], length(vocab_stat$term))
  expect_equal( length(m@x), 313591L)

})
