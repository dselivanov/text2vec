context("vocabulary-corpus construction")

train_ind <- 1:1000

txt <- movie_review[['review']][train_ind]

get_test_iterator <- function()
  itoken(txt,
         preprocess_function = tolower,
         tokenizer = word_tokenizer,
         progessbar = F)

test_that("Unigran Vocabulary Corpus construction", {
  # Vocabulary construction
  iterator <- get_test_iterator()
  vocab <- vocabulary(iterator,
                             ngram = c('ngram_min' = 1L,
                                       'ngram_max' = 1L),
                             serialize_dir = NULL)
  # Vocabulary stats
  expect_equal(length(vocab$vocab$terms), 19297)
  expect_equal( vocab$vocab$terms[ which.max(vocab$vocab$doc_counts) ], 'the')
  expect_equal( max(vocab$vocab$doc_counts), 992)
  expect_equal( max(vocab$vocab$terms_counts), 13224)
  # VocabCorpus construction

  vcorpus <- create_vocab_corpus(get_test_iterator(),
                                 vocabulary = vocab)
  # dtm
  m <- vcorpus$get_dtm()
  expect_equal( dim(m)[[1]], length(train_ind))
  expect_equal( dim(m)[[2]], length(vocab$vocab$terms))
  expect_equal( length(m@x), 141714L)

  # check classification accuracy
  fit <- glmnet::cv.glmnet(x = m, y = movie_review[['sentiment']][train_ind],
                           family = 'binomial',
                           type.measure = "auc",
                           nfolds = 4)

  expect_gt(max(fit$cvm), 0.8)
})

test_that("Bigram Vocabulary Corpus construction", {
  # unigram + bigram VocabCorpus construction
  iterator <- get_test_iterator()
  vocab <- vocabulary(iterator,
                      ngram = c('ngram_min' = 2L,
                                'ngram_max' = 2L),
                      serialize_dir = NULL)

  expect_equal(sum(grepl("_", vocab$vocab$terms, fixed = T)), 121333L)
  expect_equal(length(vocab$vocab$terms), 121333L)
  # VocabCorpus construction
  vcorpus <- create_vocab_corpus(get_test_iterator(),
                                 vocabulary = vocab)
  # dtm
  m <- vcorpus$get_dtm()
  expect_equal( dim(m)[[1]], length(train_ind))
  expect_equal( dim(m)[[2]], length(vocab$vocab$terms))
  expect_equal( length(m@x), 220104L)
})

test_that("Unigram + Bigram Vocabulary Corpus construction", {
  # unigram + bigram VocabCorpus construction
  iterator <- get_test_iterator()
  vocab <- vocabulary(iterator,
                      ngram = c('ngram_min' = 1L,
                                'ngram_max' = 2L),
                      serialize_dir = NULL)
  expect_equal(length(vocab$vocab$terms), 140630L)
  # VocabCorpus construction
  vcorpus <- create_vocab_corpus(get_test_iterator(),
                                 vocabulary = vocab)
  # dtm
  m <- vcorpus$get_dtm()
  expect_equal( dim(m)[[1]], length(train_ind))
  expect_equal( dim(m)[[2]], length(vocab$vocab$terms))
  expect_equal( length(m@x), 361818L)

})
