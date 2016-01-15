context("vocabulary-corpus construction")

train_ind <- 1:1000

txt <- movie_review[['review']][train_ind]

get_test_iterator <- function()
  itoken(txt,
         preprocess_function = tolower,
         tokenizer = word_tokenizer,
         progessbar = F)

test_that("Vocabulary pruning", {
  iterator <- get_test_iterator()
  vocab <- vocabulary(iterator)
  # Vocabulary stats
  expect_equal(length(vocab$vocab$terms), 19297)
  expect_equal( vocab$vocab$terms[ which.max(vocab$vocab$doc_counts) ], 'the')
  expect_equal( max(vocab$vocab$doc_counts), 992)
  expect_equal( max(vocab$vocab$terms_counts), 13224)

  COUNT_MAX <- 5000L
  COUNT_MIN <- 20L
  PROP_MIN <- 0.05
  PROP_MAX <- 0.95
  STOP_WORDS <- c('is', 'in', 'it')
  p_vocab <- prune_vocabulary(vocab,
                              term_count_min = COUNT_MIN,
                              term_count_max = COUNT_MAX,
                              doc_proportion_min = PROP_MIN,
                              doc_proportion_max = PROP_MAX,
                              stop_words = STOP_WORDS,
                              max_number_of_terms = Inf
                              )
  # same number of underlying documents
  expect_identical(p_vocab$document_count, vocab$document_count)
  # same ngrams
  expect_identical(p_vocab$ngram, p_vocab$ngram)
  # number of terms in prunned vocab
  expect_equal(nrow(p_vocab$vocab), 425L)
  # check removed stop words
  expect_false(any(STOP_WORDS %in% p_vocab$terms))

  PROP_MAX <- 0.05
  LIMIT <- 20L
  p_vocab <- prune_vocabulary(vocab,
                              doc_proportion_max = PROP_MAX,
                              max_number_of_terms = LIMIT)

  expect_equal( nrow(p_vocab$vocab), LIMIT)
  expect_true( all(p_vocab$vocab$doc_proportions <= PROP_MAX))

  # test for https://github.com/dselivanov/text2vec/issues/46
  vcorpus <- create_vocab_corpus(get_test_iterator(), vocabulary = p_vocab)

  dtm <- get_dtm(vcorpus)

  expect_identical(dim(dtm), c(length(txt), LIMIT))

})

test_that("Unigran Vocabulary Corpus construction", {
  # Vocabulary construction
  iterator <- get_test_iterator()
  vocab <- vocabulary(iterator)
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
