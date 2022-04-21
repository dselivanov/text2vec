context("vocabulary-corpus construction")

train_ind = 1:1000
ids = movie_review$id[train_ind]

txt = tolower(movie_review[['review']][train_ind])
names(txt) = ids

tokens = word_tokenizer(txt)

it = itoken(tokens, progressbar = FALSE, ids = ids)

test_that("Vocabulary pruning", {
  vocab = create_vocabulary(it)
  # Vocabulary stats
  expect_equal(length(vocab$term), 19297)
  expect_equal( vocab$term[ which.max(vocab$doc_count) ], 'the')
  expect_equal( max(vocab$doc_count), 992)
  expect_equal( max(vocab$term_count), 13224)

  COUNT_MAX = 5000L
  COUNT_MIN = 20L
  PROP_MIN = 0.05
  PROP_MAX = 0.95

  p_vocab = prune_vocabulary(vocab,
                              term_count_min = COUNT_MIN,
                              term_count_max = COUNT_MAX,
                              doc_proportion_min = PROP_MIN,
                              doc_proportion_max = PROP_MAX,
                             vocab_term_max = Inf
                              )
  # same number of underlying documents
  expect_identical(p_vocab$document_count, vocab$document_count)
  # same ngrams
  expect_identical(p_vocab$ngram, p_vocab$ngram)
  # number of terms in prunned vocab
  expect_equal(nrow(p_vocab), 428L)

  doc_count_min = 10L
  doc_count_max = 100L
  p_vocab = prune_vocabulary(vocab,
                             doc_count_min = doc_count_min,
                             doc_count_max = doc_count_max)
  expect_true( all(p_vocab$doc_count <= doc_count_max))
  expect_true( all(p_vocab$doc_count >= doc_count_min))

  PROP_MAX = 0.05
  LIMIT = 20L
  p_vocab = prune_vocabulary(vocab,
                              doc_proportion_max = PROP_MAX,
                             vocab_term_max = LIMIT)

  expect_equal( nrow(p_vocab), LIMIT)
  expect_true( all(p_vocab$doc_proportions <= PROP_MAX))

  # test for https://github.com/dselivanov/text2vec/issues/46
  vectorizer = vocab_vectorizer(p_vocab)

  dtm = create_dtm(it, vectorizer)
  # check we keep names for input. see #51
  expect_equal(rownames(dtm), ids)
  expect_identical(dim(dtm), c(length(txt), LIMIT))

  # check we keep names for input. see #51
  dtm_lda_c = as.lda_c(dtm)
  expect_equal(names(dtm_lda_c), ids)
})

test_that("Vocabulary stopwords", {
  STOP_WORDS = c('is', 'in', 'it')
  vocab = create_vocabulary(it, stopwords = STOP_WORDS)
  # check removed stop words
  expect_false(any(STOP_WORDS %in% vocab$term))
})

test_that("Unigran Vocabulary Corpus construction", {
  # Vocabulary construction
  vocab = create_vocabulary(it)
  vectorizer = vocab_vectorizer(vocab)

  # dtm
  m = create_dtm(it, vectorizer)
  expect_equal( dim(m)[[1]], length(train_ind))
  expect_equal( dim(m)[[2]], length(vocab$term))
  expect_equal( length(m@x), 141714L)

  # check classification accuracy
  # fit = glmnet::cv.glmnet(x = m, y = movie_review[['sentiment']][train_ind],
  #                          family = 'binomial',
  #                          type.measure = "auc",
  #                          nfolds = 4)
  #
  # expect_gt(max(fit$cvm), 0.8)
})

test_that("bi-gram Vocabulary Corpus construction", {
  # unigram + bigram VocabCorpus construction

  vocab = create_vocabulary(it,
                      ngram = c('ngram_min' = 2L,
                                'ngram_max' = 2L))

  expect_equal(sum(grepl("_", vocab$term, fixed = TRUE)), 121333L)
  expect_equal(length(vocab$term), 121333L)

  vectorizer = vocab_vectorizer(vocab)

  # dtm
  m = create_dtm(it, vectorizer)
  expect_equal( dim(m)[[1]], length(train_ind))
  expect_equal( dim(m)[[2]], length(vocab$term))
  expect_equal( length(m@x), 220104L)
})

test_that("Unigram + Bigram Vocabulary Corpus construction", {
  # unigram + bigram VocabCorpus construction
  vocab = create_vocabulary(it,
                      ngram = c('ngram_min' = 1L,
                                'ngram_max' = 2L))
  expect_equal(length(vocab$term), 140630L)

  vectorizer = vocab_vectorizer(vocab)
  # dtm
  m = create_dtm(it, vectorizer)
  expect_equal( dim(m)[[1]], length(train_ind))
  expect_equal( dim(m)[[2]], length(vocab$term))
  expect_equal( length(m@x), 361818L)
})

test_that("hash dtm & normalize", {
  n_hash = 2**10
  dtm = create_dtm(it, hash_vectorizer(n_hash))
  expect_equal(ncol(dtm), n_hash)
  expect_equal(nrow(dtm), length(train_ind))

  expect_equivalent(rowSums(normalize(dtm, "l1")), rep(1, nrow(dtm)))
  expect_equivalent(rowSums(normalize(as.matrix(dtm), "l1")), rep(1, nrow(dtm)))
  expect_error(rowSums(normalize(data.frame(i = 1, j = 2, x = 3), "l1")))
})
