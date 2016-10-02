context("lsa model")
N = 100
n_topics = 20
train_ind = 1:N

txt = movie_review[['review']][train_ind] %>% tolower %>% word_tokenizer
ids = movie_review[['id']][train_ind]
it = itoken(txt, ids = ids, progressbar = FALSE)

vocab = create_vocabulary(it) %>%
  prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.5)

dtm = create_dtm(it, vocab_vectorizer(vocab))

test_that("LSA", {

  model = LatentSemanticAnalysis$new(n_topics)
  set.seed(1L)
  model$fit(dtm)
  documents = model$transform(dtm)
  expect_equal(rownames(documents), ids)


  # check cosine angle between original vectors and vectors is latent factor space
  # i1 = 1; i2 = 2;
  #
  # cos_lsa = sum(documents[i1, ] * documents[i2, ]) /
  #   sqrt(sum(documents[i1, ] ^ 2)) /
  #   sqrt(sum(documents[i2, ] ^ 2))
  #
  # cos_orig = sum(dtm[i1, ] * dtm[i2, ]) /
  #   sqrt(sum(dtm[i1, ] ^ 2)) /
  #   sqrt(sum(dtm[i2, ] ^ 2))
  #
  # expect_equal(cos_lsa,  cos_orig)
})

