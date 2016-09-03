context("lsa model")
N = 100
n_topics = 100
train_ind = 1:N

txt = movie_review[['review']][train_ind] %>% tolower %>% word_tokenizer
ids = movie_review[['id']][train_ind]

vocab = create_vocabulary(itoken(txt)) %>%
  prune_vocabulary(term_count_min = 5, doc_proportion_max = 0.5)

dtm = create_dtm(itoken(txt, ids = ids), vocab_vectorizer(vocab))

test_that("LSA", {

  model = LatentSemanticAnalysis$new(n_topics)
  # suppressWarnings because we perform full rank SVD and Rspectra produce warning
  suppressWarnings(model$fit(dtm))
  documents = model$transform(dtm)
  expect_equal(rownames(documents), ids)

  # check fit_transform & (fit(); transform()) give similar results
  # suppressWarnings because we perform full rank SVD and Rspectra produce warning
  suppressWarnings(documents_2 <- fit_transform(dtm, LatentSemanticAnalysis$new(n_topics)))
  expect_equal(documents, documents_2)


  # check cosine angle between original vectors and vectors is latent factor space
  i1 = 1; i2 = 2;

  cos_lsa = sum(documents[i1, ] * documents[i2, ]) /
    sqrt(sum(documents[i1, ] ^ 2)) /
    sqrt(sum(documents[i2, ] ^ 2))

  cos_orig = sum(dtm[i1, ] * dtm[i2, ]) /
    sqrt(sum(dtm[i1, ] ^ 2)) /
    sqrt(sum(dtm[i2, ] ^ 2))

  expect_equal(cos_lsa,  cos_orig)
})

