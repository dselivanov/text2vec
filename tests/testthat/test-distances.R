context("distances")
ind = 1:100
m1 = matrix(1:15, nrow = 5)
m2 = matrix(1:12, nrow = 4)
tol = 1e-5

tokens = movie_review$review[ind] %>% tolower %>% word_tokenizer
it = itoken(tokens, progressbar = F)
v = create_vocabulary(it) %>% prune_vocabulary(term_count_min = 3)
dtm = create_dtm(it, vectorizer = vocab_vectorizer(v))
tcm = create_tcm(it, vectorizer = vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 5))
i1 = 1:10
i2 = 1:20

test_that("cosine", {
  expect_error(dist2(dtm[i1, ], dtm[i2, ], method = "cosine", norm = "l1"))

  cos_dist = dist2(dtm[i1, ], dtm[i2, ], method = "cosine", norm = "l2")
  cos_sim = sim2(dtm[i1, ], dtm[i2, ], method = "cosine", norm = "l2")
  expect_equal(1 - cos_sim, cos_dist)
  expect_lte(max(cos_dist), 1)
  expect_equal(nrow(cos_dist), length(i1))
  expect_equal(ncol(cos_dist), length(i2))
  expect_equal(cos_dist[1, 1], 0, tol = tol)
  expect_equal(cos_dist[1, 2], 0.4294539, tol = tol )
  expect_lte(max(cos_dist), 1)
  # check case when x = y (y = NULL)
  expect_equal(dist2(dtm[i1, ], method = "cosine", norm = "l2"),
               dist2(dtm[i1, ], dtm[i1, ], method = "cosine", norm = "l2"))
})

test_that("jaccard", {
  expect_error(dist2(m1, m2, method = "jaccard", norm = "l2"))
  jac_dist = dist2(dtm[i1, ], dtm[i2, ], method = "jaccard", norm = "none")
  jac_sim = sim2(dtm[i1, ], dtm[i2, ], method = "jaccard", norm = "l2")
  expect_equal(1 - jac_sim, jac_dist)
  expect_lte(max(jac_dist), 1)
  expect_equal(nrow(jac_dist), length(i1))
  expect_equal(ncol(jac_dist), length(i2))
  expect_equal(jac_dist[1, 1], 0, tol = tol)
  expect_equal(jac_dist[1, 2], 0.8207547, tol = tol)
  expect_lte(max(jac_dist), 1)
  expect_equal(dist2(dtm[i1, ], method = "jaccard", norm = "none"),
               dist2(dtm[i1, ], dtm[i1, ], method = "jaccard", norm = "none"))
})

test_that("euclidean", {
  # euclidean works only with dense matrices
  expect_error(dist2(dtm[i1, ], dtm[i2, ], method = "euclidean"))
  # m1 = matrix(1, nrow = 5, ncol = 4)
  # m2 = matrix(1, nrow = 3, ncol = 4)
  euc_dist = dist2(m1, m2, method = "euclidean", norm = "none")
  expect_equal(nrow(euc_dist), nrow(m1))
  expect_equal(ncol(euc_dist), nrow(m2))
  # calculate distances with base dist function
  base_dist = dist(rbind(m1, m2), method = "euclidean")
  # subset correct elements - they should correspond to two input matrices
  base_dist = as.matrix(base_dist)[1:nrow(m1), nrow(m1) + 1:nrow(m2)]
  dimnames(base_dist) = NULL
  expect_equal(base_dist, euc_dist)
  expect_equal(dist2(m1, method = "euclidean", norm = "none"),
               dist2(m1, m1, method = "euclidean", norm = "none"))
})

test_that("relaxed word mover distance", {
  glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = v, x_max = 10)
  glove$fit(tcm, n_iter = 10)
  wv = glove$get_word_vectors()
  rwmd_model = RWMD$new(wv)
  rwmd_model$verbose = FALSE
  rwmd_dist = dist2(dtm[i1, ], dtm[i2, ], method = rwmd_model, norm = "none")
  expect_equal(nrow(rwmd_dist), length(i1))
  expect_equal(ncol(rwmd_dist), length(i2))
  expect_equal(rwmd_dist[1,1], 0, tol = tol)
  expect_equal(dist2(dtm[i1, ], method = rwmd_model, norm = "none"),
               dist2(dtm[i1, ], dtm[i1, ], method = rwmd_model, norm = "none"))

})
