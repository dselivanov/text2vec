context("text2vec utils - tokenization, etc.")

txt = tolower(movie_review$review[[1]])
N = 20
it = itoken(movie_review$review[1:N], ids = movie_review$id[1:N], progressbar = F)
dtm = create_dtm(it, hash_vectorizer(2**8), "dgTMatrix")

txt_first_10 = c("with", "all", "this", "stuff",
             "going", "down", "at", "the",
             "moment", "with")

test_that("word_tokenizer ", {
  tokens = word_tokenizer(txt)[[1]]
  expect_equal(length(tokens), 438)
  expect_equal(tokens[1:10], txt_first_10)
  # non ASCII symbols
  tokens = word_tokenizer("one, two. Three! four")[[1]]
  expect_equal(tokens, c("one", "two", "Three", "four"))
})

test_that("space_tokenizer ", {
  tokens = space_tokenizer(txt)[[1]]
  expect_equal(length(tokens), 433)
  expect_equal(tokens[1:10], txt_first_10)
  expect_error(space_tokenizer("one, two. Three! four", "[[:punct:]]|\\s+")[[1]])
})

test_that("char_tokenizer ", {
  txt = "aaabbc!."
  tokens = char_tokenizer(txt)[[1]]
  expect_equal(length(tokens), nchar(txt))
})

test_that("split_into ", {
  n_splits = 3
  splits = split_into(1:N, n_splits)
  expect_equal(length(splits), n_splits)
  max_chunk_len = max(vapply(splits, length, 0L))
  expect_lte(max_chunk_len, ceiling(N / n_splits))
})

test_that("rbind_dgTMatrix ", {
  it1 = itoken(movie_review$review[1:10], ids = movie_review$id[1:10], progressbar = F)
  dtm1 = create_dtm(it1, hash_vectorizer(2**8), "dgTMatrix")
  it2 = itoken(movie_review$review[11:20], ids = movie_review$id[11:20], progressbar = F)
  dtm2 = create_dtm(it2, hash_vectorizer(2**8), "dgTMatrix")
  expect_equal(Matrix::uniqTsparse(text2vec:::rbind_dgTMatrix(dtm1, dtm2)),
               Matrix::uniqTsparse(dtm))
})
test_that("as.lda_c ", {
  K = 100
  train_tokens = tolower(movie_review$review[1:K])
  train_tokens =  word_tokenizer(train_tokens)

  it_train = itoken(train_tokens,
                    ids = movie_review$id[1:K])

  vocab = create_vocabulary(it_train)
  vocab = prune_vocabulary(vocab, term_count_min = 10, doc_proportion_max = 0.1)
  dtm = create_dtm(it_train, vocab_vectorizer(vocab))
  rs = Matrix::rowSums(sign(dtm))
  expect_equal(sum(rs == 0) , 2)
  dtm_lda_c = as.lda_c(dtm)
  expect_equal(names(dtm_lda_c), movie_review$id[1:K])
  expect_equal(rs, vapply(dtm_lda_c, ncol, 0L))
  expect_equal(Matrix::rowSums(dtm), vapply(dtm_lda_c, function(x) sum(x[2, ]), 0L))
})


# test of vectorized approach for term_index_combinations ----------------------------------------
#TODO use testthat for the following....
#TODO test if it works for one term against sets of terms,e.g.,
#indexes are c(1,2,3), set would be: c(1, list(1,2,3), c(2, list(1,2,3)), c(3, list(1,2,3))
#TODO test with some numbers from data and different orderings...
#see http://pydoc.net/gensim/3.2.0/gensim.topic_coherence.segmentation/
#assuming ordered diagonal
m <- matrix(rbind(c(40, 1, 2, 3),
                  c(0, 30, 4, 5),
                  c(0,  0,20, 6),
                  c(0,  0, 0,10)), ncol = 4)
m[lower.tri(m)] <- t(m)[lower.tri(m)]

idxs <- c(1,2,3,4) #e.g. as in tcm ordered by diagonal
idxs_as_in_topic <- c(2,1,3,4) #order of indices (corresponding terms) in topic

a <- term_index_combinations(idxs, comb_type = "one_idx-succeeding_idxs")
colnames(a) <- c("x", "y")
values <- t(mapply(function(x, y) {c(v_x = m[x,x], v_y = m[y,y], v_xy = m[x,y], v_xy_div_y = m[x,y]/m[y,y])},
                   a[,1], a[,2]))
a <- cbind(a, values)

test <- m/diag(m)
sum(test[lower.tri(test)]) == sum(a[,"v_xy_div_y"])

b <- term_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs")
colnames(b) <- c("x", "y")
values <- t(mapply(function(x, y) {c(v_x = m[x,x], v_y = m[y,y], v_xy = m[x,y], v_xy_div_y = m[x,y]/m[y,y])},
                   b[,1], b[,2]))
b <- cbind(b, values)

test <- m/diag(m)
sum(test[upper.tri(test)]) == sum(b[,"v_xy_div_y"])


c <- term_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs-topic_order")
colnames(c) <- c("x", "y")
values <- t(mapply(function(x, y) {c(v_x = m[x,x], v_y = m[y,y], v_xy = m[x,y], v_xy_div_y = m[x,y]/m[y,y])},
                   c[,1], c[,2]))
c <- cbind(c, values)

test <- m/diag(m)
sum(test[lower.tri(test)]) == sum(c[,"v_xy_div_y"])

