futile.logger::flog.threshold(futile.logger::FATAL)

context("collocations")

preprocessor = function(x) {
  gsub("[^[:alnum:]\\s]", replacement = " ", tolower(x))
}
sample_ind = 1:100
tokens = preprocessor(movie_review$review[sample_ind])
tokens = word_tokenizer(tokens)
it = itoken(tokens, ids = movie_review$id[sample_ind])
system.time(v <- create_vocabulary(it))
v = prune_vocabulary(v, term_count_min = 5)

cc = Collocations$new(collocation_count_min = 5, pmi_min = 5)
cc$fit(it, n_iter = 2)
cc$collocation_stat

it2 = cc$transform(it)
v2 = create_vocabulary(it2)
v2 = prune_vocabulary(v2, term_count_min = 5)

# check what phrases model has learned


test_that("collocations internalfunctions", {
  sep = "_"
  ptr = text2vec:::create_xptr_unordered_set(c("c_te", "d_ivoire", "c_te_d_ivoire"))
  x = text2vec:::collapse_collocations_cpp(list(c("c", "te", "d", "2", "2")), ptr, sep)[[1]]
  expect_equal(c("c_te", "d", "2", "2" ), x)
  x = text2vec:::collapse_collocations_cpp(list(c("c", "te", "d", "ivoire", "2")), ptr, sep)[[1]]
  expect_equal(c("c_te_d_ivoire", "2" ), x)
  x = text2vec:::collapse_collocations_cpp(list(c("c", "te", "d", "ivoire")), ptr, sep)[[1]]
  expect_equal(c("c_te_d_ivoire"), x)

  expect_equal(length(setdiff(v2$term, v$term)), 44L)
  expect_equal(nrow(cc$collocation_stat), 46L)
  expect_true("jeroen_krabb" %in% v2$term)
  expect_true(all(cc$collocation_stat$n_ij >= 5))
})

