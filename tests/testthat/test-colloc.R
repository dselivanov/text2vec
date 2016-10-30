context("collocations")
test_that("collocations internalfunctions", {
  sep = "_"
  ptr = text2vec:::create_xptr_unordered_set(c("c_te", "d_ivoire", "c_te_d_ivoire"))
  x = text2vec:::collapse_collocations_cpp(list(c("c", "te", "d", "2", "2")), ptr, sep)[[1]]
  expect_equal(c("c_te", "d", "2", "2" ), x)
  x = text2vec:::collapse_collocations_cpp(list(c("c", "te", "d", "ivoire", "2")), ptr, sep)[[1]]
  expect_equal(c("c_te_d_ivoire", "2" ), x)
  x = text2vec:::collapse_collocations_cpp(list(c("c", "te", "d", "ivoire")), ptr, sep)[[1]]
  expect_equal(c("c_te_d_ivoire"), x)
})

