logger = lgr::get_logger("text2vec")
logger$set_threshold("fatal")

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
  ptr_sw = text2vec:::create_xptr_unordered_set(character(0))
  x = text2vec:::collapse_collocations_cpp(list(c("c", "te", "d", "2", "2")), ptr, ptr_sw, sep)[[1]]
  expect_equal(c("c_te", "d", "2", "2" ), x)
  x = text2vec:::collapse_collocations_cpp(list(c("c", "te", "d", "ivoire", "2")), ptr, ptr_sw, sep)[[1]]
  expect_equal(c("c_te_d_ivoire", "2" ), x)
  x = text2vec:::collapse_collocations_cpp(list(c("c", "te", "d", "ivoire")), ptr, ptr_sw, sep)[[1]]
  expect_equal(c("c_te_d_ivoire"), x)

  expect_equal(length(setdiff(v2$term, v$term)), 44L)
  expect_equal(nrow(cc$collocation_stat), 46L)
  expect_true("jeroen_krabb" %in% v2$term)
  expect_true(all(cc$collocation_stat$n_ij >= 5))
})


test_that("collocations Dunning's LLR", {
  # Data taken from Dunning's original paper:
  # http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.5962
  tbl = '270.72 110 2442 111 29114 "the swiss"
  263.90 29 13 123 31612 "can be"
  256.84 31 23 139 31584 "previous year"
  167.23 10 0 3 31764 "mineral water"
  157.21 76 104 2476 29121 "at the"
  157.03 16 16 51 31694 "real terms"
  146.80 9 0 5 31763 "natural gas"
  115.02 16 0 865 30896 "owing to"
  104.53 10 9 41 31717 "health insurance"
  100.96 8 2 27 31740 "stiff competition"
  98.72 12 111 14 31640 "is likely"
  95.29 8 5 24 31740 "qualified personnel"
  94.50 10 93 6 31668 "an estimated"
  91.40 12 111 21 31633 "is expected"
  81.55 10 45 35 31687 "1 2"
  76.30 5 13 0 31759 "balance sheet"
  73.35 16 2536 1 29224 "the united"
  68.96 6 2 45 31724 "accident insurance"
  68.61 24 43 1316 30394 "terms of"
  61.61 3 0 0 31774 "natel c"
  60.77 6 92 2 31677 "will probably"
  57.44 4 11 1 31761 "great deal"
  57.44 4 11 1 31761 "government bonds"
  57.14 13 7 1327 30430 "part of"
  53.98 4 1 18 31754 "waste paper"
  53.65 4 13 2 31758 "machine exhibition"
  52.33 7 61 27 31682 "rose slightly"
  52.30 5 9 25 31738 "passenger service"
  49.79 4 61 0 31712 "not yet"
  48.94 9 12 429 31327 "affected by"
  48.85 13 1327 12 30425 "of september"
  48.80 9 4 872 30892 "continue to"
  47.84 4 41 1 31731 "2 nd"
  47.20 8 27 157 31585 "competition from"
  46.38 10 472 20 31275 "a positive"
  45.53 4 18 6 31749 "per 100"
  44.36 7 0 1333 30437 "course of"
  43.93 5 18 33 31721 "generally good"
  43.61 19 50 1321 30387 "level of"
  43.35 20 2532 25 29200 "the stock"
  43.07 6 875 0 30896 "to register"
  43.06 3 1 10 31763 "french speaking"
  41.69 3 29 0 31745 "3 rd"
  41.67 3 1 13 31760 "knitting machines"
  40.68 4 5 40 31728 "25 000"
  39.23 9 5 1331 30432 "because of"
  39.20 5 40 25 31707 "stock markets"
  38.87 2 0 1 31774 "scanner cash"
  38.79 3 0 48 31726 "pent up"
  38.51 3 23 1 31750 "firms surveyed"
  38.46 4 2 98 31673 "restaurant business"
  38.28 3 12 3 31759 "fell back"
  38.14 6 4 432 31335 "climbed by"
  37.20 6 41 70 31660 "total production"
  37.15 2 0 2 31773 "hay crop"
  36.98 3 10 5 31759 "current transactions"'

  contingency_tbl = read.table(text = tbl, header = FALSE, stringsAsFactors = FALSE)

  names(contingency_tbl) = c("LLR", "AB", "A~B", "~AB", "~A~B", "bigram")
  nword = contingency_tbl[["AB"]] + contingency_tbl[["A~B"]] +  contingency_tbl[["~AB"]] + contingency_tbl[["~A~B"]]
  n_i = contingency_tbl[["AB"]] + contingency_tbl[["A~B"]]
  n_j = contingency_tbl[["AB"]] + contingency_tbl[["~AB"]]
  n_ij = contingency_tbl[["AB"]]

  L_func = text2vec:::L_func

  test_llr = -2 * (  L_func(n_j / nword, n_i, n_ij) +
                   L_func(n_j / nword, nword - n_i, n_j - n_ij) -
                   L_func(n_ij / n_i, n_i, n_ij) -
                   L_func((n_j - n_ij) / (nword - n_i), nword - n_i, n_j - n_ij))


  expect_equal(test_llr, contingency_tbl[["LLR"]], tolerance = 1e-4)
})
