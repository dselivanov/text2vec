# library(microbenchmark)
# x <- list(letters[c(1:15, 10:20)])
# 15 times faster
# microbenchmark::microbenchmark(
#   r2v <- r2vec::find_ngrams(dat = x, n = 3, verbose = F)[[1]],
#   tmlite <- tmlite::ngram(x[[1]], n_min = 1, n_max = 3, delim = ' ')
# )
# setdiff(tmlite, r2v)
