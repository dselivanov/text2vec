# library(text2vec)
# text8_file = "~/text8"
# wiki = readLines(text8_file, n = 1, warn = FALSE)
# ## ------------------------------------------------------------------------
# # Create iterator over tokens
# tokens <- space_tokenizer(wiki)
# # Create vocabulary. Terms will be unigrams (simple words).
# it = itoken(tokens, progressbar = FALSE)
# vocab <- create_vocabulary(it)
# vocab <- prune_vocabulary(vocab, term_count_min = 5L)
#
# ## ------------------------------------------------------------------------
# # Use our filtered vocabulary
# vectorizer <- vocab_vectorizer(vocab, grow_dtm = FALSE,skip_grams_window = 1L, skip_grams_window_context = "right")
# system.time(tcm <- create_tcm(it, vectorizer))
# dt2 = data.table(i = tcm@i + 1, j = tcm@j + 1, n_ij = tcm@x,
#                  n_i = vocab$vocab$terms_counts[tcm@i + 1],
#                  n_j = vocab$vocab$terms_counts[tcm@j + 1] )
#
# nword = sum(vocab$vocab$terms_counts)
#
# min_count = 100
# dt2[, pmi :=  log2( (n_ij / nword)    / ((n_i / nword) * (n_j / nword)))]
# dt2[, w2v := (n_ij - min_count) * nword / (as.numeric(n_i) * n_j)]
# dt2[, LFMD := log2( (n_ij / nword)**2 / ((n_i / nword) * (n_j / nword))) + log2(n_ij / nword)]
# # cnt(a, b) - min_count) * N / (cnt(a) * cnt(b)) > threshold
# # dt22 = dt2[n_ij >= 50 & pmi > 4][order(-LFMD)]
# # dt22 = dt2[n_ij >= 50 & pmi > 4][order(-pmi)]
# # dt22 = dt2[n_ij >= 50 & w2v > 0][order(-w2v)]
#
# # dt3 = dt22[i == which(vocab$vocab$terms == "los")]
# # k= 1:min(50, nrow(dt3))
# dt3 = dt22
# k = (nrow(dt3) - 5):(nrow(dt3))
# k = 1:100
# for(i in k) {
#   print(paste(vocab$vocab$terms[c(dt3$i[i], dt3$j[i])], collapse = "_"))
# }
#
# # mapply(function(ii) paste(vocab$vocab$terms(dt3$i[ii], dt3$j[ii])), k)
# # vocab$vocab$terms[c(dt3$i[k], dt3$j[k])]
