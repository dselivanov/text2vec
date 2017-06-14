# library(text2vec)
# data("movie_review")
#
# it <- itoken(movie_review[['review']], preprocess_function = tolower,
#              tokenizer = word_tokenizer, n_chunks = 10, progessbar = F)
# # using unigrams here
# t1 <- Sys.time()
# vocab <- vocabulary(src = it, ngram = c(1L, 1L))
# print( difftime( Sys.time(), t1, units = 'sec'))
#
# pruned_vocab <- prune_vocabulary(vocab, term_count_min = 10,
#                                  doc_proportion_max = 0.5, doc_proportion_min = 0.001)
#
# it <- itoken(movie_review[['review']], preprocess_function = tolower,
#              tokenizer = word_tokenizer, n_chunks = 10, progessbar = F)
#
# corpus <- create_vocab_corpus(it, vocabulary = pruned_vocab, grow_dtm = T,
#                               skip_grams_window = 5,
#                               tcm_disk_dump_threshold = .Machine$integer.max)
# dtm <- get_dtm(corpus)
#
# tcm <- get_tcm(corpus)
#
# m <- as(rbind(tcm, dtm), 'dgTMatrix')
# rownames(m) <- c(rownames(tcm), as.character(1:nrow(dtm)))
#
# m2 <- m %>%
#   mutate(transformer = tfidf_transformer )
#
#
# Sys.time()
# fit <- glove(tcm = m2, word_vectors_size = 50, x_max = 30,  num_iters = 0, shuffle = T,
#              learning_rate = 0.1, convergence_threshold = 0.005)
# Sys.time()
#
# m1 <- fit$word_vectors$w_i + fit$word_vectors$w_j
#
# # fit <- glmnet::cv.glmnet(x = m1[-1:(-ncol(m)), ], y = movie_review[['sentiment']],
# fit <- glmnet::cv.glmnet(x = dtm, y = movie_review[['sentiment']],
#                  family = 'binomial',
#                  # lasso penalty
#                  alpha = 1,
#                  # interested area unded ROC curve
#                  type.measure = "auc",
#                  # 5-fold cross-validation
#                  nfolds = 5,
#                  # high value, less accurate, but faster training
#                  thresh = 1e-5,
#                  # again lower number iterations for faster training
#                  # in this vignette
#                  maxit = 1e4)
# plot(fit)
