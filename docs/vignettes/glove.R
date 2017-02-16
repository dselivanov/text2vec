## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
#  library(text2vec)
#  text8_file = "~/text8"
#  if (!file.exists(text8_file)) {
#    download.file("http://mattmahoney.net/dc/text8.zip", "~/text8.zip")
#    unzip ("~/text8.zip", files = "text8", exdir = "~/")
#  }
#  wiki = readLines(text8_file, n = 1, warn = FALSE)

## ------------------------------------------------------------------------
#  # Create iterator over tokens
#  tokens <- space_tokenizer(wiki)
#  # Create vocabulary. Terms will be unigrams (simple words).
#  it = itoken(tokens, progressbar = FALSE)
#  vocab <- create_vocabulary(it)

## ------------------------------------------------------------------------
#  vocab <- prune_vocabulary(vocab, term_count_min = 5L)

## ------------------------------------------------------------------------
#  # Use our filtered vocabulary
#  vectorizer <- vocab_vectorizer(vocab)
#  # use window of 5 for context words
#  tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

## ---- message=TRUE-------------------------------------------------------
#  glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
#  glove$fit(tcm, n_iter = 20)
#  # 2016-10-03 10:09:14 - epoch 1, expected cost 0.0893
#  # 2016-10-03 10:09:17 - epoch 2, expected cost 0.0608
#  # 2016-10-03 10:09:19 - epoch 3, expected cost 0.0537
#  # 2016-10-03 10:09:22 - epoch 4, expected cost 0.0499
#  # 2016-10-03 10:09:25 - epoch 5, expected cost 0.0475
#  # 2016-10-03 10:09:28 - epoch 6, expected cost 0.0457
#  # 2016-10-03 10:09:30 - epoch 7, expected cost 0.0443
#  # 2016-10-03 10:09:33 - epoch 8, expected cost 0.0431
#  # 2016-10-03 10:09:36 - epoch 9, expected cost 0.0423
#  # 2016-10-03 10:09:39 - epoch 10, expected cost 0.0415
#  # 2016-10-03 10:09:42 - epoch 11, expected cost 0.0408
#  # 2016-10-03 10:09:44 - epoch 12, expected cost 0.0403
#  # 2016-10-03 10:09:47 - epoch 13, expected cost 0.0400
#  # 2016-10-03 10:09:50 - epoch 14, expected cost 0.0395
#  # 2016-10-03 10:09:53 - epoch 15, expected cost 0.0391
#  # 2016-10-03 10:09:56 - epoch 16, expected cost 0.0388
#  # 2016-10-03 10:09:59 - epoch 17, expected cost 0.0385
#  # 2016-10-03 10:10:02 - epoch 18, expected cost 0.0383
#  # 2016-10-03 10:10:05 - epoch 19, expected cost 0.0380
#  # 2016-10-03 10:10:08 - epoch 20, expected cost 0.0378

## ---- message=TRUE, eval=FALSE-------------------------------------------
#  glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
#  # `glove` object will be modified by `fit()` call !
#  fit(tcm, glove, n_iter = 20)

## ------------------------------------------------------------------------
#  word_vectors <- glove$get_word_vectors()

## ------------------------------------------------------------------------
#  berlin <- word_vectors["paris", , drop = FALSE] -
#    word_vectors["france", , drop = FALSE] +
#    word_vectors["germany", , drop = FALSE]
#  cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
#  head(sort(cos_sim[,1], decreasing = TRUE), 5)
#  # berlin     paris    munich    leipzig   germany
#  # 0.8015347 0.7623165 0.7013252 0.6616945 0.6540700

