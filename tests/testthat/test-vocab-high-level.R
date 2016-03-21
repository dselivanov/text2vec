context("vocabulary-corpus construction")

get_test_iterator <- function(txt)
  itoken(txt,
         preprocess_function = tolower,
         tokenizer = word_tokenizer,
         progessbar = F)

train_ind <- 1:1000
N_WORKER <- 4

txt <- movie_review[['review']][train_ind]
ids <- movie_review[['id']][train_ind]

txt_splits <- text2vec:::split_into(txt, N_WORKER)


test_that("Vocabulary with foreach", {
  iterator <- get_test_iterator(txt)
  vocab_1 <- create_vocabulary(iterator)

  iterator_list <- lapply(txt_splits, get_test_iterator)
  vocab_2 <- create_vocabulary(iterator_list)

  expect_equal(sort(vocab_1$vocab$terms),  sort(vocab_2$vocab$terms))
  expect_equal(vocab_1$document_count,  vocab_2$document_count)
  expect_equal(vocab_1$document_count,  max(train_ind))
  expect_equal(vocab_1$ngram,  vocab_2$ngram)
})
