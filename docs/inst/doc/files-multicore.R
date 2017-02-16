## ----global_options, include=FALSE, echo=FALSE---------------------------
knitr::opts_chunk$set(echo=TRUE, eval=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(text2vec)
library(magrittr)
data("movie_review")

# remove all internal EOL to simplify reading
movie_review$review = gsub(pattern = '\n', replacement = ' ', 
                            x = movie_review$review, fixed = TRUE)
N_FILES = 10
CHUNK_LEN = nrow(movie_review) / N_FILES
files = sapply(1:N_FILES, function(x) tempfile())
chunks = split(movie_review, rep(1:N_FILES, 
                                  each = nrow(movie_review) / N_FILES ))
for (i in 1:N_FILES ) {
  write.table(chunks[[i]], files[[i]], quote = T, row.names = F,
              col.names = T, sep = '|')
}

# Note what the moview review data looks like
str(movie_review, strict.width = 'cut')

## ------------------------------------------------------------------------
library(data.table)
reader = function(x, ...) {
  # read
  chunk = data.table::fread(x, header = T, sep = '|')
  # select column with review
  res = chunk$review
  # assign ids to reviews
  names(res) = chunk$id
  res
}
# create iterator over files
it_files  = ifiles(files, reader = reader)
# create iterator over tokens from files iterator
it_tokens = itoken(it_files, preprocess_function = tolower, 
                    tokenizer = word_tokenizer, progessbar = FALSE)

vocab = create_vocabulary(it_tokens)

## ------------------------------------------------------------------------
dtm = create_dtm(it_tokens, vectorizer = vocab_vectorizer(vocab))
str(dtm, list.len = 5)

## ------------------------------------------------------------------------
for (i in 1:N_FILES ) {
  write.table(chunks[[i]][["review"]], files[[i]], quote = T, row.names = F,
              col.names = T, sep = '|')
}
# read with default reader - readLines
it_files  = ifiles(files)
# create iterator over tokens from files iterator
it_tokens = itoken(it_files, preprocess_function = tolower, 
                    tokenizer = word_tokenizer, progessbar = FALSE)
dtm = create_dtm(it_tokens, vectorizer = hash_vectorizer())
str(dtm, list.len = 5)

## ---- warning=FALSE, message=FALSE, eval=FALSE---------------------------
#  N_WORKERS = 4
#  library(doParallel)
#  # register parallel backend
#  registerDoParallel(N_WORKERS)
#  
#  #  prepare splits
#  # "jobs" is a list of itoken iterators!
#  N_SPLITS = 4
#  
#  jobs = files %>%
#    split_into(N_SPLITS) %>%
#    lapply(ifiles, reader = reader) %>%
#    # Worth to set chunks_number to 1 because we already splitted input
#    lapply(itoken, chunks_number = 1, preprocess_function = tolower,
#           tokenizer = word_tokenizer, progessbar = FALSE)
#  
#  # Alternatively when data is in memory we can perform splite in the following way:
#  #
#  # review_chunks = split_into(movie_review$review, N_SPLITS)
#  # review_ids = split_into(movie_review$id, N_SPLITS)
#  #
#  # jobs = Map(function(doc, ids) {
#  #  itoken(iterable = doc, ids = ids, preprocess_function = tolower,
#  #         tokenizer = word_tokenizer, chunks_number = 1, progessbar = FALSE)
#  # }, review_chunks, review_ids)
#  
#  # Now all below function calls will benefit from multicore machines
#  # Each job will be evaluated in separate process
#  
#  # vocabulary creation
#  vocab = create_vocabulary(jobs)
#  
#  # DTM vocabulary vectorization
#  v_vectorizer = vocab_vectorizer(vocab)
#  vocab_dtm_parallel = create_dtm(jobs, vectorizer = v_vectorizer)
#  
#  # DTM hash vectorization
#  h_vectorizer = hash_vectorizer()
#  hash_dtm_parallel = create_dtm(jobs, vectorizer = h_vectorizer)
#  
#  # co-ocurence statistics
#  tcm_vectorizer = vocab_vectorizer(vocab, grow_dtm = FALSE, skip_grams_window = 5)
#  tcm_parallel = create_tcm(jobs, vectorizer = tcm_vectorizer)

