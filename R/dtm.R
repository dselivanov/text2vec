#' @name get_dtm
#' @title Creates Document-Term matrix
#' @description Creates Document-Term matrix from Corpus object.
#' @param corpus HashCorpus or VocabCorpus object.
#' See \link{create_corpus} for details.
#' @param type character, one of \code{c("dgCMatrix", "dgTMatrix", "lda_c")}.
#' "lda_c" - Blei's lda-c format (list of 2*doc_terms_size),
#' see \url{https://www.cs.princeton.edu/~blei/lda-c/readme.txt}
#' @examples
#' \dontrun{
#' N <- 1000
#' it <- itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer, chunks_number = 10)
#' v <- vocabulary(it, c(1L, 1L) )
#'
#' #remove very common and uncommon words
#' pruned_vocab = prune_vocabulary(v, term_count_min = 10,
#'  doc_proportion_max = 0.8, doc_proportion_min = 0.001,
#'  max_number_of_terms = 10000)
#'
#' vectorizer <- vocab_vectorizer(v)
#' it <- itoken(movie_review$review[1:N], preprocess_function = tolower,
#'              tokenizer = word_tokenizer, chunks_number = 10)
#' corpus <- create_corpus(it, vectorizer)
#' dtm <- get_dtm(corpus)
#'
#' tf_scale_matrix <- get_tf(dtm, type = 'tf')
#' dtm_tf <- tf_scale_matrix %*% dtm
#' dtm_tf_idf <- get_tf %*% m %*% get_idf(dtm)
#'
#' # The same result we can obtain using tfidf_transformer function
#' dtm_tf_idf_2 <- tfidf_transformer(dtm)
#' identical(dtm_tf_idf, dtm_tf_idf_2)
#' }
#' @export
get_dtm <- function(corpus, type = c("dgCMatrix", "dgTMatrix", "lda_c")) {
  if (inherits(corpus, 'Rcpp_VocabCorpus') || inherits(corpus, 'Rcpp_HashCorpus')) {
    type <- match.arg(type)
    dtm <- corpus$get_dtm()
    dtm@Dimnames[[1]] <- attr(corpus, 'ids')
    coerce_dgTMatrix(dtm, type)
  }
  else
    stop("corpus should be instance of Rcpp_HashCorpus or Rcpp_VocabCorpus classes")
}

#' @name create_dtm
#' @title Document-Term Matrix construction.
#' @description High-level function for Document-Term Matrix construction.
#' If parallel backend is registered, it will construct DTM in multiple threads.
#' @details User should keep in mind, that he/she should split data itself and
#' provide list of \link{itoken} iterators. Each element of \code{itoken_list}
#' will be handled in separate thread and at the end they will be combined.
#' @param itoken_list \code{list} of iterators over tokens - \code{itoken}.
#' Each element is a list of tokens = tokenized and normalized strings.
#' @param vectorizer \code{function} vectorizer function.
#' @param type character, one of \code{c("dgCMatrix", "dgTMatrix", "lda_c")}.
#' "lda_c" - Blei's lda-c format (list of 2*doc_terms_size),
#' see \url{https://www.cs.princeton.edu/~blei/lda-c/readme.txt}
#' @param ... - arguments to \link{foreach} function which is used to iterate
#' over \code{itoken_list} under the hood.
#' @return Document-Term Matrix
#' @seealso \link{itoken}
#' @examples
#' \dontrun{
#' data("movie_review")
#' # set to number of cores on your machine
#' N_WORKERS <- 1
#' splits <- split(movie_review$review, rep(1:N_WORKERS, each = nrow(movie_review) / N_WORKERS ))
#' jobs <- lapply(splits, itoken, tolower, word_tokenizer)
#' vectorizer <- hash_vectorizer()
#' doParallel::registerDoParallel(N_WORKERS)
#' dtm <- create_dtm(jobs, vectorizer, type = 'dgTMatrix')
#' }
#' @export
create_dtm <- function(itoken_list,
                       vectorizer,
                       type = c("dgCMatrix", "dgTMatrix", "lda_c"),
                       ...) {
  type <- match.arg(type)

  combine_fun <- switch(type,
                        dgCMatrix = rbind,
                        dgTMatrix = rbind_dgTMatrix,
                        lda_c = c)

  foreach(it = itoken_list,
        .combine = combine_fun,
        .inorder = T,
        .multicombine = T,
        ...) %dopar%
        {
          corp <- vectorizer(it)
          # get it in triplet form - fastest and most
          # memory efficient way because internally it
          # kept in triplet form
          dtm_chunk <- get_dtm(corp, 'dgTMatrix')
          # remove corpus and trigger gc()!
          # this will release a lot of memory
          rm(corp); gc();
          if (type != 'dgTMatrix')
            coerce_dgTMatrix(dtm_chunk, type)
          else
            dtm_chunk
        }
}
