#' @name get_tcm
#' @title Creates Term-Coocurnce matrix construction
#' @description Creates Term-Coocurnce matrix from Corpus object.
#' @param corpus HashCorpus or VocabCorpus object.
#' See \link{create_corpus}, \link{vectorizers} for details.
#' @seealso \link{create_corpus}
#' @examples
#' \dontrun{
#' txt <- movie_review[['review']][1:1000]
#' it <- itoken(txt, tolower, word_tokenizer)
#' vocab <- vocabulary(it)
#' #remove very common and uncommon words
#' pruned_vocab = prune_vocabulary(vocab, term_count_min = 10,
#'  doc_proportion_max = 0.8, doc_proportion_min = 0.001, max_number_of_terms = 5000)
#'
#' it <- itoken(txt, tolower, word_tokenizer)
#' corpus <- create_vocab_corpus(it, pruned_vocab, grow_dtm = FALSE, skip_grams_window = 5)
#' tcm <- get_tcm(corpus)
#' dim(tcm)
#' }
#' @export
get_tcm <- function(corpus) {
  if (inherits(corpus, 'Rcpp_VocabCorpus') || inherits(corpus, 'Rcpp_HashCorpus')) {
    tcm <- corpus$get_tcm()
    dim_names <- colnames(tcm)
    dimnames(tcm) <- list(dim_names, dim_names)
    tcm
  }
  else
    stop("corpus should be Rcpp_HashCorpus class or Rcpp_VocabCorpus class")
}

#' @name create_tcm
#' @title Term-Cooccurence Matrix construction.
#' @description High-level function for Term-Cooccurence Matrix construction.
#' If parallel backend is registered, it will construct TCM in multiple threads.
#' @details User should keep in mind, that he/she should split data itself and
#' provide list of \link{itoken} iterators. Each element of \code{itoken_list}
#' will be handled in separate thread and at the end they will be combined.
#' @param itoken_list \code{list} of iterators over tokens - \code{itoken}.
#' Each element is a list of tokens = tokenized and normalized strings.
#' @param vectorizer \code{function} vectorizer function.
#' @param ... - arguments to \link{foreach} function which is used to iterate
#' over \code{itoken_list} under the hood.
#' @return \code{dgCMatrix} Term-Cooccurence Matrix
#' @seealso \link{itoken}
#' @examples
#' \dontrun{
#' data("movie_review")
#' # set to number of cores on your machine
#' N_WORKERS <- 1
#' splits <- split(movie_review$review, rep(1:N_WORKERS, each = nrow(movie_review) / N_WORKERS ))
#' jobs <- lapply(splits, itoken, tolower, word_tokenizer)
#' v <- vocabulary(jobs, c(1L, 1L) )
#' vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)
#' jobs <- lapply(splits, itoken, tolower, word_tokenizer)
#' doParallel::registerDoParallel(N_WORKERS)
#' tcm <- create_tcm(jobs, vectorizer)
#' }
#' @export
create_tcm <- function(itoken_list,
                       vectorizer,
                       ...) {

  combine_fun <- function(...) {
    Reduce(`+`, list(...))
  }

  foreach(it = itoken_list,
          .combine = mc_sum,
          .inorder = F,
          .multicombine = T,
          ...) %dopar%
          {
            corp <- vectorizer(it)
            # get it in triplet form - fastest and most
            # memory efficient way because internally it
            # kept in triplet form
            dtm_chunk <- get_tcm(corp)
            # remove corpus and trigger gc()!
            # this will release a lot of memory
            rm(corp); gc();
            as(dtm_chunk, 'dgCMatrix')
          }
}

mc_reduce <- function(X, FUN,  ...) {
  if (length(X) >= 2) {
    # split into pairs of elements
    pairs <- split(X, ceiling(seq_along(X) / 2))

    X_NEW <-
      foreach( pair = pairs, ...) %dopar% {
        # if length(X) is odd, we will recieve several pairs + single element
        if (length(pair) == 1)
          pair[[1]]
        else {
          FUN(pair[[1]], pair[[2]])
        }
      }
    # recursive call
    mc_reduce(X_NEW, FUN = FUN, ...)
  }
  # finally reduce to single element
  else
    X[[1]]
}

mc_sum <- function(...) {
  mc_reduce(list(...),
            FUN = function(a, b) {a + b},
           .inorder = FALSE,
           .multicombine = TRUE)
}
