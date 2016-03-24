#' @name get_tcm
#' @title Extract term-co-occurence matrix
#' @description This function creates a term-co-occurence matrix from a
#'   \code{Corpus} object.
#' @param corpus \code{HashCorpus} or \code{VocabCorpus} object. See
#'   \link{create_corpus}, \link{vectorizers} for details.
#' @seealso \link{create_corpus}
#' @examples
#' \dontrun{
#' txt <- movie_review[['review']][1:1000]
#' it <- itoken(txt, tolower, word_tokenizer)
#' vocab <- create_vocabulary(it)
#' #remove very common and uncommon words
#' pruned_vocab = prune_vocabulary(vocab, term_count_min = 10, doc_proportion_max = 0.8,
#'                                 doc_proportion_min = 0.001, max_number_of_terms = 5000)
#'
#' vectorizer <- vocab_vectorizer(pruned_vocab, grow_dtm = FALSE, skip_grams_window = 5L)
#' it <- itoken(txt, tolower, word_tokenizer)
#' corpus <- create_corpus(it, vectorizer)
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
#' @title Term-co-occurence matrix construction
#' @description This is a high-level function for constructing a
#'   term-co-occurrence matrix. If a parallel backend is registered, it will
#'   construct the TCM in multiple threads.
#' @details The user should keep in mind that he or she should split data and
#'   and provide a list of \link{itoken} iterators. Each element of
#'   \code{itoken_src} will be handled in a separate thread combined at the end
#'   of processing.
#' @param itoken_src \code{list} of iterators over tokens from \link{itoken}.
#'   Each element is a list of tokens, that is, tokenized and normalized
#'   strings.
#' @param vectorizer \code{function} vectorizer function. See
#'   \link{vectorizers}.
#' @param ... arguments to \link{foreach} function which is used to iterate over
#'   \code{itoken_src}.
#' @return \code{dgCMatrix} TCM matrix
#' @seealso \link{itoken}
#' @examples
#' \dontrun{
#' data("movie_review")
#'
#' # single threadx
#'
#' tokens <- movie_review$review %>% tolower %>% word_tokenizer
#' it <- itoken(tokens)
#' v <- create_vocabulary(jobs)
#' vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)
#' tcm <- create_tcm(itoken(tokens), vectorizer)
#'
#' # parallel version
#'
#' # set to number of cores on your machine
#' N_WORKERS <- 1
#' splits <- split_into(movie_review$review, N_WORKERS)
#' jobs <- lapply(splits, itoken, tolower, word_tokenizer)
#' v <- create_vocabulary(jobs)
#' vectorizer <- vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)
#' jobs <- lapply(splits, itoken, tolower, word_tokenizer)
#' doParallel::registerDoParallel(N_WORKERS)
#' tcm <- create_tcm(jobs, vectorizer)
#' }
#' @export
create_tcm <- function(itoken_src, vectorizer, ...) {
  UseMethod("create_tcm")
}

#' @rdname create_tcm
#' @export
create_tcm.itoken <- function(itoken_src, vectorizer, ...) {
  corp <- vectorizer(itoken_src)
  # get it in triplet form - fastest and most
  # memory efficient way because internally it
  # kept in triplet form
  tcm <- get_tcm(corp)
  # remove corpus and trigger gc()!
  # this will release a lot of memory
  rm(corp); gc();
  tcm
}

#' @rdname create_tcm
#' @param verbose \code{logical} print status messages
#' @export
create_tcm.list <- function(itoken_src, vectorizer, verbose = FALSE, ...) {

  foreach(it = itoken_src,
          .combine = function(...) triplet_sum(..., verbose = verbose),
          .inorder = F,
          .multicombine = T,
          # user already made split for jobs
          # preschedule = FALSE is much more memory efficient
          .options.multicore = list(preschedule = FALSE),
          ...) %dopar%
          {
            create_tcm(it, vectorizer, ...)
          }
}

triplet_sum <- function(..., verbose = FALSE) {
  if (verbose)
    print(paste(Sys.time(), "got results from workers, call combine ..."))

  lst <- list(...)

  if (any(vapply(lst, is.null, FALSE)))
    stop('Got NULL from one of the jobs.
          Probably result size >= 2gb and package "parallel" can\'t collect results.
          Try to split input into more chunks (so result on each chunk must be < 2gb)')

  res <- uniqTsparse(Reduce(`+`, lst))
  # assume all matrices have same dimnames
  res@Dimnames <- lst[[1]]@Dimnames
  res
}

# multicore combine
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
# multicore version of triplet_sum
mc_triplet_sum <- function(...) {
  mc_reduce(list(...),
            FUN = function(a, b) uniqTsparse(a + b),
            .inorder = FALSE,
            .multicombine = TRUE)
}
