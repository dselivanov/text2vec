#' @name vocabulary
#' @title Creates vocabulary (unique terms)
#' @description collects unique terms and corresponding statistics from object.
#' See \code{value} section.
#' @param src iterator over \code{list} of \code{character} vectors - documents from which
#' user want construct vocabulary. Or, alternatively,
#' \code{character} vector = user-defined vocabulary terms (which will be used "as is").
#' @param ngram \code{integer} vector. The lower and upper boundary of the range of
#' n-values for different n-grams to be extracted. All values of n such that
#' ngram_min <= n <= ngram_max will be used.
#' @return \code{text2vec_vocabulary} object,
#' which is actually a \code{list} with following fields:
#'
#' 1. \bold{vocab} - \code{data.frame} which contains columns
#'\itemize{
#'  \item{\code{terms}       }{ \code{character} vector of unique terms}
#'  \item{\code{terms_counts} }{ \code{integer} vector of term counts across all documents}
#'  \item{\code{doc_counts}  }{ \code{integer} vector of document counts
#'                            that contain corresponding term}
#'}
#'
#' 2. \bold{ngram} - \code{integer} vector, the lower and upper boundary of the range of n-gram-values.
#' 3. \bold{document_count} - \code{integer} number of documents vocabulary was built.
#'
#' @examples
#' data("movie_review")
#' txt <- movie_review[['review']][1:100]
#' it <- itoken(txt, tolower, word_tokenizer, chunks_number = 10)
#' vocab <- vocabulary(it)
#' pruned_vocab = prune_vocabulary(vocab, term_count_min = 10,
#'  doc_proportion_max = 0.8, doc_proportion_min = 0.001, max_number_of_terms = 20000)
#' @export
vocabulary <- function(src, ngram = c('ngram_min' = 1L, 'ngram_max' = 1L)) {
  UseMethod("vocabulary")
}

#' @describeIn vocabulary creates \code{text2vec_vocabulary} from predefined
#' character vector. Terms will be inserted \bold{as is}, without any checks
#' (ngrams numner, ngram delimiters, etc.).
#' @export
vocabulary.character <- function(src, ngram = c('ngram_min' = 1L, 'ngram_max' = 1L)) {

  ngram_min <- as.integer( ngram[[1]] )
  ngram_max <- as.integer( ngram[[2]] )
  vocab_length = length(src)

  res <- list(
    # keep structure similar to `vocabulary.itoken` object. not used at the moment,
    # but we should keep same structure (keep in mind prune_vocabulary)
    vocab = data.frame('terms' = src,
                       'terms_counts' = rep(NA_integer_, vocab_length),
                       'doc_counts' = rep(NA_integer_, vocab_length),
                       # 'doc_proportions' = rep(NA_real_, vocab_length),
                       stringsAsFactors = FALSE
                       ),
    ngram = c('ngram_min' = ngram_min, 'ngram_max' = ngram_max),
    document_count = NA_integer_
  )

  class(res) <- c('text2vec_vocabulary')
  res
}

#' @describeIn vocabulary collects unique terms and corresponding statistics from object.
#' @export
vocabulary.itoken <- function(src, ngram = c('ngram_min' = 1L, 'ngram_max' = 1L)) {
  ngram_min <- as.integer( ngram[[1]] )
  ngram_max <- as.integer( ngram[[2]] )
  vocab <- new(VocabularyBuilder, ngram_min, ngram_max)

  foreach(tokens = src) %do% {
    vocab$insert_document_batch(tokens)
  }

  res <- list(
    vocab = vocab$get_vocab_statistics(),
    ngram = c('ngram_min' = ngram_min, 'ngram_max' = ngram_max),
    document_count = vocab$get_document_count()
  )

  class(res) <- c('text2vec_vocabulary')
  res
}

#' @describeIn vocabulary collects unique terms and corresponding statistics from
#' list of itoken iterators. If parallel backend is registered, it will build vocabulary
#' in parallel using \link{foreach}.
#' @param ... additional arguments to \link{foreach} function.
#' @export
vocabulary.list <- function(src, ngram = c('ngram_min' = 1L, 'ngram_max' = 1L),  ...) {
  stopifnot( all( vapply(X = src, FUN = inherits, FUN.VALUE = FALSE, "itoken") ) )

  foreach(it = src,
          .combine = combine_vocabulary,
          .inorder = FALSE,
          .multicombine = TRUE,
          ...) %dopar%
          {
            vocabulary(it, ngram)
          }
}

combine_vocabulary <- function(...) {
  vocab_list <- list(...)
  ngram <- vocab_list[[1]][['ngram']]

  # extract vocabulary stats data.frame and rbind them
  combined_vocab <- vocab_list %>%
    lapply(function(x) x[['vocab']]) %>%
    rbindlist

  # reduce by terms
  combined_vocab <- combined_vocab[, .('terms_counts' = sum(terms_counts),
                                       'doc_counts' = sum(doc_counts)),
                                   by = terms]
  setcolorder(combined_vocab, c('terms', 'terms_counts', 'doc_counts'))

  combined_document_count <- sum(vapply(vocab_list, function(x) x[['document_count']], FUN.VALUE = NA_integer_))

  res <- list(
    vocab = combined_vocab,
    ngram = ngram,
    document_count = combined_document_count
  )
  class(res) <- c('text2vec_vocabulary')
  res
}

#' @name prune_vocabulary
#' @title Prunes vocabulary.
#' @description Perform filtering of the input vocabulary and trhrowing out very frequet and
#' very infrequet terms. Also leaves top `max_number_of_terms` (by count) terms. See examples
#' in \link{vocabulary} function.
#' @param vocabulary vocabulary from \link{vocabulary} function output.
#' @param term_count_min minimum number of occurences over all documents.
#' @param term_count_max maximum number of occurences over all documents.
#' @param doc_proportion_min minimum proportion of documents which should contain term.
#' @param doc_proportion_max maximum proportion of documents which should contain term.
#' @param stop_words - \code{character} vector. Word to remove from vocabulary.
#' @param max_number_of_terms maximum number of terms in vocabulary.
#' @seealso \link{vocabulary}
#' @export
prune_vocabulary <- function(vocabulary,
                  term_count_min = 1L,
                  term_count_max = Inf,
                  doc_proportion_min = 0.0,
                  doc_proportion_max = 1.0,
                  stop_words = character(0),
                  max_number_of_terms = Inf) {

  if (!inherits(vocabulary, 'text2vec_vocabulary'))
    stop('vocabulary should be an object of class text2vec_vocabulary')

  vocab_df <- vocabulary$vocab
  vocab_size <- nrow(vocab_df)

  douments_count <- vocabulary[['document_count']]

  ind <- rep(TRUE, vocab_size)

  doc_proportion <- NULL

  if (term_count_min > 1L)
    ind <- ind & (vocab_df[['terms_counts']] >= term_count_min)
  if (is.finite(term_count_max))
    ind <- ind & (vocab_df[['terms_counts']] <= term_count_max)

  if (doc_proportion_min > 0) {
    doc_proportion <- vocab_df[['doc_counts']] / douments_count
    ind <- ind & (doc_proportion >= doc_proportion_min)
  }

  if (doc_proportion_max < 1.0) {
    # not calculated in prev ster
    if (is.null(doc_proportion))
      doc_proportion <- vocab_df[['doc_counts']] / douments_count

    ind <- ind & (doc_proportion <= doc_proportion_max)
  }

  pruned_vocabulary <- vocab_df[ind, ]

  # remove stop words if asked
  if (is.character(stop_words) && length(stop_words) > 0) {
    non_stop_words_ind <- !(pruned_vocabulary$terms %in% stop_words)
    pruned_vocabulary <- pruned_vocabulary[non_stop_words_ind, ]
  }

  # restrict to max number if asked
  if (is.finite(max_number_of_terms)) {
    pruned_vocabulary <- pruned_vocabulary[order(pruned_vocabulary$terms_counts, decreasing = T),]
    max_number_of_terms <- min(max_number_of_terms, nrow(pruned_vocabulary))
    pruned_vocabulary <- pruned_vocabulary[1:max_number_of_terms, ]
  }

  pruned_vocabulary <- list('vocab' = pruned_vocabulary,
                            'ngram' = vocabulary[['ngram']],
                            'document_count' = vocabulary[['document_count']])

  class(pruned_vocabulary) <- 'text2vec_vocabulary'

  pruned_vocabulary
}
