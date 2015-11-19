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
#' @param  ... arguments passed to other methods (inculding \link{write_rds} function).
#' @return \code{text2vec_vocabulary} object,
#' which is actually a \code{list} with following fields:
#'
#' 1. \bold{vocab} - \code{data.frame} which contains columns
#'\itemize{
#'  \item{\code{terms}       }{ \code{character} vector of unique terms}
#'  \item{\code{terms_counts} }{ \code{integer} vector of term counts across all documents}
#'  \item{\code{doc_counts}  }{ \code{integer} vector of document counts
#'                            that contain corresponding term}
#'  \item{\code{doc_proportions}   }{ \code{numeric} vector of document proportions
#'                            that contain corresponding term}
#'}
#'
#' 2. \bold{ngram} - \code{integer} vector, the lower and upper boundary of the range of n-gram-values.
#'
#' @examples
#' data("movie_review")
#' txt <- movie_review[['review']][1:100]
#' it <- itoken(txt, tolower, word_tokenizer, chunks_number = 10)
#' vocab <- vocabulary(it)
#' pruned_vocab = prune_vocabulary(vocab, term_count_min = 10,
#'  doc_proportion_max = 0.8, doc_proportion_min = 0.001, max_number_of_terms = 20000)
#' @export
vocabulary <- function(src,
                       ngram = c('ngram_min' = 1L, 'ngram_max' = 1L),
                       ...) {
  UseMethod("vocabulary")
}

#' @describeIn vocabulary creates \code{text2vec_vocabulary} from predefined
#' character vector. Terms will be inserted \bold{as is}, without any checks
#' (ngrams numner, ngram delimiters, etc.).
#' @export
vocabulary.character <- function(src,
                                 ngram = c('ngram_min' = 1L, 'ngram_max' = 1L),
                                 ...) {

  ngram_min <- as.integer( ngram[[1]] )
  ngram_max <- as.integer( ngram[[2]] )
  vocab_length = length(src)

  res <- list(
    # keep structure similar to `vocabulary.itoken` object. not used at the moment,
    # but we should keep same structure (keep in mind prune_vocabulary)
    vocab = data.frame('terms' = src,
                       'terms_counts' = rep(NA_integer_, vocab_length),
                       'doc_counts' = rep(NA_integer_, vocab_length),
                       'doc_proportions' = rep(NA_real_, vocab_length),
                       stringsAsFactors = FALSE
                       ),
    ngram = c('ngram_min' = ngram_min, 'ngram_max' = ngram_max)
  )

  class(res) <- c('text2vec_vocabulary')
  res
}

#' @describeIn vocabulary collects unique terms and corresponding statistics from object.
#' @param serialize_dir As a \bold{side effect}, we can save tokenized texts in serialized form to disk.
#' So, \code{serialize_dir} is a \code{character} - path where to save tokenized input files.
#' @export
vocabulary.itoken <- function(src,
                              ngram = c('ngram_min' = 1L, 'ngram_max' = 1L),
                              serialize_dir = NULL, ...) {
  ngram_min <- as.integer( ngram[[1]] )
  ngram_max <- as.integer( ngram[[2]] )
  vocab <- new(VocabularyBuilder, ngram_min, ngram_max)
  i <- 1

  write_tokens <- FALSE
  if ( !is.null(serialize_dir) )
    if ( !dir.exists(serialize_dir) )
      stop("serialize_dir does not exist")
  else {
    base_path <- paste0(serialize_dir, '/part-')
    write_tokens <- TRUE
  }

  while (TRUE) {
    # iterate
    tokens = try(nextElem(src), silent = TRUE)
    # check end of iterator
    if (class(tokens) == "try-error") {
      if (attributes(tokens)$condition$message == "StopIteration")
        break
      # handle other errors
      else
        stop(attributes(tokens)$condition$message)
    }
    # insert into vocabulary
    vocab$insert_document_batch(tokens)
    # write tokens to disk
    if (write_tokens)
      write_rds(tokens, paste0(base_path, i), ...)
    i <- i + 1
  }

  res <- list(
    vocab = vocab$get_vocab_statistics(),
    ngram = c('ngram_min' = ngram_min, 'ngram_max' = ngram_max))

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
#' @param max_number_of_terms maximum number of terms in vocabulary.
#' @seealso \link{vocabulary}
#' @export
prune_vocabulary <- function(vocabulary,
                  term_count_min = 1L,
                  term_count_max = Inf,
                  doc_proportion_min = 0.0,
                  doc_proportion_max = 1.0,
                  max_number_of_terms = Inf) {
  if (!inherits(vocabulary, 'text2vec_vocabulary'))
    stop('vocabulary should be an object of class text2vec_vocabulary')

  vocab <- vocabulary$vocab

  ind <- vocab[['terms_counts']] >= term_count_min &
    vocab[['terms_counts']] <= term_count_max &
    vocab[['doc_proportions']] >= doc_proportion_min &
    vocab[['doc_proportions']] <= doc_proportion_max

  pruned_vocabulary <- vocab[ind, ]
  pruned_vocabulary <- pruned_vocabulary[order(pruned_vocabulary$terms_counts, decreasing = T),]

  max_number_of_terms <- min(max_number_of_terms, nrow(pruned_vocabulary))
  pruned_vocabulary <- list('vocab' = pruned_vocabulary[1:max_number_of_terms, ],
                            'ngram' = vocabulary[['ngram']])

  class(pruned_vocabulary) <- 'text2vec_vocabulary'

  pruned_vocabulary
}
