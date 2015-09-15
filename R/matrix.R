#' @name get_dtm
#' @title DTM ( TDM ) construction
#' @description Creates Document-Term-Matrix/Term-Document-Matrix from TmliteCorpus.
#' @param corpus - TmliteCorpus input corpus.
#' It can be obtained via \link{create_dict_corpus} function
#' @param dictionary - \link{character} or \link{NULL} -  use only words from this dict
#' dictionary in Document-Term-Matrix construction.
#' NULL if all words should be used.
#' @param stopwords - \link{character} or \link{NULL} - words to remove from DTM ( TDM )
#' @examples
#' preprocess_fun <- function(txt) {
#'    txt %>%
#'      tolower %>%
#'      # keep only words with latin letters
#'      gsub(pattern = "[^a-z]", replacement = " ", x = .) %>%
#'      # strip whitespaces
#'      gsub(pattern = "\\s+", replacement = " ", x = .)
#' }
#' stemfun <- function(txt_char_vec, lang = 'en')
#'  lapply(txt_char_vec, function(x) SnowballC::wordStem(x, language = lang))
#' # or use simple_preprocess() insted
#' txt <- c(paste(letters[c(4:7, 5:12)], collapse = " "), paste(LETTERS[c(5:9, 7:12) ], collapse = " "))
#' corpus <- create_dict_corpus(txt,
#'    preprocess_fun = preprocess_fun,
#'    tokenizer = simple_tokenizer,
#'    stemming_fun = stemfun,
#'    batch_size = 1
#'    )
#' dtm <- get_dtm(corpus, dictionary = letters[4:8], stopwords = letters[5:6] )
#' tdm <- get_tdm(corpus, dictionary = letters[4:8], stopwords = letters[5:6] )
#' @export
get_dtm <- function(corpus, dictionary = NULL, stopwords = NULL,
                    type = c("dgCMatrix", "dgTMatrix", "LDA_C")) {
  type <- match.arg(type)
  dtm <- switch(type,
           dgCMatrix = as(corpus$get_dtm(0L), "dgCMatrix"),
           dgTMatrix = corpus$get_dtm(0L),
           LDA_C = corpus$get_dtm(1L),
           NULL
           )
  if(type != 'LDA_C') {
    terms <- dtm@Dimnames[[2]]
    terms_len <- dtm@Dim[[2]]

    if(is.character(dictionary) && length(dictionary) > 0)
      ind_dict <- terms %in% dictionary
    else ind_dict <- rep(T, terms_len)

    if(is.character(stopwords) && length(stopwords) > 0)
      ind_stop <- !(terms %in% stopwords)
    else ind_stop <- rep(T, terms_len)

    ind <- ind_stop & ind_dict

    if( sum(ind) < terms_len)
      dtm[,ind]
    else
      dtm
  } else {
    if(any( !is.null(dictionary),  !is.null(dictionary) ))
      warning("for LDA-C format we currently didn't filter stopwords, and don't use dictionary")
    dtm
  }
}

#' @describeIn get_dtm
#' @export
get_tdm <- function(corpus, dictionary = NULL, stopwords = NULL) {
  t(get_dtm(corpus, dictionary = dictionary, stopwords = stopwords))
}

#' @name dtm_get_idf
#' @title Inverse Document-Frequency scaling matrix construction
#' @description Creates Inverse Document-Frequency (idf) scaling matrix
#' from Document-Term-Matrix.
#' \deqn{idf = \log(\fraq {#documents in the corpus}{# documents where the term + 1}}{%
#' idf = log (# documents in the corpus) / (# documents where the term appears) }
#' For examples see  \link{get_dtm}
#' @param m \link{sparseMatrix} - DocumentTermMatrix.
#' @param logScale function to use in idf calculation. Usually \link{log2} used.
#' @export
dtm_get_idf <- function(dtm, logScale = log2)
                    #type = c('idf', 'idfProb'))
                    # type = 'idf')
{
  dtm@x = rep(1, length(dtm@x))
  cs <- colSums(dtm)
  idf <- logScale(nrow(dtm) / (cs + 1))
  #, idfProb = pmax(0.01, logScale((nrow(dtm) - cs + 0.5) / (cs + 0.5)))
  Diagonal(dim(dtm)[2], idf)
}

#' @name dtm_get_tf
#' @title TermFrequency scaling matrix construction from Document-Term-Matrix
#' @description Creates TermFrequency (tf) scaling matrix from Document-Term-Matrix
#' @param type type of scaling. Formula for tf :
#' \deqn{tf = \fraq {# word appears in document}{# words in document}}{%
#' tf = (# word appears in document) / (# words in document) }
#  For boolean:
#' \deqn{tf = {Does word appears in document (boolean encoding): 0 if not appears, 1 if appears}}{%
#' tf = (Does word appears in document (boolean encoding): 0 if not appears, 1 if appears)}
#' @param dtm \link{sparseMatrix} - Document-Term-Matrix
#' @param type \code{c('tf', 'boolean')} - type of TF scaling matrix
#' @examples
#' txt <- c(paste(letters[c(4:7, 5:12)], collapse = " "), paste(LETTERS[c(5:9, 7:12) ], collapse = " "))
#' corpus <- create_dict_corpus(txt,
#'    tokenizer = simple_tokenizer
#'    )
#' dtm <- get_dtm(corpus, dictionary = letters[4:8], stopwords = letters[5:6] )
#' tf_scale_matrix <- dtm_get_tf(dtm, type = 'tf')
#' dtm_tf <- tf_scale_matrix %*% dtm
#' dtm_tf_idf <- dtm_get_tf %*% m %*% dtm_get_idf(dtm)
#' # The same result we can obtain using transform_dtm function with parameter type = 'tfidf'
#' dtm_tf_idf_2 <- transform_dtm(dtm, type='tfidf')
#' identical(dtm_tf_idf, dtm_tf_idf_2)
#' @export
dtm_get_tf <- function(dtm, type = c('tf', 'boolean'))
{
  type <- match.arg(type)
  tf <- switch(type,
               tf = 1 / rowSums(dtm),
               boolean = 1 / rowSums(dtm > 0)
  )
  Diagonal(dim(dtm)[1], tf)
}

#' @name dtm_transform
#' @title Transform Document-Term-Matrix to TF/TF-IDF/BOOLEAN form
#' @description Creates transformed TF/TF-IDF/BOOLEAN matrix from DocumentTermMatrix.
#' For example see \link{dtm_get_tf}
#' @param dtm \link{sparseMatrix} - Document-Term-Matrix
#' @param type type of transormation. See \link{dtm_get_tf}, \link{dtm_get_idf}
#' @export
dtm_transform <- function(dtm, type = c('tfidf', 'tf', 'boolean'), ...) {
  type <- match.arg(type)
  dtm_transformed <- switch(type,
                tfidf = dtm_get_tf(dtm, type = 'tf') %*% dtm %*% dtm_get_idf(dtm),
                tf = dtm_get_tf(dtm, type = type) %*% dtm,
                boolean = dtm_get_tf(dtm, type = type) %*% dtm
                )
  dtm_transformed
}

#' @name dtm_remove_common_terms
#' @title remove (un)common words (terms) from Document-Term Matrix
#' @description Creates reduced Document-Term Matrix - throws out uncommon words
#' to obtain matrix with a given level of sparsity.
#' @param m \link{sparseMatrix} - TermDocumentMatrix
#' @param sparsity maximum level of spasity in resulted matrix
#' @export
dtm_remove_common_terms <- function (dtm, uncommon = 0.01, common = 0.99)
{
  tdm <-t(dtm)
  tab <- c(sum(tdm@i == 0), tabulate(tdm@i, nbins = dim(tdm)[[1]] - 1))
  t1 <- tab > tdm@Dim[[2]] * uncommon
  t2 <- tab < tdm@Dim[[2]] * common
  t(tdm[t1 & t2, ])
}
