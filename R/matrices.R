#' @name get_tdm
#' @title TermDocumentMatrix construction
#' @description Creates TermDocumentMatrix from list of character vectors.
#' @param textList - \link{character} \link{list} - input text corpus. It should be provided as list of character vectors.
#' Each element in the list represents document in corpus, each element in character vector represents word(term) in document.
#' @param dictionary - \link{character} or \link{NULL} -  use only words from this dictionary in TermDocumentMatrix construction.
#' NULL if all words should be used.
#' @examples
#' # create TermDocumentMatrix from two simple texts.
#' textList <- preprocess_corpus(corpus = c("first text", "Second text. It is much much longer!:-)"))
#' TDM <- get_tdm(textList)
#' TDMWithDictionary <- get_tdm(textList, dictionary = c("first", "second", "text"))
#' @export
get_tdm <- function(textList, dictionary = NULL, parallerismLevel = 1, mc.cores = ceiling(detectCores() / 2)) {
  if(! is.list(textList)) stop("input textList parameter should be a list of character vectors")
  if(! all(sapply(textList, is.character, USE.NAMES = F))) stop("input textList parameter should be a list of character vectors")
  len <- length(textList)
  #splits <- split_vector(vector = textList, parallerismLevel = parallerismLevel, splits = mc.cores)
  #wc <- unlist(mclapply(splits, function(indx, textListArg) lapply(textListArg[indx[1]:indx[2]], ListWordCount), textList), recursive = F)
  wc <- ListWordCount(textList)
  if(is.character(dictionary) && length(dictionary) > 0) {
    mtch <- match(names(wc$wordCounts), dictionary, nomatch = 0)
    dicIndex <- which(mtch > 0)
    j <- wc$documentIndex[dicIndex]
    v <- wc$wordCounts[dicIndex]
    allTerms <- sort(unique(dictionary))
    i <- mtch[dicIndex]
  }
  else {
    j <- wc$documentIndex
    v <- wc$wordCounts
    allTerms <- sort(unique(names(v)))
    i <- match(names(v), allTerms)
  }
  return( sparseMatrix(i = i, j = j, x = v,
                       dims=c(length(allTerms), len),
                       dimnames=list(allTerms, NULL)))
}

#' @name get_dtm
#' @title DocumentTermMatrix construction
#' @description Creates DocumentTermMatrix from list of character vectors.
#' @param textList - \link{character} \link{list} - input text corpus. It should be provided as list of character vectors.
#' Each element in the list represents document in corpus, each element in character vector represents word(term) in document.
#' @param dictionary - \link{character} or \link{NULL} -  use only words from this dictionary in DocumentTermMatrix construction.
#' NULL if all words should be used.
#' @examples
#' # create DocumentTermMatrix from two simple texts
#' textList <- preprocess_corpus(corpus = c("first text", "Second text. It is much much longer!:-)"))
#' DTM <- get_dtm(textList)
#' DTMWithDictionary <- get_dtm(textList, dictionary = c("first", "second", "text"))
#' @export
get_dtm <- function(textList, dictionary = NULL) {
  t(get_tdm(textList, dictionary = dictionary))
}

#' @name get_idf
#' @title InverseDocumentFrequency scaling matrix construction from DocumentTermMatrix
#' @description Creates InverseDocumentFrequency (idf) scaling matrix from DocumentTermMatrix
#' @param m \link{simple_triplet_matrix} - DocumentTermMatrix.
#' @param logScale function to use in idf calculation. In usually \link{log2} used.
#' @param type type of scaling. Formula for idf:
#' \deqn{idf = \log(\fraq {#documents in the corpus}{# documents where the term + 1}}{%
#' idf = log (# documents in the corpus) / (# documents where the term appears) }
# For idfProb:
# \deqn{idf = \log(\fraq {#documents in the corpus}{# documents where the term appears + 1}}{%
# idf = log (# documents in the corpus) / (# documents where the term appears + 1) }
#' @examples
#' # create DocumentTermMatrix from two simple texts
#' textList <- preprocess_corpus(corpus = c("first text", "Second text. It is much much longer!:-)"))
#' DTM <- get_dtm(textList)
#' IDFScalingMatrix <- get_idf(DTM)
#' @export
get_idf <- function(m, logScale = log2,
                    #type = c('idf', 'idfProb'))
                    type = 'idf')
{
  # m should be DTM matrix
  m@x = rep(1, length(m@x))
  type <- match.arg(type)
  cs <- col_sums(m)
  idf <- switch(type,
                idf = logScale(nrow(m) / (cs + 1))
                #, idfProb = pmax(0.01, logScale((nrow(m) - cs + 0.5) / (cs + 0.5)))
                )
  Diagonal(dim(m)[2], idf)
}

#' @name get_tf
#' @title TermFrequency scaling matrix construction from DocumentTermMatrix
#' @description Creates TermFrequency (idf) scaling matrix from DocumentTermMatrix
#' @param m \link{simple_triplet_matrix} - DocumentTermMatrix.
#' @param type type of scaling. Formula for tf :
#' \deqn{tf = \fraq {# word appears in document}{# words in document}}{%
#' tf = (# word appears in document) / (# words in document) }
#  For boolean:
#' \deqn{tf = {Does word appears in document (boolean encoding): 0 if not appears, 1 if appears}}{%
#' tf = (Does word appears in document (boolean encoding): 0 if not appears, 1 if appears)}
#' @examples
#' # create DocumentTermMatrix from two simple texts
#' textList <- preprocess_corpus(corpus = c("first text", "Second text. It is much much longer!:-)"))
#' DTM <- get_dtm(textList)
#' TFScalingMatrix <- get_tf(DTM, type = 'tf')
#' # Let's calculate TermFrequency scaled matrix:
#' TFTransformed <- TFScalingMatrix %*% m
#' # or TermFrequency-InverseDocumentFrequency scaled matrix.
#' TFIDFTransformed <- TFScalingMatrix %*% m %*% get_idf(DTM)
#' # The same result we can obtain using transform_dtm function with parameter type = 'tfidf'
#' TFIDFTransformed_2 <- transform_dtm(DTM, type='tfidf')
#' identical(TFIDFTransformed_2, TFIDFTransformed)
#' @export
get_tf <- function(m, type = c('tf', 'boolean'))
{
  # m = DTM matrix
  type <- match.arg(type)
  tf <- switch(type,
               tf = 1 / row_sums(m),
               boolean = 1 / row_sums(m > 0)
  )
  Diagonal(dim(m)[1], tf)
}

#' @name transform_dtm
#' @title Transform DocumentTermMatrix of wordcounts to TF/TF-IDF/BOOLEAN form
#' @description Creates transformed TF/TF-IDF/BOOLEAN matrix from DocumentTermMatrix of wordcounts
#' @param m \link{simple_triplet_matrix} - DocumentTermMatrix.
#' @param type type of transormation. See \link{get_tf}, \link{get_idf}
#' @examples
#' # create DocumentTermMatrix from two simple texts
#' textList <- preprocess_corpus(corpus = c("first text", "Second text. It is much much longer!:-)"))
#' DTM <- get_dtm(textList)
#' DTMTransformed <- transform_dtm(DTM, type='tfidf')
#' @export
transform_dtm <- function(m, type = c('tfidf', 'tf', 'boolean'), ...) {
  type <- match.arg(type)
  mTransformed <- switch(type,
                tfidf = get_tf(m, type = 'tf', ...) %*% m %*% get_idf(m, type = 'idf', ...),
                tf = get_tf(m, type = type) %*% m,
                boolean = get_tf(m, type = type) %*% m,
                )
  mTransformed
}

#' @name remove_sparse_terms
#' @title remove sparse words (terms) from TermDocumentMatrix matrix
#' @description Creates reduced TermDocumentMatrix - throws out uncommon words
#' to obtain matrix with a given level of sparsity.
#' @param m \link{simple_triplet_matrix} - TermDocumentMatrix
#' @param sparsity maximum level of spasity in resulted matrix
#' @examples
#' # create DocumentTermMatrix from two simple texts
#' textList <- preprocess_corpus(corpus = c("first text", "Second text. It is much much longer!:-)"))
#' DTM <- get_dtm(textList)
#' DTMTransformed <- transform_dtm(DTM, type='tfidf')
#' @export
remove_sparse_terms <- function (m, sparsity)
{
  # m = TDM matrix
  # same as:
  # t <- table(m@i)> m@Dim[2] * (1 - sparse)
  # but much faster. Add sum(m@i==0) because tabulate works only with POSITIVE itegers
  t <- c(sum(m@i == 0), tabulate(m@i)) > m@Dim[2] * (1 - sparsity)
  return(m[which(t == T), ])
}
