#' @export
simple_tokenizer <- function (text, what = "character", sep = ' ', quote = "", strip.white = TRUE, quiet = TRUE, ...)
{
  scan(text = text, what = what, sep = sep, quote = quote, strip.white = strip.white, quiet = quiet, ...)
}
#' @export
regexp_tokenizer <- function (text, split = '([[:space:]]|[[:punct:]])+', ...)
{
  strsplit(text, split = split, ...)[[1]]
}
#' @export
clean_text <- function(charVector,
                      patternLookupPipe =c("[^[:alnum:]]+"),
                      patternReplacementPipe = c(" "))
{
  if(! is.vector(patternLookupPipe, mode = 'character')) stop('patternLookupPipe should be character vector')
  if(! is.vector(patternReplacementPipe, mode = 'character')) stop('patternReplacementPipe should be character vector')
  if(length(patternLookupPipe) != length(patternReplacementPipe)) stop('pattern and replacement length should be equal')
  res <- charVector
  for (i in 1:length(patternLookupPipe))
    res <- gsub(pattern = patternLookupPipe[i], replacement = patternReplacementPipe[i], x = res)
  res
}
#' @export
preprocess_text <- function(text,
                            patternLookupPipe = c("[^[:alnum:]]+"),
                            patternReplacementPipe = c(" "),
                            letterCaseTransformation = tolower,
                            tokenizer = simple_tokenizer,
                            stopwords = character(0),
                            stemming_function = NULL,
                            ...)
{
  text %>%
    iconv(., to = 'UTF-8', sub = ' ') %>% enc2native %>%
    clean_text(patternLookupPipe = patternLookupPipe, patternReplacementPipe = patternReplacementPipe) %>%
    (function(textVector) {
      if (is.function(letterCaseTransformation)) {
        letterCaseTransformation(textVector)
      } else {
        if(!is.null(letterCaseTransformation)) warning("letterCaseTransformation should be a FUNCTION to transform text or NULL if no transformation needed.")
    }}) %>%
    tokenizer %>%
    (function(x) {
      if(is.character(stopwords) && length(stopwords) > 0) x[!(x %in% stopwords)]
      else x
    }) %>%
    (function(textVector) {
      if (is.function(stemming_function))
        stemming_function(textVector)
      else textVector
    })
}

#' @export
preprocess_corpus <- function(corpus,
                              PreprocessFunction = PreprocessText,
                              parallerismLevel = 1,
                              mc.cores = ceiling(detectCores() / 2),
                              ...)
{
  splits <- split_vector(corpus, parallerismLevel = parallerismLevel, splits = mc.cores)
  unlist(mclapply(splits,
                  function(range, inputCorpus, ...) lapply(inputCorpus[range[1]:range[2]], preprocess_text, ...),
                  corpus,
                  mc.cores = mc.cores),
         ,recursive = FALSE)
}
