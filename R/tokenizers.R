#' @name tokenizers
#' @title Simple tokenization functions, which performs string splitting
#' @description simple wrappers around \code{base} regular expressions.
#' For much more faster and functional tokenizers see \code{tokenizers} package:
#' \url{https://cran.r-project.org/web/packages/tokenizers/index.html}.
#' Also see \code{str_split_*} functions in \code{stringi} and \code{stringr} packages.
#' The reason for not including this packages to \code{text2vec} dependencies is our
#' desare to keep number of dependencies as small as possible.
#' @param strings \code{character} vector
#' @param pattern \code{character} pattern symbol.
#' @param ... other parameters to \link{strsplit} function, which is used under the hood.
#' @return \code{list} of \code{character} vectors.
#' Each element of list containts vector of tokens.
#' @examples
#' doc = c("first  second", "bla, bla, blaa")
#' # split by words
#' word_tokenizer(doc)
#' #faster, but far less general - perform split by a fixed single whitespace symbol.
#' regexp_tokenizer(doc, " ", TRUE)

#' @rdname tokenizers
#' @export
word_tokenizer = function(strings, ...)
{
  strsplit(strings, "\\W", ...) %>% lapply(function(x) x[nchar(x) > 0])
}

#' @rdname tokenizers
#' @export
regexp_tokenizer = function(strings, pattern, ...)
{
  strsplit(strings, pattern, ...)
}

#' @rdname tokenizers
#' @export
char_tokenizer = function(strings, ...)
{
  strsplit(strings, "", TRUE, ...)
}

#' @rdname tokenizers
#' @export
space_tokenizer = function(strings, ...)
{
  strsplit(strings, " ", TRUE, ...)
}
