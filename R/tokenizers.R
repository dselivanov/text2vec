#' @name tokenizers
#' @title Tokenization functions, which performs string splitting
#' @description simple wrappers around \code{stringi} and \code{stringr} packages functionality.
#' @details Uses \link{str_split} under the hood(which build on top of \code{stringi::stri_split}).
#' Actually just a wrapper for \code{str_split} which is very consistent, flexible and robust.
#' See \link{str_split} and \link{modifiers} for details.
#' @param string \code{character} vector
#' @param pattern \code{character} pattern symbol. Also can be one of \link{modifiers}.
#' @return \code{list} of \code{character} vectors.
#' Each element of list containts vector of tokens.
#' @examples
#' doc <- c("first  second", "bla, bla, blaa")
#' # split by words
#' word_tokenizer(doc)
#' #faster, but far less general - perform split by a fixed single whitespace symbol.
#' regexp_tokenizer(doc, pattern = stringr::fixed(" "))

#' @rdname tokenizers
#' @export
word_tokenizer <- function(string)
{
  str_split(string = string, pattern = boundary("word"))
}

#' @rdname tokenizers
#' @export
regexp_tokenizer <- function(string, pattern)
{
  str_split(string = string, pattern = pattern)
}

#' @export
stringr::fixed
#' @export
stringr::coll
#' @export
stringr::regex
#' @export
stringr::boundary
