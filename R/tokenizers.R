#' @name tokenizers
#' @title Tokenization functions, which performs string splitting
#' by regular expression or fixed string.
#' @details Uses \link{str_split} under the hood. (which build on top of \link{stri_split}).
#' Actually just a wrapper for \code{str_split} which is very consistent flexible and robust.
#' See \link{str_split} and \link{modifiers} for details.
#' @param string \link{character} vector
#' @param pattern \link{character} pattern symbol. Also can be one of \link{modifiers}.
#' @return \link{list} of \link{character} vectors.
#' Each element of list containts vector of tokens.
#' @examples
#' doc <- c("first  second", "bla, bla, blaa")
#' # split by words
#' regexp_tokenizer(doc, pattern = stringr::boundary("word"))
#' #faster, but far less general - perform split by a fixed single whitespace symbol.
#' regexp_tokenizer(doc, pattern = stringr::fixed(" "))

#' @rdname tokenizers
#' @export
regexp_tokenizer <- function (string, pattern = boundary("word"))
{
  str_split(string = string, pattern = pattern)
}
