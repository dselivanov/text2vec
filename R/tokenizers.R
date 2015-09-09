#' @name simple_tokenizer
#' @title simple tokenization function. Perform split by fixed symbol.
#' @param txt - \link{character} vector
#' @param split  - \link{character} split symbol
#' @return - \link{list} of \link{character} vectors.
#' Each vector containts tokens.
#' @examples
#' doc <- c("first second", "bla bla")
#' simple_tokenizer(doc)
#' @export
simple_tokenizer <- function (txt, split = ' ')
{
  strsplit(x = txt, split = split, fixed = T)
}
#' @name simple_tokenizer
#' @title simple tokenization function. Perform split by regular expression.
#' @param txt - \link{character} vector
#' @param split  - \link{character} split symbol
#' @param ... - arguments to \link{strsplit} function, which is used internally.
#' @return - \link{list} of \link{character} vectors.
#' Each element of list containts vector of tokens.
#' @examples
#' doc <- c("first  second", "bla, bla, blaa")
#' regexp_tokenizer(doc, split = '([[:space:]]|[[:punct:]])+')
#' @export
regexp_tokenizer <- function (txt, split = '([[:space:]]|[[:punct:]])+', ...)
{
  strsplit(txt, split = split, ...)
}
