#' @name simple_preprocess
#' @title simple standart text preprocessing function.
#' @param change_case_func - \link{function} performing case transformation -
#' convert text between upper case, lower case. \link{tolower} by default.
#' It should take \code{caracter} vectors and return \code{caracter} vectors.
#' Use \link{identity} or \code{NULL} if no transformation needed.
#' @param clean_func - \link{function} performing text cleaning.
#' By default it only keep latin symbols '[a-z]+' and replace other symbols by
#' whitespace. Should take \code{caracter} vectors and return \code{caracter} vectors.
#' Use \link{identity} or \code{NULL} if no transformation needed.
#' @param strip_space - \link{logical}, wheteher replace multiple whitespace by signle one.
#' @param trim_space - \link{logical}, wheteher remove trailing and leading space.
#' @return - \link{character} vectors - preprocessed text.
#' @examples
#' doc <- c(" first  second", "bla, bla, blaa 33  ")
#' simple_preprocess(doc)
#' @export
simple_preprocess <- function(txt,
                              change_case_func = tolower,
                              clean_func = (function(x) gsub(pattern = "[^a-z]", replacement = " ", x = x)),
                              strip_space = T,
                              trim_space = T) {

  case_func    <-   if(is.function(change_case_func)) change_case_func else identity

  clean_func   <-   if(is.function(clean_func)) clean_func else identity

  strip_space_func <- if(isTRUE(strip_space)) {
    function(x) gsub(pattern = "\\s+", replacement = " ", x = x)
  } else identity

  trim_space_func <- if(isTRUE(trim_space)) {
    function(x) gsub(pattern = "^\\s|\\s$", replacement = "", x)
  } else identity

   txt %>%
     case_func %>%
     clean_func %>%
     strip_space_func %>%
     trim_space_func
}
