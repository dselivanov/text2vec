# // Copyright (C) 2015 - 2017  Dmitriy Selivanov
# // This file is part of text2vec
# //
#   // text2vec is free software: you can redistribute it and/or modify it
# // under the terms of the GNU General Public License as published by
# // the Free Software Foundation, either version 2 of the License, or
# // (at your option) any later version.
# //
#   // text2vec is distributed in the hope that it will be useful, but
# // WITHOUT ANY WARRANTY; without even the implied warranty of
# // MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# // GNU General Public License for more details.
# //
#   // You should have received a copy of the GNU General Public License
# // along with text2vec.  If not, see <http://www.gnu.org/licenses/>.

#' @name tokenizers
#' @title Simple tokenization functions for string splitting
#' @description Few simple tokenization functions. For more comprehensive list see \code{tokenizers} package:
#' \url{https://cran.r-project.org/package=tokenizers}.
#' Also check \code{stringi::stri_split_*}.
#' @param strings \code{character} vector
#' @param xptr \code{logical} tokenize at C++ level - could speed-up by 15-50\%.
#' @param sep \code{character}, \code{nchar(sep)} = 1 - split strings by this character.
#' @param ... other parameters (usually not used - see source code for details).
#' @return \code{list} of \code{character} vectors. Each element of list contains vector of tokens.
#' @examples
#' doc = c("first  second", "bla, bla, blaa")
#' # split by words
#' word_tokenizer(doc)
#' #faster, but far less general - perform split by a fixed single whitespace symbol.
#' space_tokenizer(doc, " ")

#' @rdname tokenizers
#' @export
word_tokenizer = function(strings, ...)
{
  res = stringi::stri_split_boundaries(strings, type = "word", skip_word_none = TRUE)
  names(res) = names(strings)
  res
}

#' @rdname tokenizers
#' @export
char_tokenizer = function(strings, ...)
{
  res = stringi::stri_split_boundaries(strings, type = "character")
  names(res) = names(strings)
  res
}

#' @rdname tokenizers
#' @export
space_tokenizer = function(strings, sep = " ", xptr = FALSE, ...)
{
  stopifnot(nchar(sep) == 1)
  if(!xptr) {
    stringi::stri_split_fixed(strings, pattern = sep, ...)
  } else {
    cpp_fixed_char_tokenizer(strings, sep)
  }
}
