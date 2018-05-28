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
#' @param udpipe_model - udpipe model, can be loaded with \code{?udpipe::udpipe_load_model}
#' @param tagger \code{"default"} - tagger parameter as per \code{?udpipe::udpipe_annotate} docs.
#' @param tokenizer \code{"tokenizer"} - tokenizer parameter as per \code{?udpipe::udpipe_annotate} docs.
#' @param pos_keep \code{character(0)} specifies which tokens to keep. \code{character(0)} means to keep all of them
#' @param pos_remove \code{c("PUNCT", "DET", "ADP", "SYM", "PART", "SCONJ", "CCONJ", "AUX", "X", "INTJ")} - which tokens to remove.
#'   \code{character(0)} is equal to not remove any.
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

#' @rdname tokenizers
#' @export
postag_lemma_tokenizer = function(strings,
                                  udpipe_model,
                                  tagger = "default",
                                  tokenizer = "tokenizer",
                                  pos_keep = character(0),
                                  pos_remove = c("PUNCT", "DET", "ADP", "SYM", "PART", "SCONJ", "CCONJ", "AUX", "X", "INTJ")) {
  if(!requireNamespace("udpipe", quietly = TRUE))
    stop("please install 'udpipe' package first: install.packages('udpipe')")

  if(!is.character(pos_keep) || !is.character(pos_remove))
    stop("'pos_keep' should be character vector (of length 0 if option is not needed)")

  id = names(strings)
  if(is.null(id))
    id = as.character(seq_along(strings))

  res = udpipe::udpipe_annotate(udpipe_model,
                                x = strings,
                                doc_id = id,
                                parser = "none",
                                tagger = tagger,
                                tokenizer = tokenizer)
  res = as.data.table(res)
  res = res[ , .(token = list(filter_pos(lemma, upos, pos_keep, pos_remove))), keyby = doc_id]
  res = res[J(id), token]
  names(res) = id
  res
}

filter_pos = function(lemma, pos, pos_keep, pos_remove) {
  index_remove = rep(FALSE, length(lemma))
  index_keep   = rep(TRUE,  length(lemma))

  # FIXME may be better to replace data.table::%chin% with fastmatch::%fin%
  # for now udpipe_annotate() is main bottleneck and way slower than call to %in%
  # so we keep %chin% in order to not introduce more dependencies

  if(length(pos_remove) > 0)
    index_remove = pos %chin% pos_remove

  if(length(pos_keep) > 0)
    index_keep = pos %chin% pos_keep

  index_keep = index_keep & !index_remove
  res = lemma[index_keep]
  names(res) = pos[index_keep]
  res
}
