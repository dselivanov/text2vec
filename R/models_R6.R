# // Copyright (C) 2015 - 2016  Dmitriy Selivanov
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

text2vec_estimator = R6::R6Class(
  classname = "estimator",
  public = list(
    fit = function(...) {stop("Method is not implemented")},
    verbose = FALSE
  ),
  private = list(
    fitted = FALSE,
    internal_matrix_format = NULL
  )
)

text2vec_word_embedding_model = R6::R6Class(
  classname = "word_embedding_model",
  inherit = text2vec_estimator,
  public = list(
    get_word_vectors = function(...) {stop("Method is not implemented")}
  )
)

text2vec_transformer = R6::R6Class(
  classname = c("transformer"),
  inherit = text2vec_estimator,
  public = list(
    transform = function(...) {stop("Method is not implemented")}
  )
)
text2vec_topic_model = R6::R6Class(
  "topic_model",
  inherit = text2vec_transformer,
  public = list(
    fit_transform = function(...) {stop("Method is not implemented")},
    get_word_vectors = function(...) {stop("Method is not implemented")}
    # partial_fit = function() {stop("Method is not implemented")},
    # predict = function() {stop("Method is not implemented")},
  ),
  private = list(
    n_topics = NULL
  )
)
