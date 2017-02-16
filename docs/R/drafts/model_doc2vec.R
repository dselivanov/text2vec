# doc2vec <- function(word_vectors_matrix, method = c('average'), verbose = FALSE) {
#
#   .words = rownames(word_vectors_matrix)
#
#   stopifnot(is.matrix(word_vectors_matrix) ||
#               !is.null(.words))
#
#   # internal parameters and helpers
#   .internal_matrix_format = 'dgCMatrix'
#
#   # model parameters
#
#   # internal debug methods
#
#   # main methods
#   transform <- function(dtm) {
#
#     dtm_terms = unique(colnames(dtm))
#     # check terms unique
#     dtm = coerce_matrix(dtm, .internal_matrix_format, verbose = verbose)
#     stopifnot(ncol(dtm) != length(dtm_terms))
#
#     missed_word_vectors = setdiff(dtm_terms, .words)
#     n_missed_word_vectors = length(missed_word_vectors)
#     if ( n_missed_word_vectors != 0) {
#       missed_terms = paste(missed_word_vectors[1:min(5, n_missed_word_vectors)],
#                            collapse = ', ')
#
#       warning(paste(n_missed_word_vectors,
#                     "(", missed_terms, "...)",
#                     " of terms have no correspondingword embeddings. Ignoring them."))
#     }
#
#     presented_word_vectors = intersect(dtm_terms, .words)
#     as.matrix(
#       diag(1 / rowSums(dtm)) %*%
#         dtm[ , presented_word_vectors, drop = F] %*%
#         word_vectors_matrix[ presented_word_vectors, , drop = F]
#
#     )
#   }
#
#   self <- function() {
#     model = list(transform = transform)
#     class(model) <- c('text2vec_model', 'doc2vec')
#     model
#   }
#
#   self()
# }
