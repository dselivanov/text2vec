#' @name prepare_analogue_questions
#' @title Prepares questions list from \code{questions-words.txt} format.
#' @param questions_file_path \code{character} path of questions file.
#' @param vocab_terms \code{character vector}, words which we have in vocabulary
#' and word embeddings matrix.
#' @param verbose \code{logical} whether to print messages during evaluation.
#' @description for full examples see \link{glove}
#' @seealso \link{check_analogue_accuracy}, \link{glove}
#' @export
prepare_analogue_questions <- function(questions_file_path, vocab_terms, verbose = TRUE) {
  lines <- read_lines(questions_file_path) %>%
    tolower %>%
    str_split(fixed(" "))

  # identify categories of questions
  section_name_ind <- which( sapply(lines, length) != 4 )
  # identify start and end of questions by category
  section_start_ind <- section_name_ind + 1
  section_end_ind <- c(section_name_ind[ -1 ] - 1, length(lines))
  # construct question matrices by category
  q <- Map(
    function(i1, i2, quetsions)
      # take questions strings
      quetsions[i1:i2] %>%
      # make character matrix
      (function(x) do.call(rbind, x)) %>%
      # detect word_vectors rows corresponding to words in question
      match(vocab_terms) %>%
      # make character matrix
      matrix(ncol = 4) %>%
      # detect whether vocabulary contains all words from question
      # filter out question if vocabulary does not contain all words
      (function(x) {
        any_na_ind <- apply(x, 1, anyNA)
        x[!any_na_ind, ]
      }),
    section_start_ind,
    section_end_ind,
    MoreArgs = list(quetsions = lines)
  )

  questions_number <- sapply(q, nrow) %>% sum

  if (verbose) {
    msg <- sprintf("%s -  %d full questions found out of %d total",
                   as.character(Sys.time()),
                   questions_number,
                   length(lines) - length(section_name_ind)
    )
    message(msg)
  }

  stats::setNames(q, sapply(lines[section_name_ind], .subset2, 2))
}

#' @name check_analogue_accuracy
#' @title Checks accuracy of word embeddings on analogue task.
#' @param questions_lst \code{list} of questions. Each element of \code{questions_lst} is
#' a \code{integer matrix} with \code{ncol = 4} and represents set of questions
#' related to particular category. Each element of matrix is a index of row in
#' \code{m_word_vectors}. See output of \link{prepare_analogue_questions} for details
#' @param m_word_vectors word vectors \code{numeric matrix}. Each row should represent word.
#' @param verbose \code{logical} whether to print messages during evaluation.
#' @description for full examples see \link{glove}
#' @seealso \link{prepare_analogue_questions}, \link{glove}
#' @export
check_analogue_accuracy <- function(questions_lst, m_word_vectors, verbose = TRUE) {

  m_word_vectors_norm <-  sqrt(rowSums(m_word_vectors ^ 2))

  categories_number <- length(questions_lst)

  res <- vector(mode = 'list', length = categories_number)

  for (i in 1:categories_number) {
    q_mat <- questions_lst[[i]]
    q_number <- nrow(q_mat)
    category <- names(questions_lst)[[i]]

    m_query <-
      (m_word_vectors[q_mat[, 2], ] +
         m_word_vectors[q_mat[, 3], ] -
         m_word_vectors[q_mat[, 1], ])
    query_norm <- m_query %>%
      (function(x) x ^ 2) %>%
      rowSums %>%
      sqrt

    m_query / query_norm

    cos_mat <- cosine(m_query , m_word_vectors, m_word_vectors_norm)

    for (j in 1:q_number)
      cos_mat[j, q_mat[j, c(1, 2, 3)]] <- -Inf

    preds <- max.col(cos_mat)
    act <- q_mat[, 4]
    correct_number <- sum(preds == act)

    if (verbose) {
      msg <- sprintf("%s - %s: correct %d out of %d, accuracy = %.4f",
                     as.character(Sys.time()),
                     category,
                     correct_number,
                     q_number,
                     correct_number / q_number )
      message(msg)
    }
    res[[i]] <-
      data.frame(
        'predicted' = preds,
        'actual' = act,
        'category' = category,
        stringsAsFactors = FALSE
      )
  }
  res <- do.call(rbind, res)

  if (verbose) {
    msg <- sprintf("%s - OVERALL ACCURACY = %.4f", as.character(Sys.time()),
                   sum(res[['predicted']] == res[['actual']]) / nrow(res) )
    message(msg)
  }
  res
}
