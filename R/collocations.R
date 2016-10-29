#' @name Collocations
#' @title Collocations model.
#' @description Creates Collocations model which can be used for phrase extraction.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' colloc = Collocations$new(vocabulary)
#' colloc$fit(it)
#' colloc$transform(it)
#' colloc$prune(pmi_min = 5, gensim_min = 0, lfmd_min = -Inf)
#' }
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(vocabulary)}}{Constructor for Collocations model.For description of arguments see \bold{Arguments} section.}
#'   \item{\code{$fit(it)}}{fit Collocations model to input iterator \code{it}}
#'   \item{\code{$transform(it)}}{transform input iterator using learned collocations
#'   and produce iterator with collapsed phraeses}
#'   \item{\code{$transform()}}{ create generator which transforms iterator collapsing phrases}
#'   \item{\code{$prune(pmi_min = 5, gensim_min = 0, lfmd_min = -Inf)}}{
#'   filter out non-relevant phrases with low score. User can do it directly by modifying \code{collocation_stat} object.}
#'}
#' @field collocation_stat \code{data.table} with collocations(phrases) statistics.
#' Useful for filtering non-relevant phrases
#' @section Arguments:
#' \describe{
#'  \item{colloc}{A \code{Collocation} model object}
#'  \item{it}{An input \code{itoken} iterator}
#'  \item{vocabulary}{\code{text2vec_vocabulary} - will look for collactions only for words in vocabulary}
#' }
#' @export
Collocations = R6::R6Class(
  private = list(
    sep = NULL,
    phrases = NULL,
    phrases_ptr = NULL,
    v = NULL,
    min_collocation_count = NULL
  ),
  public = list(
    collocation_stat = NULL,
    initialize = function(vocab, min_collocation_count = 50, sep = "_") {
      private$v = copy(vocab)
      private$sep = sep
      private$min_collocation_count = min_collocation_count
    },
    fit = function(it) {
      if(!is.null(self$collocation_stat)) {
        private$v = create_vocabulary(unique(c(private$phrases, private$v$vocab$terms)), sep_ngram = private$sep )
        # private$v = create_vocabulary(unique(c(private$phrases, private$v$vocab$terms)), sep_ngram = private$sep )
        it_internal = self$transform(it)
      }
      else {
        it_internal = it$clone(deep = T)
      }
      vectorizer = vocab_vectorizer(private$v, grow_dtm = FALSE,
                                    skip_grams_window = 1L,
                                    skip_grams_window_context = "right")
      tcm = create_tcm(it_internal, vectorizer)
      word_counts = attr(tcm, "word_count", TRUE)
      nword = sum(word_counts)
      ii = tcm@x >= private$min_collocation_count
      tcm@i = tcm@i[ii]
      tcm@j = tcm@j[ii]
      tcm@x = tcm@x[ii]
      dt = data.table(
        prefix = colnames(tcm)[tcm@i + 1L],
        suffix = colnames(tcm)[tcm@j + 1L],
        # count prefix
        n_i = word_counts[tcm@i + 1L],
        # count suffix
        n_j = word_counts[tcm@j + 1L],
        # cooccurence count
        n_ij = as.integer(tcm@x)
      )
      # see http://www.lrec-conf.org/proceedings/lrec2002/pdf/128.pdf for details about PMI and LFMD
      dt[ , pmi :=  log2( (n_ij / nword) / ((n_i / nword) * (n_j / nword))) ]
      dt[ , lfmd := log2( (n_ij / nword) ^ 2 / ((n_i / nword) * (n_j / nword))) + log2(n_ij / nword)]
      # https://radimrehurek.com/gensim/models/phrases.html#gensim.models.phrases.Phrases
      # A phrase of words a and b is accepted if (cnt(a, b) - min_count) * N / (cnt(a) * cnt(b)) > threshold
      # where N is the total vocabulary size.
      dt[ , gensim := (n_ij - private$min_collocation_count) * nword / (as.numeric(n_i) * n_j)]
      self$collocation_stat = rbindlist(list(self$collocation_stat, dt), use.names = TRUE, fill = TRUE)

      private$phrases = paste(self$collocation_stat$prefix, self$collocation_stat$suffix, sep = private$sep)
      private$phrases_ptr = create_xptr_unordered_set(private$phrases)

      self$collocation_stat[, rank_pmi := frank(-pmi, ties.method = "first")]
      self$collocation_stat[, rank_lfmd := frank(-lfmd, ties.method = "first")]
      self$collocation_stat[, rank_gensim := frank(-gensim, ties.method = "first")]
      self$collocation_stat = self$collocation_stat[order(rank_pmi + rank_lfmd + rank_gensim)]
    },
    prune = function(pmi_min = 5, gensim_min = 0, lfmd_min = -Inf) {
      ii = self$collocation_stat$pmi >= pmi_min &
        self$collocation_stat$gensim >= gensim_min &
        self$collocation_stat$lfmd >= lfmd_min
      self$collocation_stat = self$collocation_stat[ii, ]

      private$phrases = paste(self$collocation_stat$prefix, self$collocation_stat$suffix, sep = private$sep)
      private$phrases_ptr = create_xptr_unordered_set(private$phrases)
    },
    transform = function(it) {
      collapse_collocations = function(x) collapse_collocations_cpp(x$tokens, private$phrases_ptr, private$sep)
      it_internal = it$clone(deep = TRUE)
      itoken_transformer_R6$new(it_internal, collapse_collocations)
    }
  )
)
