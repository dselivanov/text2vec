#' @name Collocations
#' @title Collocations model.
#' @description Creates Collocations model which can be used for phrase extraction.
#' @section Usage:
#' For usage details see \bold{Methods, Arguments and Examples} sections.
#' \preformatted{
#' colloc = Collocations$new(vocabulary = NULL, collocation_count_min = 50, pmi_min = 5, gensim_min = 0, lfmd_min = -Inf, sep = "_")
#' colloc$partial_fit(it, ...)
#' colloc$fit(it, n_iter = 1, ...)
#' colloc$transform(it)
#' colloc$prune(pmi_min = 5, gensim_min = 0, lfmd_min = -Inf)
#' colloc$collocation_stat
#' }
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{$new(vocabulary = NULL, collocation_count_min = 50, sep = "_")}}{Constructor for Collocations model.For description of arguments see \bold{Arguments} section.}
#'   \item{\code{$fit(it, n_iter = 1, ...)}}{fit Collocations model to input iterator \code{it}.
#'   Iterating over input iterator \code{it} \code{n_iter} times, so hierarchically can learn multi-word phrases.
#'   Invisibly returns \code{collocation_stat}.}
#'   \item{\code{$partial_fit(it, ...)}}{iterates once over data and learns collocations. Invisibly returns \code{collocation_stat}.
#'   Workhorse for \code{$fit()}}.
#'   \item{\code{$transform(it)}}{transforms input iterator using learned collocations model.
#'   Result of the transformation is new \code{itoken} or \code{itoken_parallel} iterator which will
#'   produce tokens with phrases collapsed into single token.}
#'   \item{\code{$prune(pmi_min = 5, gensim_min = 0, lfmd_min = -Inf)}}{
#'   filter out non-relevant phrases with low score. User can do it directly by modifying \code{collocation_stat} object.}
#'}
#' @field collocation_stat \code{data.table} with collocations(phrases) statistics.
#' Useful for filtering non-relevant phrases
#' @section Arguments:
#' \describe{
#'  \item{colloc}{A \code{Collocation} model object}
#'  \item{n_iter}{number of iteration over data}
#'  \item{pmi_min, gensim_min, lfmd_min}{minimal scores of the corresponding statistics in order to collapse tokens into collocation
#'  see data in \code{colloc$collocation_stat} for better understanding}
#'  \item{it}{An input \code{itoken} or \code{itoken_parallel} iterator}
#'  \item{vocabulary}{\code{text2vec_vocabulary} - if provided will look for collocations consisted of only from vocabulary}
#' }
#' @examples
#' library(text2vec)
#' data("movie_review")
#'
#' preprocessor = function(x) {
#'   tolower(x) %>% gsub("[^[:alnum:]\\s]", replacement = " ", .)
#' }
#' sample_ind = 1:100
#' tokens = movie_review$review[sample_ind] %>% preprocessor %>% word_tokenizer
#' it = itoken(tokens, ids = movie_review$id[sample_ind])
#' system.time(v <- create_vocabulary(it))
#' v = prune_vocabulary(v, term_count_min = 5)
#'
#' cc = Collocations$new(collocation_count_min = 5, pmi_min = 5)
#' cc$fit(it, n_iter = 2)
#' cc$collocation_stat
#'
#' it2 = cc$transform(it)
#' v2 = create_vocabulary(it2)
#' v2 = prune_vocabulary(v2, term_count_min = 5)
#' # check what phrases model has learned
#' setdiff(v2$term, v$term)
#' # [1] "main_character"  "jeroen_krabb"    "boogey_man"      "in_order"
#' # [5] "couldn_t"        "much_more"       "my_favorite"     "worst_film"
#' # [9] "have_seen"       "characters_are"  "i_mean"          "better_than"
#' # [13] "don_t_care"      "more_than"       "look_at"         "they_re"
#' # [17] "each_other"      "must_be"         "sexual_scenes"   "have_been"
#' # [21] "there_are_some"  "you_re"          "would_have"      "i_loved"
#' # [25] "special_effects" "hit_man"         "those_who"       "people_who"
#' # [29] "i_am"            "there_are"       "could_have_been" "we_re"
#' # [33] "so_bad"          "should_be"       "at_least"        "can_t"
#' # [37] "i_thought"       "isn_t"           "i_ve"            "if_you"
#' # [41] "didn_t"          "doesn_t"         "i_m"             "don_t"
#'
#' # and same way we can create document-term matrix which contains
#' # words and phrases!
#' dtm = create_dtm(it2, vocab_vectorizer(v2))
#' # check that dtm contains phrases
#' which(colnames(dtm) == "jeroen_krabb")
#' @export
Collocations = R6::R6Class(
  private = list(
    sep = NULL,
    phrases = NULL,
    phrases_ptr = NULL,
    v = NULL,
    collocation_count_min = NULL,
    pmi_min = NULL,
    gensim_min = NULL,
    lfmd_min = NULL
  ),
  public = list(
    collocation_stat = NULL,
    initialize = function(vocabulary = NULL,
                          collocation_count_min = 50L,
                          pmi_min = 5,
                          gensim_min = 0,
                          lfmd_min = -Inf,
                          sep = "_") {
      if(is.null(vocabulary)) {
        flog.debug("got NULL as vocabulary - so it will be built from training data iterator later")
      } else if(inherits(vocabulary, "text2vec_vocabulary")) {
        private$v = copy(vocabulary)
      } else {
        stop("'vocabulary' shold be object of class 'text2vec_vocabulary' or NULL")
      }

      private$sep = sep
      private$collocation_count_min = collocation_count_min
      private$pmi_min = pmi_min
      private$gensim_min = gensim_min
      private$lfmd_min = lfmd_min
    },
    fit = function(it, n_iter = 1, ...) {
      for(i in seq_len(n_iter)) {
        self$partial_fit(it, ...)
        flog.info("iteration %d - found %d collocations", i, nrow(self$collocation_stat))
      }
      invisible(self$collocation_stat)
    },
    partial_fit = function(it, ...) {
      stopifnot(inherits(it, "itoken") || inherits(it, "itoken_parallel"))
      if(is.null(private$v)) {
        flog.debug("building vocabulary for Collocations model")
        private$v = create_vocabulary(it)
        private$v = prune_vocabulary(private$v, term_count_min = private$collocation_count_min)
        flog.debug("vocabulary construction done - %d terms", nrow(private$v))
      }
      if(!is.null(self$collocation_stat)) {
        private$v = create_vocabulary(unique(c(private$phrases, private$v$term)), sep_ngram = private$sep )
        it_internal = self$transform(it)
      }
      else {
        if(inherits(it, "itoken_parallel")) {
          flog.debug("clonning itoken_parallel")
          it_internal = lapply(it, function(x) x$clone(deep = TRUE))
          data.table::setattr(it_internal, "class", "itoken_parallel")
        } else
          it_internal = it$clone(deep = TRUE)
      }
      vectorizer = vocab_vectorizer(private$v)
      tcm = create_tcm(it_internal, vectorizer, skip_grams_window = 1L,
                       skip_grams_window_context = "right")
      # flog.debug("tcm done dim = %d * %d", nrow(tcm), ncol(tcm))
      word_counts = attr(tcm, "word_count", TRUE)
      # cast to double in order to not get integer overflow in multiplications below
      nword = as.numeric(sum(word_counts))
      ii = tcm@x >= private$collocation_count_min
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
        # co-occurence count
        n_ij = as.integer(tcm@x)
      )
      # see http://www.lrec-conf.org/proceedings/lrec2002/pdf/128.pdf for details about PMI and LFMD
      dt[ , pmi :=  log2( (n_ij / nword) / ((n_i / nword) * (n_j / nword))) ]
      dt[ , lfmd := log2( (n_ij / nword) ^ 2 / ((n_i / nword) * (n_j / nword))) + log2(n_ij / nword)]
      # https://radimrehurek.com/gensim/models/phrases.html#gensim.models.phrases.Phrases
      # A phrase of words a and b is accepted if (cnt(a, b) - min_count) * N / (cnt(a) * cnt(b)) > threshold
      # where N is the total vocabulary size.
      dt[ , gensim := (n_ij - private$collocation_count_min) * nword / (as.numeric(n_i) * n_j)]
      self$collocation_stat = rbindlist(list(self$collocation_stat, dt), use.names = TRUE, fill = TRUE)
      self$prune()

      private$phrases = paste(self$collocation_stat$prefix, self$collocation_stat$suffix, sep = private$sep)
      private$phrases_ptr = create_xptr_unordered_set(private$phrases)

      self$collocation_stat[, rank_pmi := frank(-pmi, ties.method = "first")]
      self$collocation_stat[, rank_lfmd := frank(-lfmd, ties.method = "first")]
      self$collocation_stat[, rank_gensim := frank(-gensim, ties.method = "first")]
      self$collocation_stat = self$collocation_stat[order(rank_pmi + rank_lfmd + rank_gensim)]
      setkey(self$collocation_stat, rank_pmi)

      invisible(self$collocation_stat)
    },
    prune = function(pmi_min = private$pmi_min, gensim_min = private$gensim_min, lfmd_min = private$lfmd_min) {
      ii = self$collocation_stat$pmi >= pmi_min &
        self$collocation_stat$gensim >= gensim_min &
        self$collocation_stat$lfmd >= lfmd_min
      self$collocation_stat = self$collocation_stat[ii, ]

      private$phrases = paste(self$collocation_stat$prefix, self$collocation_stat$suffix, sep = private$sep)
      private$phrases_ptr = create_xptr_unordered_set(private$phrases)

      invisible(self$collocation_stat)
    },
    transform = function(it) {
      # if pointer is invalid - init it
      if(is_invalid_ptr(private$phrases_ptr))
        private$phrases_ptr = create_xptr_unordered_set(private$phrases)

      collapse_collocations = function(x) collapse_collocations_cpp(x$tokens, private$phrases_ptr, private$sep)
      if(inherits(it, "itoken_parallel")) {
        flog.debug("clonning itoken_parallel")
        it_transformed = lapply(it, function(x) {
          itoken_transformer_R6$new(x$clone(deep = TRUE), collapse_collocations)
        })
        data.table::setattr(it_transformed, "class", "itoken_parallel")
      } else {
        it_transformed = itoken_transformer_R6$new(it$clone(deep = TRUE), collapse_collocations)
        data.table::setattr(it_transformed, "class", c("itoken", class(it)))
      }
      it_transformed
    }
  )
)
