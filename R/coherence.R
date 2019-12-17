#' Coherence metrics for topic models
#'
#' Given a topic model with topics represented as ordered term lists, the coherence may be used to assess the quality of individual topics.
#' This function is an implementation of several of the numerous possible metrics for such kind of assessments.
#' Coherence calculation is sensitive to the content of the reference \code{tcm} that is used for evaluation
#' and that may be created with different parameter settings. Please refer to the details section (or reference section) for information
#' on typical combinations of metric and type of \code{tcm}. For more general information on measuring coherence
#' a starting point is given in the reference section.
#'
#' The currently implemented coherence \code{metrics} are described below including a description of the
#' content type of the \code{tcm} that showed good performance in combination with a specific metric.  \cr
#' For details on how to create \code{tcm} see the example section.  \cr
#' For details on performance of metrics see the resources in the reference section
#' that served for definition of standard settings for individual metrics.  \cr
#' Note that depending on the use case, still, different settings than the standard settings for creation of \code{tcm} may be reasonable.  \cr
#' Note that for all currently implemented metrics the \code{tcm} is reduced to the top word space on basis of the terms in \code{x}.
#'
#' Considering the use case of finding the optimum number of topics among several models with different metrics,
#' calculating the mean score over all topics and normalizing this mean coherence scores from different metrics
#' might be considered for direct comparison.
#'
#' Each metric usually opts for a different optimum number of topics. From initial experience it may be assumed
#' that logratio, pmi and nmpi usually opt for smaller numbers, whereas the other metrics rather tend to propose higher numbers.
#'
#' Implemented metrics:
#' \itemize{
#'   \item "mean_logratio"  \cr
#'                    The logarithmic ratio is calculated as \cr
#'                    \code{log(smooth + tcm[x,y]) - log(tcm[y,y])},  \cr
#'                    where x and y are term index pairs from a "preceding" term index combination.  \cr
#'                    Given the indices c(1,2,3), combinations are \code{list(c(2,1), c(3,1), c(3,2))}.  \cr
#' \cr
#'                    The \code{tcm} should represent the boolean term co-occurrence (internally the actual counts are used)
#'                    in the original documents and, therefore, is an intrinsic metric in the standard use case.  \cr
#' \cr
#'                    This metric is similar to the UMass metric, however, with a smaller smoothing constant by default
#'                    and using the mean for aggregation instead of the sum.  \cr
#'
#'   \item "mean_pmi" \cr
#'                    The pointwise mutual information is calculated as  \cr
#'                    \code{log2((tcm[x,y]/n_doc_tcm) + smooth) - log2(tcm[x,x]/n_doc_tcm) - log2(tcm[y,y]/n_doc_tcm)},  \cr
#'                    where x and y are term index pairs from an arbitrary term index combination  \cr
#'                    that subsets the lower or upper triangle of \code{tcm}, e.g. "preceding".  \cr
#' \cr
#'                    The \code{tcm} should represent term co-occurrences within a boolean sliding window of size \code{10} (internally probabilities are used)
#'                    in an external reference corpus and, therefore, is an extrinsic metric in the standard use case.  \cr
#' \cr
#'                    This metric is similar to the UCI metric, however, with a smaller smoothing constant by default
#'                    and using the mean for aggregation instead of the sum.
#'
#'   \item "mean_npmi" \cr
#'                    Similar (in terms of all parameter settings, etc.) to "mean_pmi" metric
#'                    but using the normalized pmi instead, which is calculated as \cr
#'                    \code{(log2((tcm[x,y]/n_doc_tcm) + smooth) - log2(tcm[x,x]/n_doc_tcm) - log2(tcm[y,y]/n_doc_tcm)) / -log2((tcm[x,y]/n_doc_tcm) + smooth)},  \cr
#' \cr
#'                    This metric may perform better than the simpler pmi metric.
#'
#'   \item "mean_difference" \cr
#'                    The difference is calculated as  \cr
#'                    \code{tcm[x,y]/tcm[x,x] - (tcm[y,y]/n_tcm_windows)},  \cr
#'                    where x and y are term index pairs from a "preceding" term index combination.  \cr
#'                    Given the indices c(1,2,3), combinations are \code{list(c(1,2), c(1,3), c(2,3))}.  \cr
#' \cr
#'                    The \code{tcm} should represent the boolean term co-occurrence (internally probabilities are used)
#'                    in the original documents and, therefore, is an intrinsic metric in the standard use case.
#'
#'   \item "mean_npmi_cosim" \cr
#'                    First, the npmi of an individual top word with each of the top words is calculated as in "mean_npmi". \cr
#'                    This result in a vector of npmi values for each top word. \cr
#'                    On this basis, the cosine similarity between each pair of vectors is calculated. \cr
#' \cr
#'                    The \code{tcm} should represent term co-occurrences within a boolean sliding window of size \code{5} (internally probabilities are used)
#'                    in an external reference corpus and, therefore, is an extrinsic metric in the standard use case.  \cr
#'
#'   \item "mean_npmi_cosim2" \cr
#'                    First, a vector of npmi values for each top word is calculated as in "mean_npmi_cosim". \cr
#'                    On this basis, the cosine similarity between each vector and the sum of all vectors is calculated
#'                    (instead of the similarity between each pair). \cr
#' \cr
#'                    The \code{tcm} should represent term co-occurrences within a boolean sliding window of size \code{110} (internally probabilities are used)
#'                    in an external reference corpus and, therefore, is an extrinsic metric in the standard use case.  \cr
#'}
#'
#' @param x A \code{character matrix} with the top terms per topic (each column represents one topic),
#'          e.g., as created by \code{get_top_words()}.
#'          Terms of \code{x} have to be ranked per topic starting with rank 1 in row 1.
#' @param tcm The term co-occurrence matrix, e.g, a \code{Matrix::sparseMatrix} or \code{base::matrix},
#'            serving as the reference to calculate coherence metrics.
#'            Please note that a memory efficient version of the \code{tcm} is assumed as input
#'            with all entries in the lower triangle (excluding diagonal) set to zero (see, e.g., \code{create_tcm}).
#'            Please also note that some efforts during any pre-processing steps might be skipped since the \code{tcm}
#'            is internally reduced to the top word space, i.e., all unique terms of \code{x}.
#' @param metrics Character vector specifying the metrics to be calculated. Currently the following metrics are implemented:
#'                \code{c("mean_logratio", "mean_pmi", "mean_npmi", "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2")}.
#'                Please refer to the details section for more information on the metrics.
#' @param smooth Numeric smoothing constant to avoid logarithm of zero. By default, set to \code{1e-12}.
#' @param n_doc_tcm The \code{integer} number of documents or text windows that was used to create the \code{tcm}.
#'                  \code{n_doc_tcm} is used to calculate term probabilities from term counts as required for several metrics.
#' @return A \code{numeric matrix} with the coherence scores of the specified \code{metrics} per topic.
#' @references
#' Below mentioned paper is the main theoretical basis for this code. \cr
#' Currently only a selection of metrics stated in this paper is included in this R implementation. \cr
#' Authors: Roeder, Michael; Both, Andreas; Hinneburg, Alexander (2015) \cr
#' Title: Exploring the Space of Topic Coherence Measures. \cr
#' In: Xueqi Cheng, Hang Li, Evgeniy Gabrilovich und Jie Tang (Eds.): \cr
#' Proceedings of the Eighth ACM International Conference on Web Search and Data Mining - WSDM '15. \cr
#' the Eighth ACM International Conference. Shanghai, China, 02.02.2015 - 06.02.2015. \cr
#' New York, USA: ACM Press, p. 399-408. \cr
#' https://dl.acm.org/citation.cfm?id=2685324 \cr
#' This paper has been implemented by above listed authors as the Java program "palmetto". \cr
#' See https://github.com/dice-group/Palmetto or http://aksw.org/Projects/Palmetto.html.
#'
#' @examples
#'
#' library(data.table)
#' library(text2vec)
#' library(Matrix)
#' data("movie_review")
#' N = 500
#' tokens = word_tokenizer(tolower(movie_review$review[1:N]))
#' it = itoken(tokens, progressbar = FALSE)
#' v = create_vocabulary(it)
#' v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.2)
#' dtm = create_dtm(it, vocab_vectorizer(v))
#'
#' n_topics = 10
#' lda_model = LDA$new(n_topics)
#' fitted = lda_model$fit_transform(dtm, n_iter = 20)
#' tw = lda_model$get_top_words(n = 10, lambda = 1)
#'
#' # for demonstration purposes create intrinsic TCM from original documents
#' # scores might not make sense for metrics that are designed for extrinsic TCM
#' tcm = crossprod(sign(dtm))
#'
#' # check coherence
#' logger = lgr::get_logger('text2vec')
#' logger$set_threshold('debug')
#' res = coherence(tw, tcm, n_doc_tcm = N)
#' res
#'
#' # example how to create TCM for extrinsic measures from an external corpus
#' external_reference_corpus = tolower(movie_review$review[501:1000])
#' tokens_ext = word_tokenizer(external_reference_corpus)
#' iterator_ext = itoken(tokens_ext, progressbar = FALSE)
#' v_ext = create_vocabulary(iterator_ext)
#' # for reasons of efficiency vocabulary may be reduced to the terms matched in the original corpus
#' v_ext= v_ext[v_ext$term %in% v$term, ]
#' # external vocabulary may be pruned depending on the use case
#' v_ext = prune_vocabulary(v_ext, term_count_min = 5, doc_proportion_max = 0.2)
#' vectorizer_ext = vocab_vectorizer(v_ext)
#'
#' # for demonstration purposes a boolean co-occurrence within sliding window of size 10 is used
#' # 10 represents sentence co-occurrence, a size of 110 would, e.g., be paragraph co-occurrence
#' window_size = 5
#'
#' tcm_ext = create_tcm(iterator_ext, vectorizer_ext
#'                       ,skip_grams_window = window_size
#'                       ,weights = rep(1, window_size)
#'                       ,binary_cooccurence = TRUE
#'                      )
#' #add marginal probabilities in diagonal (by default only upper triangle of tcm is created)
#' diag(tcm_ext) = attributes(tcm_ext)$word_count
#'
#' # get number of sliding windows that serve as virtual documents, i.e. n_doc_tcm argument
#' n_skip_gram_windows = sum(sapply(tokens, function(x) {length(x)}))
#'
#' @export

coherence = function(x, tcm, metrics = c("mean_logratio", "mean_pmi", "mean_npmi", "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2"), smooth = 1e-12, n_doc_tcm = -1) {
#GENERAL LOGIC OF THE CODE
#The given reference tcm that may be created in different ways (e.g., extrinsic vs. intrinsic, or binary counts vs. actual counts)
#is reduced to the top word space via unique terms found in x.

#Depending on the needs of the metric the count matrix is turned into probability matrix
#by dividing by n_doc_tcm, i.e.,number of actual docs or term windows..

#Considering a loop approach (used by other packages) for creating term index combinations for the calculation,
#different subsets of indices may be built and accessed (see, e.g., textmineR::CalcProbCoherence)
#E.g. one index with all succeeding indices or with all preceding indices.
#For c(1,2,3,4) succeeding combinations would be c(1,2), (1,3), c(1,4), c(2,3), c(2,4), c(3,4)
#and preceding combinations would be c(2,1), c(3, 2), c(3, 1), c(4, 3), c(4, 2), c(4, 1)
#Using a vectorized approach, these combinations represent the lower or upper triangle of the tcm.
#At the first sight, combinations look all the same since tcm is symmetric.
#However, when, e.g., starting to divide by diagonal values (marginal probability),
#it makes a difference if we take the first index as leading index and do
#p[c(1,2)]/p[1,1] for succeeding subset or p[c(2,1)]/p[2,2] for preceding subset.

#The following general considerations are important for vectorized calculations:
#diagonal of tcm represents marginal probabilities, hence, the expression that might be used in a loop approach
#p[x,y]/(p[x,x] * p[y,y])
#translates to
#d = diag(res)
#res = res/d
#res = t(apply(res, 1, function(x) x/d))
#Considering the type of index subsets, hence, which subset of tcm is taken our current understanding is that
#tcm should be left as is after reducing to top word space and then
#for succeeding subset: do calculations with top term indices as is and take upper.triangle
#for preceding subset: sort indices of top term indices decreasingly and take upper.triangle

#After calculation of scores for individual term/index combinations, those are aggregated into a coherence score for one topic.


#CREDITS (apart from theory referred to in @references)
#Codewise, this function was partly inspired by the R packages stm and textmineR,
#of which some basic ideas, e.g., getting/ordering/accessing indices, have been used and adapted.
#stm (Molly Roberts, Brandon Stewart and Dustin Tingley) https://github.com/bstewart/stm/blob/master/R/semanticCoherence.R
#textmineR (Tommy Jones): https://github.com/TommyJones/textmineR/blob/master/R/CalcProbCoherence.R

implemented_metrics = c("mean_logratio", "mean_pmi", "mean_npmi", "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2")
stopifnot(all(metrics %in% implemented_metrics))
n_metrics = length(metrics)
top_terms = as.vector(x)
top_terms_unique = unique(top_terms)
n_topics = ncol(x)
n_terms = nrow(x)
# coherence = data.table(topic = paste0("topic_", 1:n_topics))
stopifnot( identical(colnames(tcm), rownames(tcm)))
terms_tcm = colnames(tcm)
if (length( setdiff(top_terms_unique, terms_tcm) ) > 0) {
   msg = paste("Not all of the top terms 'x' are represented in 'tcm'.",
                "Coherence scores for individual topics will be based on incomplete word sets and will be only partially valid.",
                "Please consider a thorough check of results before further downstream analysis.")
    warning(msg)
}
top_terms_tcm = intersect(top_terms_unique, terms_tcm)
tcm = as.matrix(tcm[top_terms_tcm, top_terms_tcm])
terms_tcm = colnames(tcm)

res = matrix(NA_real_, nrow = n_topics, ncol = n_metrics,
            dimnames = list(paste0("topic_", 1:n_topics), metrics))
  # calculate coherence for each topic
  for(i in seq_len(n_topics)) {
    topic_i_term_indices = match(x[, i], terms_tcm)
    #remove NA indices - not all top terms for topic 'i' are necessarily included in tcm
    topic_i_term_indices = topic_i_term_indices[!is.na(topic_i_term_indices)]
    for(j in seq_len(n_metrics)) {
      m = metrics[j]
      logger$debug("calculating coherence metric '%s' for topic %d", m, i)
      res[i, j] = calc_coherence(m, topic_i_term_indices, tcm, smooth, n_doc_tcm = n_doc_tcm)
    }
  }
  res
}

calc_coherence = function(metric, term_indices, tcm, smooth, ...) {
  switch(metric,
         "mean_logratio" =         coherence_mean_logratio         (term_indices, tcm, smooth, ...),
         "mean_pmi" =              coherence_mean_pmi              (term_indices, tcm, smooth, ...),
         "mean_npmi" =             coherence_mean_npmi              (term_indices, tcm, smooth, ...),
         "mean_difference" =       coherence_mean_difference       (term_indices, tcm, smooth, ...),
         "mean_npmi_cosim" =       coherence_mean_npmi_cosim       (term_indices, tcm, smooth, ...),
         "mean_npmi_cosim2" =      coherence_mean_npmi_cosim2      (term_indices, tcm, smooth, ...),
         stop(sprintf("don't know how to calculate metric '%s'", metric))
  )
}

coherence_mean_logratio = function(term_indices, tcm, smooth, ...) {
  #given suitably ordered pairs of indices stored in two column matrix "indices" a non-vectorized calculation would be something like
  #mapply(function(x,y) {log(smooth + tcm[x,y]) - log(tcm[y,y])}, indices[,1], indices[,2])
  res = NA
  if(length(term_indices) >= 2) {
    term_indices = sort(term_indices, decreasing = TRUE)
    res = tcm[term_indices, term_indices]
    res[upper.tri(res)] = res[upper.tri(res)] + smooth
    d = diag(res)
    res = t(res)
    res = res / d
    res = res[lower.tri(res)]
    res = log(res)
    res = mean(res, na.rm = T)
  }
  return(res)
}

coherence_mean_pmi = function(term_indices, tcm, smooth, n_doc_tcm, ...) {
  #given suitably ordered pairs of indices stored in two column matrix "indices" a non-vectorized calculation would be something like
  #mapply(function(x, y)  {log2((tcm[x,y]/n_doc_tcm) + smooth) - log2(tcm[x,x]/n_doc_tcm) - log2(tcm[y,y]/n_doc_tcm)}, indices[,1], indices[,2])
  stopifnot(n_doc_tcm > 0L)
  res = NA
  if(length(term_indices) >= 2) {
    res = tcm[term_indices, term_indices] / n_doc_tcm
    res[upper.tri(res)] = res[upper.tri(res)] + smooth
    d = diag(res)
    res = res/d
    res = res %*% diag(1 / d)
    res = res[upper.tri(res)]
    res = log2(res)
    res = mean(res, na.rm = T)
  }
  return(res)
}

coherence_mean_difference = function(term_indices, tcm, smooth, n_doc_tcm, ...) {
  #given suitably ordered pairs of indices stored in two column matrix "indices" a non-vectorized calculation would be something like
  #mapply(function(x,y)  {tcm[x,y]/tcm[x,x] - (tcm[y,y]/n_tcm_windows)}, indices[,1], indices[,2])
  stopifnot(n_doc_tcm > 0L)
  res = NA
  if (length(term_indices) >= 2) {
    res = tcm[term_indices, term_indices] / n_doc_tcm
    d = diag(res)
    res = res/d
    res = t(res) - d
    res = res[lower.tri(res)]
    res = mean(res, na.rm = T)
}
  return(res)
}

coherence_mean_npmi = function(term_indices, tcm, smooth, n_doc_tcm, ...) {
  #given suitably ordered pairs of indices stored in two column matrix "indices" a non-vectorized calculation would be something like
  #mapply(function(x, y)  {(log2((tcm[x,y]/n_doc_tcm) + smooth) - log2(tcm[x,x]/n_doc_tcm) - log2(tcm[y,y]/n_doc_tcm)) / -log2((tcm[x,y]/n_doc_tcm) + smooth)}}
  #                        , indices[,1], indices[,2])
  stopifnot(n_doc_tcm > 0L)
  res = NA
  if(length(term_indices) >= 2) {
    res = tcm[term_indices, term_indices] / n_doc_tcm
    res[upper.tri(res)] = res[upper.tri(res)] + smooth
    #interim storage of denominator
    denominator =  res[upper.tri(res)]
    d = diag(res)
    res = res/d
    res = res %*% diag(1 / d)
    res = res[upper.tri(res)]
    res = log2(res) / -log2(denominator)
    res = mean(res, na.rm = T)
  }
  return(res)
}

coherence_mean_npmi_cosim = function(term_indices, tcm, smooth, n_doc_tcm, ...) {
  #TODO
  #example of nonvectorized calculation
  stopifnot(n_doc_tcm > 0L)
  res = NA
  if(length(term_indices) >= 2) {
    res = tcm[term_indices, term_indices] / n_doc_tcm
    res[lower.tri(res, diag = F)] = t(res)[lower.tri(res, diag = FALSE)]
    res = res + smooth
    diag(res) = diag(res) - smooth
    #interim storage of denominator
    denominator =  res
    d = diag(res)
    res = res/d
    res = res %*% diag(1 / d)
    res = log2(res) / -log2(denominator)
    #create values for cosine similarity check, for this metric: the sum of all npmi values
    res_compare = t(matrix(rep(colSums(res), nrow(res)), nrow = nrow(res)))
    res = psim2(res, res_compare, method = "cosine", norm = "l2")
    res = mean(res, na.rm = T)
  }
  return(res)
}

coherence_mean_npmi_cosim2 = function(term_indices, tcm, smooth, n_doc_tcm, ...) {
  #TODO
  #example of nonvectorized calculation
  stopifnot(n_doc_tcm > 0L)
  res = NA
  if(length(term_indices) >= 2) {
    res = tcm[term_indices, term_indices] / n_doc_tcm
    res[lower.tri(res, diag = F)] = t(res)[lower.tri(res, diag = FALSE)]
    res = res + smooth
    diag(res) = diag(res) - smooth
    #interim storage of denominator
    denominator =  res
    d = diag(res)
    res = res/d
    res = res %*% diag(1 / d)
    res = log2(res) / -log2(denominator)
    #the following returns symmetric matrix of similarities between each row with each row -> subset triangle
    res = sim2(res, method = "cosine", norm = "l2")
    res = res[upper.tri(res)]
    res = mean(res, na.rm = T)
  }
  return(res)
}
