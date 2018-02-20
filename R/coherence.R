
#' Calculation of various coherence measures for topic models
#'
#'
#'
#' @param top_term_matrix A character matrix with the top terms (entries = words) ranked starting with row 1 per topic (columnnames = topics). e.g.,
#'                        For example, the output of text2vec::get_top_words(...).
#' @param twcm A term co-occurrence matrix that serves as reference to calculate coherence scores for the terms in \code{top_term_matrix}.
#' @param n_twcm_windows The number of documents that served to create the \code{twcm} to calculate probabilities from counts in the\code{twcm} used as input to some coherence measures.
#' @param log_smooth_constant Smoothing constant to avoid logarithm of zero in calcualtions, for example, \code{log(log_smooth_constant + 0)}.
#'                            By default \code{.1e-12} as used by Röder et al. (palmetto).
#'                            Use \code{1} for UMass logratio as used by Mimno.
#'                            Use \code{.01} for logratio as used in stm package.
#'
#' @return A \code{data.table} showing the various coherence scores per topic (or average over all topics).
#' @export
#'
#' @examples
#'


# library(text2vec)
# data("movie_review")
# N = 500
# tokens = word_tokenizer(tolower(movie_review$review[1:N]))
# it = itoken(tokens, progressbar = F)
# v = create_vocabulary(it)
# v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.2)
# dtm = create_dtm(it, vocab_vectorizer(v))
# #create models with different number of topics and get top terms per topic
# #in reality, probably run in parallel with more potential numbers of topics while sotring models on disk
# n_topics = c(5,10, 25, 50)
# top_terms = lapply(n_topics, function(n) {
#   lda_model = text2vec::LDA$new(n_topics = n)
#   fitted = lda_model$fit_transform(dtm, n_iter = 20)
#   lda_model$get_top_words(n = 10, topic_number = 1L:n, lambda = 1)
# })
# #term window co-occurrence matrix based on binary co-occurrence of terms in window (here full document)
# #intrinsic since same dtm is used as for fitting LDA model
# twcm_int <- Matrix::crossprod(sign(dtm))
# diag(twcm_int) <- v$term_count
# coherence_scores <- lapply(top_terms, function(x) {
#   coherence(top_term_matrix = x
#             ,twcm = twcm_int
#             ,n_twcm_windows = nrow(dtm)
#   )
# })
#
# #for finding the model with regard to number of topics the mean over coherence scores may be used
# #and a comparison of scaled values may be done
# coherence_scores_mean <- lapply(coherence_scores, function(x) {
#   x[, lapply(.SD, mean), .SDcols = setdiff(names(x), "Topic")]
# })
# plot_data = rbindlist(coherence_scores_mean)
# #scale scores between 0 and 1
# plot_data[, names(plot_data):=lapply(.SD, function(x){(x-min(x))/(max(x)-min(x))})]
# #plot values of each coherence measure
# plot(x = 1:length(n_topics), plot_data$sum_logratio_UMass, xlab = "n_topics", ylab = "coherence", type = "l", col = 1, xaxt = "n")
# axis(1, at = 1:length(n_topics))
# for (i in 2:ncol(plot_data)) {
#   lines(plot_data[,i, with = F], col = i)
# }
# legend(x = "left", legend = colnames(plot_data), text.col = 1:ncol(plot_data), cex = 0.75)


coherence =  function( top_term_matrix
                            ,twcm
                            ,n_twcm_windows = NULL
                            ) {

#GENERAL LOGIC--------------------------------------------------------
  #STEPS PPREP_1 to PREP_3 have to be performed separately from this function
  #PREP_1 get top N words per topic
  #PREP_2 reduce dtm to top N word space
  #PREP_3 create twcm with document co-occurence of top N words (binarize dtm, do cross product)
  #(PREP_3b - divide twcm by n_twcm_windows gives probability, since some Coherence measures, e.g., UMass originally use counts,
  #      the division is done at later step)

  #calculate coherence for each topic (following steps 4.x are done individually for each topic)
  #1 reduce twcm to top n word space
  #2 create pairs of twcm reference indices of wi / wj to be used for coherence calculation
  #3 get values for all wiwj pairs from twcm and calculate score for each pair given a calculation rule specfied by particular coherence measure
  #     e.g. pmi = function(wi, wj, n_twcm_windows, twcm)  {log2((twcm[wi,wj]/n_twcm_windows) + 1e-12) - log2(twcm[wi,wi]/n_twcm_windows) - log2(twcm[wj,wj]/n_twcm_windows)}
  #4 aggregate the results via mean over number of wiwj pairs (and if desired aggreagte over all topics)

  #TODO
  #(ii) use word vectors for wi / wj instead of single words, hence, subsets such as S_one_any, etc.
  #     creating, e.g., one any subsets requires to store one index against a list of indices, hence, formulas need
  #     adaption, e.g., something like twcm[unlist(wi), unlist(wj)] might work
  #(iii)Currently indices of subsets are stored in memory, this might be turned to dynamic creation of indices to spare memory usage
  #     the lines coherence[,twcm_term_idxs := split(match... would have to be incorporated into word_index_combinations

#CREDITS / REFERENCES -------------------------------------------------
  #The following paper is the main theoretical basis for this code
  #https://dl.acm.org/citation.cfm?id=2685324
  #Authors: Roeder, Michael; Both, Andreas; Hinneburg, Alexander (2015)
  #Title: Exploring the Space of Topic Coherence Measures.
  #In: Xueqi Cheng, Hang Li, Evgeniy Gabrilovich und Jie Tang (Hg.):
  #Proceedings of the Eighth ACM International Conference on Web Search and Data Mining - WSDM '15.
  #the Eighth ACM International Conference. Shanghai, China, 02.02.2015 - 06.02.2015.
  #New York, New York, USA: ACM Press, S. 399-408.
  #The paper has already been implemented as a Java implementation "palmetto" that exceeds the current functionality of this R implementation
  #Main Author / Maintainer: Michael Roeder
  #https://github.com/dice-group/Palmetto
  #http://aksw.org/Projects/Palmetto.html

  #some ideas for getting/ordering/accessing indices of twcm have been inspired by packages stm and textmineR
  #(although, these packages use different functions to achieve the same result)
  #stm: Molly Roberts, Brandon Stewart and Dustin Tingley
  #https://github.com/bstewart/stm/blob/master/R/semanticCoherence.R
  #textmineR: Tommy Jones
  #https://github.com/TommyJones/textmineR/blob/master/R/CalcProbCoherence.R

#INITIAL----------------------------------------------------------------------------
  top_terms = as.vector(top_term_matrix)
  top_terms_unique = unique(top_terms)
  n_topics = ncol(top_term_matrix)
  n_terms = nrow(top_term_matrix)
  log_smooth_constant = .1e-12
  coherence = data.table(Topic = paste0("T", 1:n_topics))

#PREPARE twcm------------------------------------------------------------------------
  #some basic checks on the input
  stopifnot( setequal(colnames(twcm),rownames(twcm)), ncol(twcm) == nrow(twcm))
  #check if twcm includes all top terms
  if ( !(setequal(intersect(top_terms_unique, colnames(twcm)), top_terms_unique)) ) {
    warning("Not all terms of top_term_matrix are included in twcm.
             Coherence scores for individual topics will be based on incomplete word sets and are only partially valid.
             Please consider a thorough check of results before further downstream analysis.")
  }
  #reduce twcm to top word space
  twcm = twcm[rownames(twcm) %in% top_terms_unique,colnames(twcm) %in% top_terms_unique]
  #change class for faster subsetting, see following comments on change of class
  twcm = as.matrix(twcm)
  #when (input) twcm is a (larger) sparse matrix the code is quite slow
  #speed of frequent subsetting of sparse twcm is the bottleneck, example:
  # m = cbind(A = c(1,0,0,0), B = c(1,0,0,0), C = c(0,0,0,1))
  # m.sp = Matrix(m, sparse = T)
  # microbenchmark::microbenchmark(
  #   m[3,3],
  #   m.sp[3,3]
  # )
  # Unit: nanoseconds
  #      expr     min       lq      mean median     uq    max neval
  # m[3, 3]       790    791.0   1492.89   1579   1580  13817   100
  # m.sp[3, 3] 144480 147637.5 152647.01 148822 150401 367514   100
  #since twcm is limited on top n word space its size is usually not incredibly large at this point
  #example: 1000 topics with top 20 words would be about 1 Mb
  # format(object.size(
  #     lapply(1:1000, function(x) {
  #       sapply(1:20, function(y) {
  #        paste(sample(letters, 7), collapse = "")
  #       })
  #     })
  #   ), units = "Mb")
  #hence, using base::matrix class seems acceptable for faster subsetting instead of potential sparseMatrix input class

  #make matrix symmetric, input twcm might not be symmetric in terms of (i) order of terms in cols/rows and (ii) their values
  twcm = twcm[match(rownames(twcm), colnames(twcm)), ]
  if(!isSymmetric(twcm)) {
      if (any((sign(twcm[upper.tri(twcm, diag = F)]) + sign(t(twcm)[upper.tri(twcm, diag = F)])) > 1)) {
            stop("Input twcm is not symmetric or not coercible to symmetric matrix.
                  Entries of upper and lower triangle, e.g., twcm[x,y] and twcm[y,x], both are >1
                  where one of the entries should be zero for converting to symmetric matrix.")
      }
      #fill missing values of upper triangle with the entries of the lower triangle and vice versa
      #NOTE, it would suffice to work with one of the triangles only
      #turning to full symmetric matrix just done for not concerning about logic of index subsetting with regard to upper/lower triangle
      #since matrix is dense at this point anyway, this sould be irrelevant from memory footprint perspective
      twcm[upper.tri(twcm, diag = F)] <- twcm[upper.tri(twcm, diag = F)] + t(twcm)[upper.tri(twcm, diag = F)]
      twcm[lower.tri(twcm, diag = F)] <-  t(twcm)[lower.tri(twcm, diag = F)]
  }

  #order twcm by term probability (entries in diagonal)
  #from left to right the probability of terms follows the rule p(twcm[i,i]) > p(twcm[i+1,i+1])
  #ordering twcm is relevant for asymmetric measures that require a certain order
  #some asymmetric measures require the original order in the topic (e.g. UMass),
  #which is therefore also stored for re-mapping indices of twcm to this order
  probability_order = order(diag(twcm),  decreasing = TRUE)
  twcm = twcm[probability_order, probability_order]
  restore_topic_order = match(top_terms_unique, colnames(twcm))

#GET REFERENCE INDICES OF TOP TERMS IN twcm FOR EACH TOPIC---------------------------
  #credits for this approach of getting indices go to authors of stm package
  coherence[,twcm_term_idxs := split(match(top_terms, colnames(twcm))
                                     , rep(1:n_topics, each=n_terms))]

  #remove NA values from term idxs (not all top_terms_unique are necessarily included in twcm)
  coherence[, twcm_term_idxs := lapply(twcm_term_idxs, function(x) x[!is.na(x)])]

# If flexible definition of coherence scores is to be implemented the following sections might be a start
# currently each measure is calculated by copy paste like commands to create data.table columns (see below)
# DEFINITION OF COHERENCE MEASURES
# WRAPPER FUNCTION TO CALCULATE COHERENCE

#CALCULATE COHERENCE----------------------------------------------------------------

  #Type 1
  #sum(..., na.rm = T) for aggregation
  #one_pre_topic_order as term indices
  #note the adapted log_smooth_constant to comply original UMass and its implementation in stm package
    coherence[,sum_logratio_UMass:= sapply(twcm_term_idxs, function(idxs) {
                        if (length(idxs) < 2) {
                          return(NA)
                        } else {
                          coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs-topic_order", topic_order = restore_topic_order))
                          coh[, score:= mapply(function(x,y) {log(1 + twcm[x,y]) - log(twcm[y,y])}
                                               ,V1, V2)]
                          return(coh[, round(sum(score, na.rm = T), d = 4)])
                        }
    })]

    #note in text2vec implementation the smoothing constant is not added in the denominator
    coherence[,sum_logratio_stm_pckg:= sapply(twcm_term_idxs, function(idxs) {
                      if (length(idxs) < 2) {
                        return(NA)
                      } else {
                        coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs-topic_order", topic_order = restore_topic_order))
                        coh[, score:= mapply(function(x,y) {log(.01 + twcm[x,y]) - log(.01 +twcm[y,y])}
                                             ,V1, V2)]
                        return(coh[, round(sum(score, na.rm = T), d = 4)])
                      }
    })]

    coherence[,sum_logratio_stmlike_uppertri:= sapply(twcm_term_idxs, function(idxs) {
      if (length(idxs) < 2) {
        return(NA)
      } else {
        reorder <- order(match(idxs, restore_topic_order), decreasing = TRUE)
        idxs <- idxs[reorder]
        res = twcm[idxs, idxs] + .01
        res = apply(res, 2, function(x) x/(diag(res)))
        res = log(res[upper.tri(res)])
        return(round(sum(res, na.rm = T), d = 4))
      }
    })]

    coherence[,sum_logratio_stmlike_lowertri:= sapply(twcm_term_idxs, function(idxs) {
      if (length(idxs) < 2) {
        return(NA)
      } else {
        reorder <- order(match(idxs, restore_topic_order), decreasing = TRUE)
        idxs <- idxs[reorder]
        res = twcm[idxs, idxs]
        res = apply(res, 2, function(x) x/(diag(res)+.01))
        res = log(.01 + res[lower.tri(res)])
        return(round(sum(res, na.rm = T), d = 4))
      }
    })]


    #Type 2
    #mean(..., na.rm = T) for aggregation
    #one_pre_topic_order as term indices
    coherence[,mean_prob_logratio_Torder:= sapply(twcm_term_idxs, function(idxs) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs-topic_order", topic_order = restore_topic_order))
                      coh[, score:= mapply(function(x,y) {log(log_smooth_constant + twcm[x,y]) - log(twcm[y,y])}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
      })]

    coherence[,mean_pmi:= sapply(twcm_term_idxs, function(idxs) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs"))
                      coh[, score:= mapply(function(x,y)  {log2((twcm[x,y]/n_twcm_windows) + log_smooth_constant) - log2(twcm[x,x]/n_twcm_windows) - log2(twcm[y,y]/n_twcm_windows)}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
      })]

    coherence[,mean_pmi_uppertri:= sapply(twcm_term_idxs, function(idxs) {
      if (length(idxs) < 2) {
        return(NA)
      } else {
        idxs = sort(idxs)
        res = log_smooth_constant  + (twcm[idxs, idxs]/n_twcm_windows)
        res = res/diag(res)
        res = t(res)/diag(res)
        res = res[upper.tri(res)]
        res = log2(res)
        return(round(mean(res, na.rm = T), d = 4))
      }
    })]

    coherence[,mean_pmi_lowertri:= sapply(twcm_term_idxs, function(idxs) {
      if (length(idxs) < 2) {
        return(NA)
      } else {
        idxs = sort(idxs)
        res = log_smooth_constant  + (twcm[idxs, idxs]/n_twcm_windows)
        res = res/diag(res)
        res = t(res)/diag(res)
        res = res[lower.tri(res)]
        res = log2(res)
        return(round(mean(res, na.rm = T), d = 4))
      }
    })]

    coherence[,mean_npmi:= sapply(twcm_term_idxs, function(idxs) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs"))
                      coh[, score:= mapply(function(x,y)   {(log2((twcm[x,y]/n_twcm_windows) + log_smooth_constant) - log2(twcm[x,x]/n_twcm_windows) - log2(twcm[y,y]/n_twcm_windows)) /  -log2((twcm[x,y]/n_twcm_windows) + log_smooth_constant)}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
      })]


    #Type 3
    #mean(..., na.rm = T) for aggregation
    #one_suc as term indices
    coherence[,mean_prob_diff:= sapply(twcm_term_idxs, function(idxs) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-succeeding_idxs"))
                      coh[, score:= mapply(function(x,y)  {twcm[y,x]/twcm[y,y] - (twcm[y,y]/n_twcm_windows)}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
      })]

  coherence[, twcm_term_idxs := NULL]
  return(coherence[])
}
