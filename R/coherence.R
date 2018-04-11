
#' Calculation of various coherence measures for topic models
#'
#'
#' Internally the function uses a smoothing constant to avoid logarithm of zero as proposed by Röder et al., i.e., \code{.1e-12}.
#'
#' @param top_term_matrix A character matrix with the top terms (entries = words) ranked starting with row 1 per topic (columnnames = topics). e.g.,
#'                        For example, the output of text2vec::get_top_words(...).
#' @param twcm A term window co-occurrence matrix that serves as reference to calculate coherence scores for the terms in \code{top_term_matrix}.
#' @param n_twcm_windows The number of windows that served to create the \code{twcm}. Needed to calculate probabilities from counts in the\code{twcm} required by some measures.
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
# #in reality, probably run in parallel with more potential numbers of topics while storing models on disk
# n_topics = c(5,10,25,50,75,100)
# top_terms = lapply(n_topics, function(n) {
#   lda_model = text2vec::LDA$new(n_topics = n)
#   fitted = lda_model$fit_transform(dtm, n_iter = 20)
#   lda_model$get_top_words(n = 10, topic_number = 1L:n, lambda = 1)
# })
# #term window co-occurrence matrix based on binary co-occurrence of terms in window (here full document)
# #intrinsic since same dtm is used as for fitting LDA model
# twcm_int = Matrix::crossprod(sign(dtm))
# diag(twcm_int) = v$term_count
# coherence_scores = lapply(top_terms, function(x) {
#   coherence(top_term_matrix = x
#             ,twcm = twcm_int
#             ,n_twcm_windows = nrow(dtm)
#             ,measure = c("all", "npmi_nonvectorized", "pmi_nonvectorized")
#   )
# })
#
# #for finding the model with regard to number of topics the mean over coherence scores may be used
# #and a comparison of scaled values may be done
# coherence_scores_mean = lapply(coherence_scores, function(x) {
#   x[, lapply(.SD, mean), .SDcols = setdiff(names(x), "Topic")]
# })
# plot_data = rbindlist(coherence_scores_mean)
# #scale scores between 0 and 1
# plot_data[, names(plot_data):=lapply(.SD, function(x){(x-min(x))/(max(x)-min(x))})]
# #plot values of each coherence measure
# plot_colors <- c("black", "red", "orange", "blue", "purple", "darkgreen", "brown", "darkred", "darkblue")
# plot(x = 1:length(n_topics), unlist(plot_data[,1]), xlab = "n_topics", ylab = "coherence", type = "l", col = plot_colors[1],  xaxt = "n")
# axis(1, at = 1:length(n_topics), labels = n_topics)
# for (i in 2:ncol(plot_data)) {
#   lines(plot_data[,i, with = F], col = plot_colors[i])
# }
# #mark the vectorized versions with points
# colnames(plot_data)
# #pmi
# points(plot_data[,3, with = F], col = plot_colors[3]) #vectorized
# lines(plot_data[,2, with = F], col = plot_colors[2]) #non vectorized
# #npmi
# points(plot_data[,5, with = F], col = plot_colors[4]) #vectorized
# points(plot_data[,4, with = F], col = plot_colors[4]) #non vectorized
# legend(x = "bottomright", legend = colnames(plot_data), text.col = plot_colors, cex = 0.75)


coherence =  function( top_term_matrix
                            ,twcm
                            ,n_twcm_windows = NULL
                            ,measure = c("npmi") #or, e.g. c("npmi", "pmi", ...)
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
  #(i) implement additional measures, e.g., Fitelson coherence
  #    this would require to check one word against a list of words (-> severaö changes in formulas)
  #(ii) vectorize NPMI correctly


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
  if (!isSymmetric(twcm)) {
      if (any((sign(twcm[upper.tri(twcm, diag = F)]) + sign(t(twcm)[upper.tri(twcm, diag = F)])) > 1)) {
            stop("Input twcm is not symmetric or not coercible to symmetric matrix.
                  Entries of upper and lower triangle, e.g., twcm[x,y] and twcm[y,x], both are >1
                  where one of the entries should be zero for converting to symmetric matrix.")
      }
      #fill missing values of upper triangle with the entries of the lower triangle and vice versa
      #NOTE, it would suffice to work with one of the triangles only
      #turning to full symmetric matrix just done for not concerning about logic of index subsetting with regard to upper/lower triangle
      #since matrix is dense at this point anyway, this sould be irrelevant from memory footprint perspective
      twcm[upper.tri(twcm, diag = F)] = twcm[upper.tri(twcm, diag = F)] + t(twcm)[upper.tri(twcm, diag = F)]
      twcm[lower.tri(twcm, diag = F)] =  t(twcm)[lower.tri(twcm, diag = F)]
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


#TODO
#the followig helper function can be deleted in final version
#since vectorization for NPMI is not fully working, yet, the function is kept
#to allow comparisons between looped and vectorized calculation

#HELPER FUNCTION TO CREATE IDX COMBINATIONS-----------------------------------------
  #' @name word_index_combinations
  #' Create combinations of indices for subsetting upper triangle (incl. diag) of a matrix
  #'
  #'Function serves to create word indices for subsetting a reference "term (document-)co-occurrence matrix"
  #'to calculate coherence scores for topics. Basically, the function customizes the output of utils::combn.
  #'Following assumptions are made about the matrix:
  #'- only upper triangle is considered,
  #'  matrix is assumed to be of symmetric nature (not necessarily its actual appearance,
  #'  actual entries in lower triangle may be filled with zeros)
  #'- diagonal of matrix contains total document occurrence of terms (or marginal probability).
  #'- diagonal is decreasingly ordered from left to right
  #'
  #' @param term_indices Integer vector containing indices to combine
  #' @param comb_type Type of combinations to create.
  #'                  one_idx-preceeding_idxs:
  #'                  Follows the logic for coherence scores of SUM(from i=2 to N)SUM(from j=1 to i-1)
  #'                  one_idx-succeeding_idxs:
  #'                  Follows the logic for coherence scores of SUM(from i=1 to N-1)SUM(from j=i+1 to N)
  #'                  one_idx-preceeding_idxs-topic_order:
  #'                  Same as one_idx-preceeding_idxs, but using an the original topic order that has to be specified.
  #'                  This is used to create indices required for UMass coherence measure.
  #'
  #' @param topic_order Integer vector of alternative topic order of indices to be assumed for matrix,
  #'                    in contrast to an ordered diagonal for restoring original order of top terms per topic.
  #'                    Alternative topic order may be created, e.g., via:
  #'                    match(terms_in_desired_order, colnames(tcm))
  #'
  #' @return A two column matrix containing desired combinations of indices in each row.
  #'
  #' @examples
  #' #'
  #' idxs = c(1,2,3) #e.g. as in tcm ordered by diagonal
  #' idxs_as_in_topic = c(2,1,3) #order of indices (corresponding terms) in topic
  #'
  #' word_index_combinations(idxs, comb_type = "one_idx-succeeding_idxs")
  #' #       [,1] [,2]
  #' # [1,]    1    2
  #' # [2,]    1    3
  #' # [3,]    2    3
  #'
  #' word_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs", )
  #' #       [,1] [,2]
  #' # [1,]    2    1
  #' # [2,]    3    1
  #' # [3,]    3    2
  #'
  #' word_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs-topic_order"
  #'           , topic_order = match(idxs_as_in_topic, idxs))
  #' #       [,1] [,2]
  #' # [1,]    3    1
  #' # [2,]    3    2
  #' # [3,]    1    2
  #'

  term_index_combinations = function(term_indices, comb_type = "one_idx-succeeding_idxs",  topic_order = NULL) {
    if (comb_type == "one_idx-preceeding_idxs") {
      idx_combs = t(combn(term_indices,2, FUN = function(y) sort(y, decreasing = TRUE)))
    } else if (comb_type == "one_idx-succeeding_idxs") {
      idx_combs = t(combn(term_indices,2, FUN = function(y) sort(y, decreasing = FALSE)))
    } else if (comb_type == "one_idx-preceeding_idxs-topic_order") {
      #for asymmetric sets the original order of words (hence, indexes of tcm) has to be restored
      reorder = order(match(term_indices, topic_order), decreasing = TRUE)
      term_indices = term_indices[reorder]
      #in contrast to the other subsets, no additional reordering of indices in combn at this point
      #to maintain original topic order
      idx_combs = t(combn(term_indices,2))
    }
    idx_combs
  }


#CALCULATE COHERENCE----------------------------------------------------------------


#following implementation with log_smooth_constant = .01, which is used in stm package,
#was used for validation of results (at least for that measure) but not kept in final
#selection of measures, since smaller constant performs better

# if ("UMass_stm" %in% measure | "all" %in% measure) {
#             #note in text2vec implementation the smoothing constant is not added in the denominator
#             coherence[,sum_logratio_stm_pckg:= sapply(twcm_term_idxs, function(idxs) {
#                               if (length(idxs) < 2) {
#                                 return(NA)
#                               } else {
#                                 coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs-topic_order", topic_order = restore_topic_order))
#                                 coh[, score:= mapply(function(x,y) {log(.01 + twcm[x,y]) - log(.01 + twcm[y,y])}
#                                                      ,V1, V2)]
#                                 return(coh[, round(sum(score, na.rm = T), d = 4)])
#                               }
#             })]
# }
#
#
#   if ("UMass_stm_vectorized" %in% measure | "all" %in% measure) {
#     coherence[,sum_logratio_stmlike_uppertri3:= sapply(twcm_term_idxs, function(idxs) {
#       if (length(idxs) < 2) {
#         return(NA)
#       } else {
#         reorder = order(match(idxs, restore_topic_order), decreasing = TRUE)
#         idxs = idxs[reorder]
#         res = twcm[idxs, idxs] + .01
#         res = res/rev(diag(res))
#         res = log(res[upper.tri(res)])
#         return(round(sum(res, na.rm = T), d = 4))
#       }
#     })]
#   }


#logratio resembles UMASS measure
if ( "mean_logratio_nonvectorized" %in% measure) {
coherence[,mean_logratiopmi_nonvectorized:= sapply(twcm_term_idxs, function(idxs) {
                if (length(idxs) < 2) {
                  return(NA)
                } else {
                  coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs-topic_order", topic_order = restore_topic_order))
                  coh[, score:= mapply(function(x,y) {log(log_smooth_constant + twcm[x,y]) - log(twcm[y,y])}
                                       ,V1, V2)]
                  return(coh[, round(mean(score, na.rm = T), d = 4)])
                }
  })]
}


  if ("mean_logratio" %in% measure | "all" %in% measure) {
    coherence[,mean_logratio:= sapply(twcm_term_idxs, function(idxs) {
      if (length(idxs) < 2) {
        return(NA)
      } else {
        reorder = order(match(idxs, restore_topic_order), decreasing = TRUE)
        idxs = idxs[reorder]
        res = twcm[idxs, idxs]
        res[upper.tri(res)] = res[upper.tri(res)] + log_smooth_constant
        res = res/rev(diag(res))
        res = log(res[upper.tri(res)])
        return(round(mean(res, na.rm = T), d = 4))
      }
    })]
  }



if ( "pmi_nonvectorized" %in% measure) {
    coherence[,mean_pmi_nonvectorized:= sapply(twcm_term_idxs, function(idxs) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs"))
                      coh[, score:= mapply(function(x,y)  {log2((twcm[x,y]/n_twcm_windows) + log_smooth_constant) - log2(twcm[x,x]/n_twcm_windows) - log2(twcm[y,y]/n_twcm_windows)}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
      })]
}


if ( "pmi" %in% measure | "all" %in% measure) {
    coherence[,mean_pmi:= sapply(twcm_term_idxs, function(idxs) {
      if (length(idxs) < 2) {
        return(NA)
      } else {
        idxs = sort(idxs)
        #res = log_smooth_constant  + (twcm[idxs, idxs]/n_twcm_windows)
        res = twcm[idxs, idxs]/n_twcm_windows
        res[upper.tri(res)] = res[upper.tri(res)] + log_smooth_constant
        #divide each entry by values of p[wi,wi] and p[wj, wj], i.e., the entries in the diagonal
        res = res/(diag(res)*rev(diag(res)))
        res = res[upper.tri(res)]
        res = log2(res)
        return(round(mean(res, na.rm = T), d = 4))
      }
    })]
}


if ( "npmi_nonvectorized" %in% measure) {
coherence[,mean_npmi_nonvectorized:= sapply(twcm_term_idxs, function(idxs) {
                if (length(idxs) < 2) {
                  return(NA)
                } else {
                  coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-preceeding_idxs"))
                  coh[, score:= mapply(function(x,y)   {(log2((twcm[x,y]/n_twcm_windows) + log_smooth_constant) - log2(twcm[x,x]/n_twcm_windows) - log2(twcm[y,y]/n_twcm_windows)) /  -log2((twcm[x,y]/n_twcm_windows) + log_smooth_constant)}
                                       ,V1, V2)]
                  return(coh[, round(mean(score, na.rm = T), d = 4)])
                }
  })]
}


  #TODO
  #vectorized version of NPMI does not produce exact same results as mapply loop version
  #the course of the curve seems to be sufficiently similar, hence, the proposed number of topics
  #should be in the same range
  #have not found the error, yet...
  if ( "npmi" %in% measure | "all" %in% measure) {
    coherence[,mean_npmi:= sapply(twcm_term_idxs, function(idxs) {
      if (length(idxs) < 2) {
        return(NA)
      } else {
        idxs = sort(idxs)
        #make probabilistic matrix and add smoothing constant
        res = twcm[idxs, idxs]/n_twcm_windows
        res[upper.tri(res)] = res[upper.tri(res)] + log_smooth_constant
        #interim storage of denominator
        denominator =  res[upper.tri(res)]
        #divide each entry by values of p[wi,wi] and p[wj, wj], i.e., the entries in the diagonal
        res = res/(diag(res)*rev(diag(res)))
        res = res[upper.tri(res)]
        #TODO normalization in the following way seems to be wrong
        res = log2(res) / -log2(denominator)
        return(round(mean(res, na.rm = T), d = 4))
      }
    })]
  }


if ( "prob_diff_nonvectorized" %in% measure) {
    coherence[,mean_prob_diff_nonvectorized:= sapply(twcm_term_idxs, function(idxs) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-succeeding_idxs"))
                      coh[, score:= mapply(function(x,y)  {twcm[y,x]/twcm[y,y] - (twcm[y,y]/n_twcm_windows)}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
      })]

}

    if ( "prob_diff" %in% measure | "all" %in% measure) {
      coherence[,mean_prob_diff:= sapply(twcm_term_idxs, function(idxs) {
        if (length(idxs) < 2) {
          return(NA)
        } else {
          idxs = sort(idxs, decreasing = TRUE)
          res = twcm[idxs, idxs]
          #divide each entry by values of p[wj,wj], i.e., the entries in the diagonal
          #and substract marginal probability of diagonal term
          res = (res/diag(res)) -  diag(res)/n_twcm_windows
          res = res[upper.tri(res)]
          return(round(mean(res, na.rm = T), d = 4))
        }
      })]
    }


if ("prob_diff2_nonvectorized" %in% measure) {
    coherence[,mean_prob_diff2_nonvectorized:= sapply(twcm_term_idxs, function(idxs) {
      if (length(idxs) < 2) {
        return(NA)
      } else {
        coh = as.data.table(term_index_combinations(idxs, comb_type = "one_idx-succeeding_idxs"))
        coh[, score:= mapply(function(x,y)  {twcm[y,x]/twcm[x,x] - (twcm[x,x]/n_twcm_windows)}
                             ,V1, V2)]
        return(coh[, round(mean(score, na.rm = T), d = 4)])
      }
    })]
}


  if ( "prob_diff2" %in% measure | "all" %in% measure) {
    coherence[,mean_prob_diff:= sapply(twcm_term_idxs, function(idxs) {
      if (length(idxs) < 2) {
        return(NA)
      } else {
        idxs = sort(idxs, decreasing = FALSE)
        res = twcm[idxs, idxs]
        #divide each entry by values of p[wj,wj], i.e., the entries in the diagonal
        #and substract marginal probability of diagonal term
        res = (res/diag(res)) -  diag(res)/n_twcm_windows
        res = res[upper.tri(res)]
        return(round(mean(res, na.rm = T), d = 4))
      }
    })]
  }


#TODO use improved vectorized version of NPMI implementation for cosim measures as soon as available
  if ( "mean_cosim_npmi" %in% measure | "all" %in% measure) {
    coherence[,mean_cosim_npmi:= sapply(twcm_term_idxs, function(idxs) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      res = log_smooth_constant  + (twcm[idxs, idxs]/n_twcm_windows)
                      diag(res) = diag(res) - log_smooth_constant
                      denominator =  -log2(res)
                      res = res/(diag(res)*rev(diag(res)))
                      res = log2(res)
                      #normalize
                      res = res/denominator
                      #create values for cosine similarity check, here: the sum of all npmi values
                      res_compare = t(matrix(rep(colSums(res), nrow(res)), nrow = nrow(res)))
                      res = psim2(res, res_compare, method = "cosine", norm = "l2")
                      return(round(mean(res, na.rm = T), d = 4))
                    }
      })]
  }




  if ( "mean_cosim_npmi2" %in% measure | "all" %in% measure) {
    coherence[,mean_cosim_npmi2:= sapply(twcm_term_idxs, function(idxs) {
      if (length(idxs) < 2) {
        return(NA)
      } else {
        res = log_smooth_constant  + (twcm[idxs, idxs]/n_twcm_windows)
        diag(res) = diag(res) - log_smooth_constant
        denominator =  -log2(res)
        res = res/(diag(res)*rev(diag(res)))
        res = log2(res)
        #normalize
        res = res/denominator
        #the following returns symmetric matrix of similarities between each row with each row -> subset triangle
        res = sim2(res, method = "cosine", norm = "l2")
        res = res[upper.tri(res)]
        return(round(mean(res, na.rm = T), d = 4))
      }
    })]
  }

  #clean output and return
  coherence[, twcm_term_idxs := NULL]
  return(coherence[])
}
