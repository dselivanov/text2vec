

#TODO DOC: short explanation of implemented coherence measures
#TODO maybe remove comb_idxs / improve names if kept

#' Calculation of various coherence measures for topic models
#'
#'
#'
#' @param top_term_matrix A character matrix with the top terms (entries = words) ranked starting with row 1 per topic (columnnames = topics). e.g.,
#'                        For example, the output of text2vec::get_top_words(...).
#' @param tcm A term co-occurrence matrix that serves as reference to calculate coherence scores for the terms in \code{top_term_matrix}.
#' @param n_tcm_windows The number of documents that served to create the \code{tcm} to calculate probabilities from counts in the\code{tcm} used as input to some coherence measures.
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
#' dtm = cbind(A = c(1,2,3,0, 0), B = c(4,0,5,0, 0), C = c(0,6,7,8, 0), D = c(0,0,0,1, 1))
#' topic_word_distribution = rbind(T1 = c(0.5,0.3,0.19, 0.01), T2 = c(0.19,0.3,0.5, 0.01),  T3 = c(0.3,0.5,0.19, 0.01))
#' colnames(topic_word_distribution) = c("A", "B", "C", "D")
#' n_topterms = 2
#' top_term_mat = apply(topic_word_distribution, 1 , function(x) order(x, decreasing = T))[1:n_topterms, ]
#' top_term_mat = apply(top_term_mat, 2 , function(x) colnames(topic_word_distribution)[x])
#' dtm_top_terms = dtm[,(unique(as.vector(top_term_mat)))]
#' dtm_top_terms[dtm_top_terms > 1] = 1
#' tcm_top_terms = crossprod(dtm_top_terms)
#'
#' coherence( tcm = tcm_top_terms
#'                , top_term_matrix = top_term_mat
#'                , n_tcm_windows = nrow(dtm))
#' # Topic logratio_UMass logratio prob_logratio    pmi    npmi prob_dif
#' # 1:    T1        -0.4055  -0.4055       -0.4055  0.737  0.5575      0.6
#' # 2:    T2        -0.6931  -1.0986       -1.0986 -0.263 -0.1133      0.1
#' # 3:    T3        -0.4055  -0.4055       -0.4055  0.737  0.5575      0.6


coherence =  function( top_term_matrix
                            ,tcm
                            ,n_tcm_windows = NULL
                            ) {

#GENERAL LOGIC--------------------------------------------------------
  #STEPS PPREP_1 to PREP_3 have to be performed separately from this function
  #PREP_1 get top N words per topic
  #PREP_2 reduce dtm to top N word space
  #PREP_3 create tcm with document co-occurence of top N words (binarize dtm, do cross product)
  #(PREP_3b - divide tcm by n_tcm_windows gives probability, since some Coherence measures, e.g., UMass originally use counts,
  #      the division is done at later step)

  #calculate coherence for each topic (following steps 4.x are done individually for each topic)
  #1 reduce tcm to top n word space
  #2 create pairs of tcm reference indices of wi / wj to be used for coherence calculation
  #3 get values for all wiwj pairs from tcm and calculate score for each pair given a calculation rule specfied by particular coherence measure
  #     e.g. pmi = function(wi, wj, n_tcm_windows, tcm)  {log2((tcm[wi,wj]/n_tcm_windows) + 1e-12) - log2(tcm[wi,wi]/n_tcm_windows) - log2(tcm[wj,wj]/n_tcm_windows)}
  #4 aggregate the results via mean over number of wiwj pairs (and if desired aggreagte over all topics)

  #TODO
  #(ii) use word vectors for wi / wj instead of single words, hence, subsets such as S_one_any, etc.
  #     creating, e.g., one any subsets requires to store one index against a list of indices, hence, formulas need
  #     adaption, e.g., something like tcm[unlist(wi), unlist(wj)] might work
  #(iii)Currently indices of subsets are stored in memory, this might be turned to dynamic creation of indices to spare memory usage
  #     the lines coherence[,tcm_term_idxs := split(match... would have to be incorporated into word_index_combinations

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

  #some ideas for getting/ordering/accessing indices of tcm have been inspired by packages stm and textmineR
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

#PREPARE TCM------------------------------------------------------------------------
  #some basic checks on the input
  stopifnot( setequal(colnames(tcm),rownames(tcm)), ncol(tcm) == nrow(tcm))
  #check if tcm includes all top terms
  if ( !(setequal(intersect(top_terms_unique, colnames(tcm)), top_terms_unique)) ) {
    warning("Not all terms of top_term_matrix are included in tcm.
             Coherence scores for individual topics will be based on incomplete word sets and are only partially valid.
             Please consider a thorough check of results before further downstream analysis.")
  }
  #reduce tcm to top word space
  tcm = tcm[rownames(tcm) %in% top_terms_unique,colnames(tcm) %in% top_terms_unique]
  #change class for faster subsetting, see following comments on change of class
  tcm = as.matrix(tcm)
  #when (input) tcm is a (larger) sparse matrix the code is quite slow
  #speed of frequent subsetting of sparse tcm is the bottleneck, example:
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
  #since tcm is limited on top n word space its size is usually not incredibly large at this point
  #example: 1000 topics with top 20 words would be about 1 Mb
  # format(object.size(
  #     lapply(1:1000, function(x) {
  #       sapply(1:20, function(y) {
  #        paste(sample(letters, 7), collapse = "")
  #       })
  #     })
  #   ), units = "Mb")
  #hence, using base::matrix class seems acceptable for faster subsetting instead of potential sparseMatrix input class

  #make matrix symmetric, input tcm might not be symmetric in terms of (i) order of terms in cols/rows and (ii) their values
  tcm = tcm[match(rownames(tcm), colnames(tcm)), ]
  if(!isSymmetric(tcm)) {
      if (any((sign(tcm[upper.tri(tcm, diag = F)]) + sign(t(tcm)[upper.tri(tcm, diag = F)])) > 1)) {
            stop("Input tcm is not symmetric or not coercible to symmetric matrix.
                  Entries of upper and lower triangle, e.g., tcm[x,y] and tcm[y,x], both are >1
                  where one of the entries should be zero for converting to symmetric matrix.")
      }
      #fill missing values of upper triangle with the entries of the lower triangle and vice versa
      #NOTE, it would suffice to work with one of the triangles only
      #turning to full symmetric matrix just done for not concerning about logic of index subsetting with regard to upper/lower triangle
      #since matrix is dense at this point anyway, this sould be irrelevant from memory footprint perspective
      tcm[upper.tri(tcm, diag = F)] <- tcm[upper.tri(tcm, diag = F)] + t(tcm)[upper.tri(tcm, diag = F)]
      tcm[lower.tri(tcm, diag = F)] <-  t(tcm)[lower.tri(tcm, diag = F)]
  }

  #order tcm by term probability (entries in diagonal)
  #from left to right the probability of terms follows the rule p(tcm[i,i]) > p(tcm[i+1,i+1])
  #ordering tcm is relevant for asymmetric measures that require a certain order
  #some asymmetric measures require the original order in the topic (e.g. UMass),
  #which is therefore also stored for re-mapping indices of tcm to this order
  probability_order = order(diag(tcm),  decreasing = TRUE)
  tcm = tcm[probability_order, probability_order]
  restore_topic_order = match(top_terms_unique, colnames(tcm))

#GET REFERENCE INDICES OF TOP TERMS IN TCM FOR EACH TOPIC---------------------------
  #credits for this approach of getting indices go to authors of stm package
  coherence[,tcm_term_idxs := split(match(top_terms, colnames(tcm))
                                     , rep(1:n_topics, each=n_terms))]

  #remove NA values from term idxs (not all top_terms_unique are necessarily included in tcm)
  coherence[, tcm_term_idxs := lapply(tcm_term_idxs, function(x) x[!is.na(x)])]

# If flexible definition of coherence scores is to be implemented the following sections might be a start
# currently each measure is calculated by copy paste like commands to create data.table columns (see below)
# DEFINITION OF COHERENCE MEASURES
# WRAPPER FUNCTION TO CALCULATE COHERENCE

#DEFINITION OF COHERENCE MEASURES------------------------------------------------------------
  # #more convenient interface for defintion of coherence functions might be established
  # #e.g. by defining them as reference/S6 class, this would also allow more flexible definition of additional measures by user
  # coh_funs = list(
  #   #LOG-RATIO
  #   logratio_UMass = structure(function(wi, wj, n_tcm_windows, tcm, log_smooth_constant) {log(log_smooth_constant + tcm[wi,wj]) - log(tcm[wj,wj])}
  #                        ,comb_type = "one_pre_topic_order"
  #                        ,aggr_fun = "function(x) sum(x, na.rm = T)")
  #   #smoothing parameter = 1 resembles UMAss, .01 resembles stm package, default as in paper by Röder, i.e. .1e-12
  #   ,logratio = structure(function(wi, wj, n_tcm_windows, tcm, log_smooth_constant) {log(log_smooth_constant + tcm[wi,wj]) - log(tcm[wj,wj])}
  #                        ,comb_type = "one_pre"
  #                        ,aggr_fun = "function(x) sum(x, na.rm = T)")
  #   ,prob_logratio = structure(function(wi, wj, n_tcm_windows, tcm, log_smooth_constant)  {log(log_smooth_constant + (tcm[wi,wj]/n_tcm_windows)) - log(tcm[wj,wj]/n_tcm_windows)}
  #                              ,comb_type = "one_pre"
  #                              ,aggr_fun = "function(x) mean(x, na.rm = T)")
  #   #PMI
  #   #format of PMI formula proposed by @andland - https://github.com/dselivanov/text2vec/issues/236
  #   ,pmi = structure(function(wi, wj, n_tcm_windows, tcm, log_smooth_constant)  {log2((tcm[wi,wj]/n_tcm_windows) + log_smooth_constant) - log2(tcm[wi,wi]/n_tcm_windows) - log2(tcm[wj,wj]/n_tcm_windows)}
  #                    ,comb_type = "one_pre"
  #                    ,aggr_fun = "function(x) mean(x, na.rm = T)")
  #   #NORMALIZED PMI
  #   #again, in contrast, to other implementations, only intrinsic NPMI as in PMIM (for implementation with sliding window see, e.g., Bouma, 2009)
  #   ,npmi = structure(function(wi, wj, n_tcm_windows, tcm, log_smooth_constant) {(log2((tcm[wi,wj]/n_tcm_windows) + log_smooth_constant) - log2(tcm[wi,wi]/n_tcm_windows) - log2(tcm[wj,wj]/n_tcm_windows)) /  -log2((tcm[wi,wj]/n_tcm_windows) + log_smooth_constant)}
  #                     ,comb_type = "one_pre"
  #                     ,aggr_fun = "function(x) mean(x, na.rm = T)")
  #   #DIFFERENCE
  #   #assuming we use ordered tcm it follows that p(wi)>p(wj)
  #   #to set bounds of the measures [-1,1] (1 is good)  wi/wj are switched in formula
  #   #this is similar (not exactly the same) to the measure of textmineR package https://github.com/TommyJones/textmineR/issues/35
  #   ,prob_dif = structure(function(wi, wj, n_tcm_windows, tcm, log_smooth_constant) {tcm[wj,wi]/tcm[wj,wj] - (tcm[wj,wj]/n_tcm_windows)}
  #                         ,comb_type = "one_suc"
  #                         ,aggr_fun = "function(x) mean(x, na.rm = T)")
  # )

#WRAPPER FUNCTION TO CALCULATE COHERENCE-------------------------------------------------------
  #
  # #TODO coh_funs and other args need to be passed to the function as argument,
  # #later steps would be less verbose when fetching several or the arguments,e.g., tcm, from the function environment -> scoping
  # calc_coh = function( idxs
  #                      , tcm
  #                      , coh_funs = coh_funs
  #                      , n_tcm_windows
  #                      , coh_measure
  #                      , log_smooth_constant = log_smooth_constant
  #                      , alternative_order
  #                        ) {
  #   #select coherence function from the ones availble
  #   coh_fun = coh_funs[[coh_measure]]
  #   if (length(idxs) < 2) {
  #     return(NA)
  #   } else {
  #   #define the wi wj set
  #   coh = as.data.table(word_index_combinations(idxs, comb_type = attr(coh_fun, "comb_type"), alternative_order = alternative_order))
  #   #calculate score for each pair of wi wj
  #   coh[, coh_res:= mapply(function(x,y) coh_fun(x,y, tcm = tcm, n_tcm_windows = n_tcm_windows, log_smooth_constant = log_smooth_constant),wi, wj)]
  #   #aggregate
  #   aggr_fun = eval(parse(text = attr(coh_fun, "aggr_fun")))
  #   return(coh[, round(aggr_fun(coh_res), d = 4)])
  #   }
  # }

#CALCULATE COHERENCE----------------------------------------------------------------

  # potential starting point for definition of flexible implementation of coherence measures
  # for (i in names(coh_funs)) {
  #   coherence[,(i):= lapply(tcm_term_idxs, function(x) {
  #                                    calc_coh( idxs = x, coh_measure = i
  #                                   , tcm = tcm
  #                                   , coh_funs = coh_funs
  #                                   , n_tcm_windows = n_tcm_windows
  #                                   , log_smooth_constant = log_smooth_constant
  #                                   , alternative_order = restore_topic_order)})
  #                   , by = Topic]
  # }


  #Type 1
  #sum(..., na.rm = T) for aggregation
  #one_pre_topic_order as term indices
  #note the adapted log_smooth_constant to comply original UMass and its implementation in stm package
    coherence[,sum_logratio_UMass:= lapply(tcm_term_idxs, function(x) {
                        if (length(idxs) < 2) {
                          return(NA)
                        } else {
                          coh = as.data.table(term_index_combinations(idxs, comb_type = "one_pre_topic_order", topic_order = restore_topic_order))
                          coh[, score:= mapply(function(x,y) {log(1 + tcm[x,y]) - log(tcm[y,y])}
                                               ,V1, V2)]
                          return(coh[, round(sum(score, na.rm = T), d = 4)])
                        }
    })]

    coherence[,sum_logratio_stm_pckg:= lapply(tcm_term_idxs, function(x) {
                      if (length(idxs) < 2) {
                        return(NA)
                      } else {
                        coh = as.data.table(term_index_combinations(idxs, comb_type = "one_pre_topic_order", topic_order = restore_topic_order))
                        coh[, score:= mapply(function(x,y) {log(.01 + tcm[x,y]) - log(tcm[y,y])}
                                             ,V1, V2)]
                        return(coh[, round(sum(score, na.rm = T), d = 4)])
                      }
    })]

    #Type 2
    #mean(..., na.rm = T) for aggregation
    #one_pre_topic_order or one_pre as term indices
    coherence[,mean_prob_logratio:= lapply(tcm_term_idxs, function(x) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_pre"))
                      coh[, score:= mapply(function(x,y) {log(log_smooth_constant + tcm[x,y]) - log(tcm[y,y])}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
    })]

    coherence[,mean_prob_logratio_Torder:= lapply(tcm_term_idxs, function(x) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_pre_topic_order", topic_order = restore_topic_order))
                      coh[, score:= mapply(function(x,y) {log(log_smooth_constant + tcm[x,y]) - log(tcm[y,y])}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
      })]

    coherence[,mean_pmi:= lapply(tcm_term_idxs, function(x) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_pre"))
                      coh[, score:= mapply(function(x,y)  {log2((tcm[x,y]/n_tcm_windows) + log_smooth_constant) - log2(tcm[x,x]/n_tcm_windows) - log2(tcm[y,y]/n_tcm_windows)}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
      })]

    coherence[,mean_pmi_Torder:= lapply(tcm_term_idxs, function(x) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_pre_topic_order", topic_order = restore_topic_order))
                      coh[, score:= mapply(function(x,y)  {log2((tcm[x,y]/n_tcm_windows) + log_smooth_constant) - log2(tcm[x,x]/n_tcm_windows) - log2(tcm[y,y]/n_tcm_windows)}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
      })]

    coherence[,mean_npmi:= lapply(tcm_term_idxs, function(x) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_pre"))
                      coh[, score:= mapply(function(x,y)   {(log2((tcm[x,y]/n_tcm_windows) + log_smooth_constant) - log2(tcm[x,x]/n_tcm_windows) - log2(tcm[y,y]/n_tcm_windows)) /  -log2((tcm[x,y]/n_tcm_windows) + log_smooth_constant)}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
      })]

    #Type 3
    #mean(..., na.rm = T) for aggregation
    #one_suc as term indices
    coherence[,mean_prob_diff:= lapply(tcm_term_idxs, function(x) {
                    if (length(idxs) < 2) {
                      return(NA)
                    } else {
                      coh = as.data.table(term_index_combinations(idxs, comb_type = "one_suc"))
                      coh[, score:= mapply(function(x,y)  {tcm[y,x]/tcm[y,y] - (tcm[y,y]/n_tcm_windows)}
                                           ,V1, V2)]
                      return(coh[, round(mean(score, na.rm = T), d = 4)])
                    }
      })]

  coherence[, tcm_term_idxs := NULL]
  return(coherence[])
}
