context("coherence")

# generation of test data -------------------------
#library(text2vec)
#data("movie_review")
N = 500
tokens = word_tokenizer(tolower(movie_review$review[1:N]))
it = itoken(tokens, progressbar = FALSE)
v = create_vocabulary(it)
v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.2)
dtm = create_dtm(it, vocab_vectorizer(v))
n_topics = 100
n_top_terms = 10
lda_model = text2vec::LDA$new(n_topics = n_topics)
fitted = lda_model$fit_transform(dtm)
top_terms = lda_model$get_top_words(n = n_top_terms, topic_number = 1L:n_topics)
topic_word_distribution = lda_model$topic_word_distribution
# -------------------------------------------------

test_that("coherence, general functionality", {

# intrinsic reference tcm from corpus for testing general functionality
  tcm_intrinsic = Matrix::crossprod(sign(dtm))
  coherence_res = coherence(x = top_terms ,tcm = tcm_intrinsic, n_doc_tcm = nrow(dtm))
  expect_true(inherits(coherence_res, "matrix"))
  expect_equal(typeof(coherence_res), "double")
  expect_true(setequal(colnames(coherence_res),
                       c("mean_logratio", "mean_pmi", "mean_npmi", "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2")))
  expect_equal(nrow(coherence_res), n_topics)

# different smoothing constants
  coherence_res_adapted_smooth = coherence(x = top_terms ,tcm = tcm_intrinsic, n_doc_tcm = nrow(dtm), smooth = .01)
  expect_false(sum(as.vector(coherence_res)) == sum(as.vector(coherence_res_adapted_smooth)))

# incomplete input
  tcm_err = tcm_intrinsic[1:2,1:2]
  tcm_err[1,2] = 0
  tcm_err[2,1] = 0
  expect_warning({coherence_err = coherence(x = top_terms, tcm = tcm_err, n_doc_tcm = nrow(dtm))})
  expect_true(all(is.na(coherence_err)))

})

test_that("coherence, vectorized vs. mapply loop calculation of PMI", {

  #1. some simple toy data----------------------------
  tcm = matrix(rbind(c(40, 1, 2, 3),
                     c(1, 30, 4, 5),
                     c(2,  4,20, 6),
                     c(3,  5, 6,10)), ncol = 4)

  #pmi nonvectorized
  #to make it easy take all indices of tcm
  idxs = 1:ncol(tcm)
  #one index with preceeding indices combinations
  idxs_combis = t(combn(idxs,2, FUN = function(x) sort(x, decreasing = TRUE)))
  pmi = mapply(function(x,y) {log2((tcm[x,y]) + 1e-12) - log2(tcm[x,x]) - log2(tcm[y,y])} ,idxs_combis[,1], idxs_combis[,2])

  #pmi vectorized
  res = as.matrix(tcm[idxs, idxs])
  res[upper.tri(res)] = res[upper.tri(res)] + 1e-12
  d = diag(res)
  res = res/d
  res = res %*% diag(1 / d)
  res = res[upper.tri(res)]
  pmi_vect = log2(res)

  expect_equal(sort(pmi), sort(pmi_vect))

  #2. some real data and several arbitrary subsets-----
  tcm = Matrix::crossprod(dtm)

  for (i in 1:10) {
    set.seed(i)
    idxs =  sample(1:ncol(tcm), 4)

    #one index with preceeding indices combinations
    idxs_combis = t(combn(idxs,2, FUN = function(x) sort(x, decreasing = TRUE)))
    pmi = mapply(function(x,y) {log2((tcm[x,y]) + 1e-12) - log2(tcm[x,x]) - log2(tcm[y,y])} ,idxs_combis[,1], idxs_combis[,2])

    #pmi vectorized
    res = as.matrix(tcm[idxs, idxs])
    res[upper.tri(res)] = res[upper.tri(res)] + 1e-12
    d = diag(res)
    res = res/d
    res = res %*% diag(1 / d)
    res = res[upper.tri(res)]
    pmi_vect = log2(res)

    expect_equal(sort(pmi), sort(pmi_vect))
  }
})

test_that("coherence, results of text2vec vs other packages", {

  #textmineR: difference metric via function CalcProbCoherence: https://github.com/bstewart/stm/blob/master/R/semanticCoherence.R
  #stm: adapted UMass via the function semCoh1beta: https://github.com/bstewart/stm/blob/master/R/semanticCoherence.R

  # definition/copy of functions from other packages ------------------
  # apart from some  slight changes of the stm function to allow direct comparison
  # marked via "<<<<<<<<<<< NOTE FROM TEXT2VEC"
  # below functions are simply a copy from the other packages

  CalcProbCoherence <- function(phi, dtm, M = 5){

    # phi is a numeric matrix or numeric vector?
    if( ! is.numeric(phi) ){
      stop("phi must be a numeric matrix whose rows index topics and columns\n",
           " index terms or phi must be a numeric vector whose entries index terms.")
    }
    # is dtm a matrix we can work with?
    if( ! is.matrix(dtm) &&
        ! inherits(dtm, 'Matrix')){
      stop("dtm must be a matrix. This can be a standard R dense matrix or a\n",
           " matrix of class dgCMatrix, dgTMatrix, dgRMatrix, or dgeMatrix")
    }

    # is M numeric? If it is not an integer, give a warning.
    if( ! is.numeric(M) | M < 1){
      stop("M must be an integer in 1:ncol(phi) or 1:length(phi)")
    }

    if(length(M) != 1){
      warning("M is a vector when scalar is expected. Taking only the first value")
      M <- M[[1 ]]
    }

    if(floor(M) != M){
      warning("M is expected to be an integer. floor(M) is being used.")
      M <- floor(M)
    }

    # dtm has colnames?
    if( is.null(colnames(dtm))){
      stop("dtm must have colnames")
    }

    # Names of phi in colnames(dtm)
    if( ! is.matrix(phi) ){
      if(sum(names(phi)[ 1:M ] %in% colnames(dtm)) != length(1:M)){
        stop("names(phi)[ 1:M ] are not in colnames(dtm)")
      }
    }else if(sum(colnames(phi)[ 1:M ] %in% colnames(dtm)) != length(1:M)){
      stop("colnames(phi)[ 1:M ] are not in colnames(dtm)")
    }

    # Declare a function to get probabilistic coherence on one topic
    pcoh <- function(topic, dtm, M){
      terms <- names(topic)[order(topic, decreasing = TRUE)][1:M]
      dtm.t <- dtm[, terms]
      dtm.t[dtm.t > 0] <- 1
      count.mat <- Matrix::t(dtm.t) %*% dtm.t
      num.docs <- nrow(dtm)
      p.mat <- count.mat/num.docs
      # result <- sapply(1:(ncol(count.mat) - 1), function(x) {
      #   mean(p.mat[x, (x + 1):ncol(p.mat)]/p.mat[x, x] - Matrix::diag(p.mat)[(x +
      #                                                                           1):ncol(p.mat)], na.rm = TRUE)
      # })
      # mean(result, na.rm = TRUE)
      result <- sapply(1:(ncol(count.mat) - 1), function(x) {
        p.mat[x, (x + 1):ncol(p.mat)]/p.mat[x, x] -
          Matrix::diag(p.mat)[(x + 1):ncol(p.mat)]
      })
      mean(unlist(result), na.rm = TRUE)
    }

    # if phi is a single topic vector get that one coherence
    if( ! is.matrix(phi) ){
      return(pcoh(topic = phi, dtm = dtm, M = M))
    }

    # Otherwise, do it for all the topics
    apply(phi, 1, function(x){
      pcoh(topic = x, dtm = dtm, M = M)
    })
  }

  semCoh1beta_adapted <- function(mat, M, beta) {
    #Get the Top N Words
    top.words <- apply(beta, 1, order, decreasing=TRUE)[1:M,]
    wordlist <- unique(as.vector(top.words))
    mat <- mat[,wordlist]
    mat = sign(mat)

    #do the cross product to get co-occurences
    cross <- tcrossprod(t(mat))

    #create a list object with the renumbered words (so now it corresponds to the rows in the table)
    temp <- match(as.vector(top.words),wordlist)
    labels <- split(temp, rep(1:nrow(beta), each=M))

    #Note this could be done with recursion in an elegant way, but let's just be simpler about it.
    sem <- function(ml,cross) {
      m <- ml[1]; l <- ml[2]
      #The following commented line is the original line from stm <<<<<<<<<<<<<<<<<<<<<<<<< NOTE FROM TEXT2VEC
      #log(.01 + cross[m,l]) - log(cross[l,l] + .01)
      #it was adapted allow direct comparison with text2vec implementation by
      #(i) removing smoothing from diagonal values
      #this is not done in text2vec since any(diag(res) == 0) is FALSE or in other words)
      #(ii) using smaller smoohting value to
      log(1e-12 + cross[m,l]) - log(cross[l,l])
    }
    result <- vector(length=nrow(beta))
    for(k in 1:nrow(beta)) {
      grid <- expand.grid(labels[[k]],labels[[k]])
      colnames(grid) <- c("m", "l") #corresponds to original paper
      grid <- grid[grid$m > grid$l,]
      calc <- apply(grid,1,sem,cross)
      #The following commented line is the original line from stm <<<<<<<<<<<<<<<<<<<<<<<<< NOTE NOTE FROM TEXT2VEC
      result[k] <- sum(calc)
      #it was adapted allow direct comparison with text2vec implementation by
      #(i) using the mean insted of the sum to
      result[k] <- mean(calc, na.rm = TRUE)
    }
    return(result)
  }

# intrinsic reference tcm from corpus as used in the other packages
tcm_intrinsic = Matrix::crossprod(sign(dtm))

coherence_text2vec = coherence(x = top_terms ,tcm = tcm_intrinsic, n_doc_tcm = nrow(dtm)
                               #select the corresponding metrics for testing against other packages
                               ,metrics = c("mean_difference", "mean_logratio"))

#calculate coherence scores with other packages
logratio_stm_adapted = semCoh1beta_adapted(mat = dtm, M = n_top_terms, beta = topic_word_distribution)
mean_difference_textmineR = CalcProbCoherence(phi = topic_word_distribution, dtm = as.matrix(dtm), M =  n_top_terms)

#compare results
compare = cbind(coherence_text2vec, mean_difference_textmineR, logratio_stm_adapted)

expect_equal(sort(compare[,"mean_logratio"]), sort(compare[,"logratio_stm_adapted"]))
expect_equal(sort(compare[,"mean_difference"]), sort(compare[,"mean_difference_textmineR"]))
})
