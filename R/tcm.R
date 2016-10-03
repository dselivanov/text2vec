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

#' @name get_tcm
#' @title Extract term-co-occurence matrix
#' @description This function creates a term-co-occurence matrix from a
#'   \code{Corpus} object.
#' @param corpus \code{HashCorpus} or \code{VocabCorpus} object. See
#'   \link{create_corpus}, \link{vectorizers} for details.
#' @seealso \link{create_corpus}
#' @examples
#' \dontrun{
#' txt = movie_review[['review']][1:1000]
#' it = itoken(txt, tolower, word_tokenizer)
#' vocab = create_vocabulary(it)
#' #remove very common and uncommon words
#' pruned_vocab = prune_vocabulary(vocab, term_count_min = 10, doc_proportion_max = 0.8,
#'                                 doc_proportion_min = 0.001, max_number_of_terms = 5000)
#'
#' vectorizer = vocab_vectorizer(pruned_vocab, grow_dtm = FALSE, skip_grams_window = 5L)
#' it = itoken(txt, tolower, word_tokenizer)
#' corpus = create_corpus(it, vectorizer)
#' tcm = get_tcm(corpus)
#' dim(tcm)
#' }
#' @export
get_tcm = function(corpus) {
  if (inherits(corpus, 'Rcpp_VocabCorpus') || inherits(corpus, 'Rcpp_HashCorpus')) {
    tcm = corpus$get_tcm()
    if (length(tcm@x) == 0)
      warning("Something goes wrong, tcm has 0 rows...")
    dim_names = colnames(tcm)
    tcm@Dimnames = list(dim_names, dim_names)
    tcm
  }
  else
    stop("corpus should be Rcpp_HashCorpus class or Rcpp_VocabCorpus class")
}

#' @name create_tcm
#' @title Term-co-occurence matrix construction
#' @description This is a function for constructing a
#' term-co-occurrence matrix(TCM). TCM matrix usually used with \link{GloVe} word embedding model.
#' @details If a parallel backend is registered, it will onstruct the TCM in multiple threads.
#' The user should keep in mind that he/she should split data and provide a list
#' of \link{itoken} iterators. Each element of \code{it} will be handled
#' in a separate thread combined at the end of processing.
#' @param it \code{list} of iterators over tokens from \link{itoken}.
#'   Each element is a list of tokens, that is, tokenized and normalized
#'   strings.
#' @param vectorizer \code{function} vectorizer function. See
#'   \link{vectorizers}.
#' @param ... arguments to \link{foreach} function which is used to iterate over
#'   \code{it}.
#' @return \code{dgTMatrix} TCM matrix
#' @seealso \link{itoken} \link{create_dtm}
#' @examples
#' \dontrun{
#' data("movie_review")
#'
#' # single thread
#'
#' tokens = movie_review$review %>% tolower %>% word_tokenizer
#' it = itoken(tokens)
#' v = create_vocabulary(jobs)
#' vectorizer = vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)
#' tcm = create_tcm(itoken(tokens), vectorizer)
#'
#' # parallel version
#'
#' # set to number of cores on your machine
#' N_WORKERS = 1
#' splits = split_into(movie_review$review, N_WORKERS)
#' jobs = lapply(splits, itoken, tolower, word_tokenizer)
#' v = create_vocabulary(jobs)
#' vectorizer = vocab_vectorizer(v, grow_dtm = FALSE, skip_grams_window = 3L)
#' jobs = lapply(splits, itoken, tolower, word_tokenizer)
#' doParallel::registerDoParallel(N_WORKERS)
#' tcm = create_tcm(jobs, vectorizer)
#' }
#' @export
create_tcm = function(it, vectorizer, ...) {
  if(attr(vectorizer, "skip_grams_window", TRUE) == 0)
    stop("You should provide vectorizer with skip_grams_window > 0")
  UseMethod("create_tcm")
}

#' @rdname create_tcm
#' @export
create_tcm.itoken = function(it, vectorizer, ...) {
  corp = vectorizer(it)
  # get it in triplet form - fastest and most
  # memory efficient way because internally it
  # kept in triplet form
  tcm = get_tcm(corp)
  # remove corpus and trigger gc()!
  # this will release a lot of memory
  rm(corp); gc();
  tcm
}

#' @rdname create_tcm
#' @param verbose \code{logical} print status messages
#' @param work_dir working directory for intermediate results
#' @export
create_tcm.list = function(it, vectorizer, verbose = FALSE, work_dir = tempdir(), ...) {
  jobs = Map(function(job_id, it) list(job_id = job_id, it = it), seq_along(it), it)
  tcm_files =
    foreach(batch = jobs,
            .combine = c,
            #.combine = function(...) triplet_sum(..., verbose = verbose),
            .inorder = F,
            .multicombine = T,
            # user already made split for jobs
            # preschedule = FALSE is much more memory efficient
            .options.multicore = list(preschedule = FALSE),
            ...) %dopar%
            {
              tcm = create_tcm(batch$it, vectorizer, ...)
              file_to_save = tempfile(pattern = paste0("tcm_map_part_", batch$job_id, "_"), tmpdir = work_dir, fileext = '.rds')
              saveRDS(tcm, file_to_save, compress = F)
              file_to_save
            }
  if (verbose)
    message(paste(Sys.time(), "map phase finished, starting reduce"))
  res_file = mc_triplet_rds_sum(tcm_files, work_dir, verbose = verbose)
  res = readRDS(res_file); unlink(res_file)
  as(res, 'dgTMatrix')
}

# triplet_sum = function(..., verbose = FALSE) {
#   if (verbose)
#     print(paste(Sys.time(), "got results from workers, call combine ..."))
#
#   lst = list(...)
#
#   if (any(vapply(lst, is.null, FALSE)))
#     stop('Got NULL from one of the jobs.
#           Probably result size >= 2gb and package "parallel" can\'t collect results.
#           Try to split input into more chunks (so result on each chunk must be < 2gb)')
#
#   res = uniqTsparse(Reduce(`+`, lst))
#   # assume all matrices have same dimnames
#   res@Dimnames = lst[[1]]@Dimnames
#   res
# }

# multicore combine
mc_reduce = function(X, FUN,  ...) {
  if (length(X) >= 2) {
    # split into pairs of elements
    pairs = split(X, ceiling(seq_along(X) / 2))

    X_NEW =
      foreach( pair = pairs, ...) %dopar% {
        # if length(X) is odd, we will recieve several pairs + single element
        if (length(pair) == 1)
          pair[[1]]
        else {
          FUN(pair[[1]], pair[[2]])
        }
      }
    # recursive call
    mc_reduce(X_NEW, FUN = FUN, ...)
  }
  # finally reduce to single element
  else
    X[[1]]
}

mc_triplet_rds_sum = function(fls, work_dir, verbose) {
  sum_m = function(a, b) {
    m1 = readRDS(a); unlink(a)
    if (!inherits(m1, 'dgCMatrix')) {
      m1 = as(m1, 'dgCMatrix')
      gc()
    }
    m2 = readRDS(b); unlink(b);
    if (!inherits(m2, 'dgCMatrix')) {
      m2 = as(m2, 'dgCMatrix')
      gc()
    }
    res = m1 + m2
    rm(m1, m2);gc();
    file_to_save = tempfile(pattern = "reduce_", tmpdir = work_dir, fileext = '.rds')
    saveRDS(res, file_to_save, compress = F)
    file_to_save
  }
  mc_reduce(fls,
            FUN = sum_m,
            .inorder = FALSE,
            .multicombine = TRUE)
}

# multicore version of triplet_sum
# mc_triplet_sum = function(...) {
#   mc_reduce(list(...),
#             FUN = function(a, b) uniqTsparse(a + b),
#             .inorder = FALSE,
#             .multicombine = TRUE)
# }

