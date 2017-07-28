# text2vec 0.5.0

1. 2017-06-12
    * Add additional filters to `prune_vocabulary` - filter by document counts
    * Clean up LSA, fixed transform method. Added option to use randomized SVD algorithm from `irlba`.
1. 2017-05-17
    * Imrove `dist2` performamce for RWMD - incorporate ideas from [gensim PR discussion](https://github.com/RaRe-Technologies/gensim/pull/800#issuecomment-260743822).
1. 2017-05-17
    * **API breaking change** - vocabulary format change - now plain `data.frame` with meta-information in attributes (stopwords, ngram, number of docs, etc).
1. 2017-03-25
    * No more rely on RcppModules
    * **API breaking change** - removed `lda_c` from formats in DTM construction
    * added `ifiles_parallel`, `itoken_parallel` high-level functions for parallel computing
    * **API breaking change**  `chunks_numer` parameter renamed to `n_chunks`
1. 2017-01-02 
    * **API breaking change** - removed `create_corpus` from public API, moved co-occurence related optons to `create_tcm` from vecorizers
    * add ability to add custom weights for co-occurence statistics calculations
1. 2016-12-30 
    * Noticeable **speedup** (**1.5x**) and even more noticeable improvement on memory usage (**2x** less!) for `create_dtm`, `create_tcm` . Now package relies on [sparsepp](https://github.com/dselivanov/sparsepp) library for underlying hash maps.
1. 2016-10-30 
    * Collocations -  detection of multi-word phrases using differend heuristics - *PMI, gensim, LFMD*.
1. 2016-10-20 
    * Fixed bug in `as.lda_c()` function

# text2vec 0.4.0

2016-10-03. See [0.4 milestone tags](https://github.com/dselivanov/text2vec/milestone/3).

1. Now under GPL (>= 2) Licence
1. "immutable" iterators - no need to reinitialize them
1. unified models interface
1. New models: LSA, LDA, GloVe with L1 regularization
1. Fast similarity and distances calculation: Cosine, Jaccard, Relaxed Word Mover's Distance, Euclidean
1. Better hadnling UTF-8 strings, thanks to @qinwf
1. iterators and models rely on `R6` package

# text2vec 0.3.0

1. 2016-01-13 fix for #46, thanks to @[buhrmann](https://github.com/buhrmann) for reporting 
1. 2016-01-16 format of vocabulary changed.
    * do not keep `doc_proportions`. see #52.
    * add `stop_words` argument to `prune_vocabulary`. signature also was changed.
1. 2016-01-17 fix for #51. if iterator over tokens returns list with names, these names will be:
    * stored as `attr(corpus, 'ids')`
    * rownames in dtm
    * names for dtm list in `lda_c` format
1. 2016-02-02 high level function for corpus and vocabulary construction.
    * construction of vocabulary from list of `itoken`. 
    * construction of dtm from list of `itoken`. 
1. 2016-02-10 rename transformers
    * now all transformers starts with `transform_*` - more intuitive + simpler usage with autocompletion
1. 2016-03-29 (accumulated since 2016-02-10)
    * rename `vocabulary` to `create_vocabulary`.
    * new functions `create_dtm`, `create_tcm`.
    * All core functions are able to benefit from multicore machines (user have to register parallel backend themselves)
    * Fix for progress bars. Now they are able to reach 100% and ticks increased after computation.
    * `ids` argument to `itoken`. Simplifies assignement of ids to rows of DTM
    * `create_vocabulary` now can handle `stopwords` 
    * see all updates [here](https://github.com/dselivanov/text2vec/milestones/0.3)
1. 2016-03-30 more robust `split_into()` util.

# text2vec 0.2.0 (2016-01-10)

First CRAN release of text2vec.

* Fast text vectorization with stable streaming API on arbitrary n-grams.
    * Functions for vocabulary extraction and management
    * Hash vectorizer (based on digest murmurhash3)
    * Vocabulary vectorizer
* GloVe algorithm word embeddings.
    * Fast term-co-occurence matrix factorization via parallel async AdaGrad.
* All core functions written in C++.
