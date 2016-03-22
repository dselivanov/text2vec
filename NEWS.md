# text2vec 0.3.0 - development version

1. 2016-01-13 fix for #46, thanks to @[buhrmann](https://github.com/buhrmann) for reporting 
1. 2016-01-16 format of vocabulary changed.
    * do not keep `doc_proportions`. see #52.
    * add `stop_words` argument to `prune_vocabulary`. signature also was changed.
1. 2016-01-17 fix for #51. if iterator over tokens returns list with names, these names will be:
    * stored as `attr(corpus, 'ids')`
    * rownames in dtm
    * names for dtm list in `lda_c` format
1. 2016-02-02 high level function for corpus and vocabulary construction. In parallel if any parallel backend was registred.
    * construction of vocabulary from list of `itoken`. 
    * construction of dtm from list of `itoken`. 
1. 2016-02-10 rename transformers
    * now all transformers starts with `transformer_*` - more intuitive + simpler usage with autocompletion
1. 

# text2vec 0.2.0 (2016-01-10)

First CRAN release of text2vec.

* Fast text vectorization with stable streaming API on arbitrary n-grams.
    * Functions for vocabulary extraction and management
    * Hash vectorizer (based on digest murmurhash3)
    * Vocabulary vectorizer
* GloVe algorithm word embeddings.
    * Fast term-cooccurence matrix factorization via parallel async AdaGrad.
* All core functions written in C++.
