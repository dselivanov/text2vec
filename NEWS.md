# text2vec 0.2.1 - development version

1. 2016-01-13 fix for #46, thanks to @[buhrmann](https://github.com/buhrmann) for reporting 
2. 2016-01-16 format of vocabulary changed.
    * do not keep `doc_proportions`. see #52.
    * add `stop_words` argument to `prune_vocabulary`. signature also was changed.

# text2vec 0.2.0 (2016-01-10)

First CRAN release of text2vec.

* Fast text vectorization with stable streaming API on arbitrary n-grams.
    * Functions for vocabulary extraction and management
    * Hash vectorizer (based on digest murmurhash3)
    * Vocabulary vectorizer
* GloVe algorithm word embeddings.
    * Fast term-cooccurence matrix factorization via parallel async AdaGrad.
* All core functions written in C++.
