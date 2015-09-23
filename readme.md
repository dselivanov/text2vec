# Quick reference
**[Quick reference](https://github.com/dselivanov/tmlite/wiki/Quick-reference)** is based on kaggle's [Bag of Words Meets Bags of Popcorn](https://www.kaggle.com/c/word2vec-nlp-tutorial) competition data - [labeledTrainData.tsv.zip](https://www.kaggle.com/c/word2vec-nlp-tutorial/download/labeledTrainData.tsv.zip).

# Key features
**Note that package is in very alpha version. This doesn't mean the package is not robust, but this means that API can change at any time.**

1. Efficient memory-friendly **streaming corpus construction**. **tmlite**'s provides API for construction corporas from `character` vectors and more important - [connections](https://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html). So it is possible (and easy!) to construct Document-Term matrices for collections of documents thar are don't fit in the memory.
2. **Fast** - core functions are written in C++, thanks to [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html) authors.
3. Efficient **n-gram generation and processing**.
4. Flexible and **easy functional-style API**. Easy chaining.
5. Has two main corpus classes - 
    - `DictCorpus` - traditional dictionary-based container used for Document-Term matrix construction.
    - `HashCorpus` - container that implements [feature hashing](https://en.wikipedia.org/wiki/Feature_hashing) or **"hashing trick"**. Similar to [scikit-learn FeatureHasher](http://scikit-learn.org/stable/modules/feature_extraction.html#feature-hashing) and  [gensim corpora.hashdictionary](https://radimrehurek.com/gensim/corpora/hashdictionary.html).
    
        > The class HashCorpus is a high-speed, low-memory vectorizer that uses a technique known as feature hashing, or the “hashing trick”. Instead of building a hash table of the features encountered in training, as the vectorizers do, instances of HashCorpus apply a hash function to the features to determine their column index in sample matrices directly. 
    
6. **Document-Term matrix is key object**. At the moment it can be extracted from corpus into `dgCMatrix`, `dgTMatrix` or [LDA-C](https://www.cs.princeton.edu/~blei/lda-c/readme.txt) which is standart for [lda](https://cran.r-project.org/web/packages/lda/index.html) package. `dgCMatrix` is default for sparse matrices in R and most of the packages that work with sparse matrices work with `dgCMatrix` matrices, so it will be **easy to interact with other packages**.


# Benchmarks
- [Comparison with tm and reasons why I started tmlite](https://github.com/dselivanov/tmlite/wiki/Comparison-with-tm)
- [tmlite vs scikit-learn vectorizers](https://github.com/dselivanov/tmlite/wiki/Comparison-with-scikit-learn-vectorizers)

# Focus

As unix philosophy says - [Do One Thing and Do It Well](https://en.wikipedia.org/wiki/Unix_philosophy#Do_One_Thing_and_Do_It_Well), so we will focus on one particular problem - infrastructure for text analysis. R ecosystem contains lots of packages that are well suited for working with sparse high-dimensional data (and thus suitable for text modeling). Here are my favourites:

- [lda](https://cran.r-project.org/web/packages/lda/index.html) blazing fast package for topic modeling.
- [glmnet](https://cran.r-project.org/web/packages/glmnet/index.html) for L1, L2 linear models.
- [xgboost](https://cran.r-project.org/web/packages/xgboost/) for gradient boosting. 
- [LiblineaR](https://cran.r-project.org/web/packages/LiblineaR/index.html) - wrapper of `liblinear` svm library.
- [irlba](https://cran.r-project.org/web/packages/irlba/index.html) - A fast and memory-efficient method for computing a few approximate singular values and singular vectors of large matrices.

These are all excellent and very efficient packages, so **tmlite** will be focused (at least in the nearest future) not on modeling, but on framework - Document-Matrix construction and manipulation - basis for any text-mining analysis. **tmlite** is partially inspired by [gensim](https://radimrehurek.com/gensim/) - robust and well designed python library for text mining. In the near future we will try to replicate some of its functionality.

# Future work

### Contributors are very welcome
Project has [issue tracker on github](https://github.com/dselivanov/tmlite/issues) where I'm filing feature requests and notes for future work. Any ideas are very appreciated.

If you like it, you can **help**:

- Test and leave feedback on [github issuer tracker](https://github.com/dselivanov/tmlite/issues) (preferably) or directly by email.
    - package is tested on linux and OS X platforms, so Windows users are especially welcome
- Fork and start contributing. Vignettes, docs, tests, use cases are very welcome.
- Or just give me a star on [project page](https://github.com/dselivanov/tmlite) :-)

### Short-term plans
- add tests
- ~~add n-gram tokenizers~~ [already done](https://github.com/dselivanov/tmlite/issues/6)
- ~~add methods for tokenization in C++ (at the moment tokenization takes almost half of runtime)~~ [see these benchmarks](https://github.com/dselivanov/tmlite/issues/2). ~~It will make sense to switch to `stringi` or `stringr`~~ - [done](https://github.com/dselivanov/tmlite/issues/18)
- ~~switch to murmur3 hash and add second hash function to reduce probability of collision~~ [done](https://github.com/dselivanov/tmlite/issues/8), thanks to @[wush978](https://github.com/wush978/FeatureHashing/issues/96)
- push dictionary and stopwords filtering into C++ code

### Middle-term plans
- add **[word2vec](https://code.google.com/p/word2vec/) wrapper**. It is strange, that R community still didn't have it.
- add corpus serialization

### Long-term plans
- integrate models like it is done in [gensim](https://radimrehurek.com/gensim/)
- try to implement out-of-core transformations like [gensim](https://radimrehurek.com/gensim/) does
