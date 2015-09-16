## Introducing `tmlite` package
Reasonable question is - why new package? R already has such great package as [tm](https://cran.r-project.org/web/packages/tm/) and companion packages [tau](https://cran.r-project.org/web/packages/tau/) and [NLP](https://cran.r-project.org/web/packages/NLP/)?

I'll try to answer these questions in the last part of this article.

## Focus

As unix philosophy says - [Do One Thing and Do It Well](https://en.wikipedia.org/wiki/Unix_philosophy#Do_One_Thing_and_Do_It_Well), so we will focus on one particular problem - infrastructure for text analysis. R ecosystem contains lots of packages that are well suited for working with sparse high-dimensional data (and thus suitable for text modeling). Here are my favourites:

- [lda](https://cran.r-project.org/web/packages/lda/index.html) blazing fast package for topic modeling.
- [glmnet](https://cran.r-project.org/web/packages/glmnet/index.html) for L1, L2 linear models.
- [xgboost](https://cran.r-project.org/web/packages/xgboost/) for gradient boosting. 
- [LiblineaR](https://cran.r-project.org/web/packages/LiblineaR/index.html) - wrapper of `liblinear` svm library.
- [irlba](https://cran.r-project.org/web/packages/irlba/index.html) - A fast and memory-efficient method for computing a few approximate singular values and singular vectors of large matrices.

These are all excellent and very efficient packages, so **tmlite** will be focused (at least in the nearest future) not on modeling, but on framework - Document-Matrix construction and manipulation - basis for any text-mining analysis. **tmlite** is partially inspired by [gensim](https://radimrehurek.com/gensim/) - robust and well designed python library for text mining. In the near future we will try to replicate some of its functionality.

## Key features
**Note that package is in very alpha version. This doesn't mean the package is not robust, but this means that API can change at any time.**

1. Flexible and easy functional-style API. Easy chaining.
2. Efficient memory-friendly **streaming corpus construction**. **tmlite**'s provides API for construction corporas from `character` vectors and more important - [connections](https://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html). So it is possible (and easy!) to construct Document-Term matrices for collections of documents thar are don't fit in the memory.
3. Fast - core functions are written in C++, thanks to [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html) authors.
4. Has two main corpus classes - 
    - `DictCorpus` - traditional dictionary-based container used for Document-Term matrix construction.
    - `HashCorpus` - container that implements [feature hashing](https://en.wikipedia.org/wiki/Feature_hashing) or **"hashing trick"**. Similar to [scikit-learn FeatureHasher](http://scikit-learn.org/stable/modules/feature_extraction.html#feature-hashing) and  [gensim corpora.hashdictionary](https://radimrehurek.com/gensim/corpora/hashdictionary.html).  
    
        > The class HashCorpus is a high-speed, low-memory vectorizer that uses a technique known as feature hashing, or the “hashing trick”. Instead of building a hash table of the features encountered in training, as the vectorizers do, instances of HashCorpus apply a hash function to the features to determine their column index in sample matrices directly. 
    
5. Document-Term matrix is key object. At the moment it can be extracted from corpus into `dgCMatrix`, `dgTMatrix` or [LDA-C](https://www.cs.princeton.edu/~blei/lda-c/readme.txt) which is standart for [lda](https://cran.r-project.org/web/packages/lda/index.html) package. `dgCMatrix` is default for sparse matrices in R and most of the packages that work with sparse matrices work with `dgCMatrix` matrices, so it will be easy to interact with them.

## Quick reference
Firыt quick example is based on kaggle's [Bag of Words Meets Bags of Popcorn](https://www.kaggle.com/c/word2vec-nlp-tutorial) competition data - [labeledTrainData.tsv.zip](https://www.kaggle.com/c/word2vec-nlp-tutorial/download/labeledTrainData.tsv.zip).  

Here I'll demostrate flexibility of the corpus creation procedure and how to vectorize large collection of documents. 

Suppose text file is very large, but it contains 3 tab-separated columns. Only one is relevant (third column in example below). Now we want to create corpus, but can't read whole file into memory. See how this will be resolved. 
First load libraries:

```r
library(tmlite)
```



```
## Loading required package: Matrix
```



```
## Warning in .doLoadActions(where, attach): trying to execute load actions
## without 'methods' package
```



```r
# for pipe syntax
library(magrittr)
```
File contains 3 columns - `id`, `sentiment`, `review`. Only `review` is relevant.

Simple preprocessing function will do the trick for us - we will only read third column - text of the review.

```r
# function receives character vector - batch of rows.
preprocess_fun <- function(x) {
  # file is tab-sepatated - split each row by \t
  rows <- strsplit(x, '\t', fixed = T)
  # text review is in the third column
  txt <- sapply(rows, function(x) x[[3]])
  # tolower, keep only letters
  simple_preprocess(txt) 
}
```
Read documents and create **dictinary-based** corpus:

```r
# we don't want read all file into RAM - we will read it iteratively, row by row
path <- '~/Downloads/labeledTrainData.tsv'
con <- file(path, open = 'r', blocking = F)
corp <- create_dict_corpus(src = con, 
                   preprocess_fun = preprocess_fun, 
                   # simple_tokenizer - split string by whitespace
                   tokenizer = simple_tokenizer, 
                   # read by batch of 1000 documents
                   batch_size = 1000,
                   # skip first row - header
                   skip = 1, 
                   # do not show progress bar because of knitr
                   progress = F
                  )
```
Now we want to try predict sentiment, based on review. For that we will use **glmnet** package, so we have to create Document-Term matrix in `dgCMatrix` format. It is easy with `get_dtm` function:

```r
dtm <- get_dtm(corpus = corp, type = "dgCMatrix") %>% 
  # remove very common and very uncommon words
  dtm_transform(filter_commons_transformer, term_freq = c(common = 0.001, uncommon = 0.975)) %>% 
  # make tf-idf transformation
  dtm_transform(tfidf_transformer)
dim(dtm)
```



```
## [1] 25000 10067
```
Cool. We have feature matrix, but don't have response variable, which is still in the large file (which possibly won't fit into memory). Fortunately reading particular columns is easy, for example see this [stackoverflow discussion](http://stackoverflow.com/questions/2193742/ways-to-read-only-select-columns-from-a-file-into-r-a-happy-medium-between-re). We will use `fread()` function from **data.table** package:

```r
library(data.table)
# read only second column - value of sentiment
dt <- fread(path, select = c(2))
```
So all stuff is ready for model fitting.

```r
library(glmnet)
```



```
## Loading required package: foreach
## Loaded glmnet 2.0-2
```



```r
# I have 4 core machine, so will use parallel backend for n-fold crossvalidation
library(doParallel)
```



```
## Loading required package: iterators
## Loading required package: parallel
```



```r
registerDoParallel(4)
# train logistic regression with 4-fold cross-validation, maximizing AUC
fit <- cv.glmnet(x = dtm, y = dt[['sentiment']], 
                 family = "binomial", type.measure = "auc", 
                 nfolds = 4, parallel = T)
plot(fit)
```

![dict_corpus_auc](http://dsnotes.com/images/2015-09-16-tmlite-intro/dict_dtm_fit-1.png) 

```r
print (paste("max AUC = ", round(max(fit$cvm), 4)))
```



```
## [1] "max AUC =  0.9485"
```
Not bad!
Now lets try to construct **dtm** using `HashCorpus` class. Our data is tiny, but for larger data or streaming environments, `HashCorpus`  is natural choice.

```r
con <- file(path, open = 'r', blocking = F)
hash_corp <- create_hash_corpus(src = con, 
                           preprocess_fun = preprocess_fun, 
                           # simple_tokenizer - split string by whitespace
                           tokenizer = simple_tokenizer, 
                           # read by batch of 1000 documents
                           batch_size = 1000,
                           # skip first row - header
                           skip = 1,
                           # don't show progress bar because of knitr
                           progress = F)
hash_dtm <- get_dtm(corpus = hash_corp, type = "dgCMatrix") %>% 
  dtm_transform(filter_commons_transformer, term_freq = c(common = 0.001, uncommon = 0.975)) %>% 
  dtm_transform(tfidf_transformer)
# note, that ncol(hash_dtm) > ncol(dtm). Effect of collisions - we can fix this by increasing `hash_size` parameter .
dim(hash_dtm)
```



```
## [1] 25000 10107
```



```r
registerDoParallel(4)
hash_fit <- cv.glmnet(x = hash_dtm, y = dt[['sentiment']], 
                      family = "binomial", type.measure = "auc", 
                      nfolds = 4, parallel = T)
plot(hash_fit)
```

![hash_corpus_auc](http://dsnotes.com/images/2015-09-16-tmlite-intro/hash_dtm_fit-1.png)

```r
# near the same result
print (paste("max AUC = ", round(max(hash_fit$cvm), 4)))
```



```
## [1] "max AUC =  0.9482"
```


## Reasons why I started develop tmlite

**All conslusions below are based on personal experience so they can be heavily biased.**

First time I started to use  **tm** was end of 2014. I tried to process collection of text dosuments which was less then 1 Gb. About 10000 texts. Surprisingly I wasn't able to process them on machine with 16 Gb of RAM! But what is really cool - R and all the packages are open source. So I started to examine source code. Unfortunatelly I ended by rewriting most of the package. That first version (anyone interested can browse commits history on github) was quite robust and can handle such tiny-to-medium collections of documents. After that I tried it on some kaggle competitions, but didn't do any new development, since my work wasn't related to text analysis and I had no time for that. Also I noted, that almost all text-mining packages in R has **tm** dependency. We will try to develop an alternative.

About month ago I started full redesign (based on previous experience) and now I rewrote core functions in C++ and want bring alpha version to community.

So why you should not use **tm**:

1. **tm** has a lot of functions - in fact reference manual contains more than 50 pages. But its **API is very messy**. A lot of packages depends on it , so it is hard redesign it.
2. **tm** is not very efficient (from my experience). I found it very slow and what is more important - **very RAM unfriendly and RAM-greedy**. (I'll provide few examples below). As I understand it is designed more for academia researchers, then data science practitioners. It perfectly handles metadata, processes different encodings. API is very high-level, but the price for that is performance.
3. Can only handle documents that fit in RAM. (To be fair I should say, that there is `PCorpus()` function. But it seems it cannot help with Document-Term matrix construction when size of the documents larger than RAM - see examples below. `DocumentTermMatrix()` is **very** RAM-greedy).

In contrast [tmlite](https://github.com/dselivanov/tmlite) is **designed for practitioners** (and kagglers!) who:
 - understand what they want and how to do that. So we will not expose trivial high-level API like `findAssocs`, `findFreqTerms`, etc.
 - work with medium to large collections of documents
 - have at least medium level of experience in R and know basic concepts of functional programming

## Comparison with tm

### Some naive benchmarks on Document-Trem matrix construction

Here I'll provide simple benchmark, which can give some impression about **tmlite** speed, compared to **tm**. For now we assume, that documents are already in memory, so we only need to clean text and tokenize it:

```r
library(tm)
```



```
## Loading required package: NLP
```



```r
library(data.table)
library(tmlite)
dt <- fread('~/Downloads/labeledTrainData.tsv')
txt <- dt[['review']]
print(object.size(txt), quote = FALSE, units = "Mb")
```



```
## 32.8 Mb
```



```r
# 32.8 Mb
system.time ( corpus_tm <- VCorpus(VectorSource(txt)) )
```



```
##    user  system elapsed 
##   1.964   0.014   1.980
```



```r
print(object.size(corpus_tm), quote = FALSE, units = "Mb")
```



```
## 121.4 Mb
```



```r
# 121.4 Mb!!!
system.time ( corpus_tm <- tm_map(corpus_tm, content_transformer(simple_preprocess)) )
```



```
##    user  system elapsed 
##  10.352   0.237   6.564
```



```r
system.time ( dtm_tm <- DocumentTermMatrix(corpus_tm, control = list(tokenize = words) ) )
```



```
##    user  system elapsed 
##  14.411   0.759  11.442
```
Now lets check timings for **tmlite**:

```r
system.time ( corp <- create_dict_corpus(src = txt, 
                   preprocess_fun = simple_preprocess, 
                   # simple_tokenizer - split string by whitespace
                   tokenizer = simple_tokenizer, 
                   # read by batch of 5000 documents
                   batch_size = 5000, 
                   # do not show progress bar because of knitr
                   progress = FALSE) )
```



```
##    user  system elapsed 
##   9.547   0.099   9.656
```



```r
# get in dgTMatrix form, because tm stores dtm matrix in triplet form
system.time ( dtm <- get_dtm(corpus = corp, type = "dgTMatrix"))
```



```
##    user  system elapsed 
##   0.043   0.007   0.051
```
Well, **only two times faster**. Is it worth the effort? Lets check another example. Here we will use [data](https://d396qusza40orc.cloudfront.net/mmds/datasets/sentences.txt.zip) from excellent [Mining massive datasets](https://www.coursera.org/course/mmds) course. This is quite a large collection of short texts - more than 9 million rows, 500Mb zipped and about 1.4Gb unzipped.

```r
# we will read only small fraction - 200000 rows (~ 42Mb)
txt <- readLines('~/Downloads/sentences.txt', n = 2e5)
print(object.size(txt), quote = FALSE, units = "Mb")
```



```
## 41.7 Mb
```



```r
# 41.7 Mb
# VCorpus is very slow, about 20 sec on my computer
system.time ( corpus_tm <- VCorpus(VectorSource(txt)) )
```



```
##    user  system elapsed 
##  20.336   0.220  20.579
```



```r
print(object.size(corpus_tm), quote = FALSE, units = "Mb")
```



```
## 749.8 Mb
```



```r
# 749.8 Mb!!! wow!
system.time ( corpus_tm <- tm_map(corpus_tm, content_transformer(simple_preprocess)) )
```



```
##    user  system elapsed 
##  19.966   1.432  27.741
```



```r
# 26 sec. To process 42 Mb of text.
```
But the following is trully absurd. This forks 2 processes (because it uses mclapply internally). Each process uses 1.3Gb of RAM. **2.6 Gb of RAM to process 42 Mb text chunk**. And this takes more then 50 sec on my macbook pro with latest core i7 intel chip. In fact it is not possible to process 1 million rows (200Mb) from my macbook pro with 16 gb of RAM.

```r
system.time ( dtm_tm <- DocumentTermMatrix(corpus_tm, control = list(tokenize = words) ) )
```



```
##    user  system elapsed 
##  94.081   3.826  50.138
```
Compare with **tmlite**:

```r
system.time ( corp <- create_dict_corpus(src = txt, 
                   preprocess_fun = simple_preprocess, 
                   # simple_tokenizer - split string by whitespace
                   tokenizer = simple_tokenizer, 
                   # read by batch of 5000 documents
                   batch_size = 5000, 
                   # do not show progress bar because of knitr
                   progress = F) )
```



```
##    user  system elapsed 
##   9.458   0.053   9.515
```



```r
# only around 9 sec and 120 Mb of ram
system.time ( dtm_tmlite <- get_dtm(corpus = corp, type = "dgTMatrix"))
```



```
##    user  system elapsed 
##   0.107   0.015   0.122
```



```r
# less than 1 second
```
So here **tmlite 8 times faster** and what is much more important **consumes 20 times less RAM**. On large collections of documents speed up will be even more significant. 

## Document-Term Matrix manipulations
In practice it can be usefull to remove common and uncommon terms. Both packages provide functions for that: `removeSparseTerms()` in **tm** and `dtm_remove_common_terms` in **tmlite**. Also note, that `removeSparseTerms()` can only remove uncommon terms, so to be fair we will test only that functionality:

```r
system.time( dtm_tm_reduced <- removeSparseTerms(dtm_tm, 0.99))
```



```
##    user  system elapsed 
##   1.274   0.101   1.375
```



```r
# common = 1 => do not remove common terms
system.time( dtm_tmlite_reduced <- dtm_tmlite %>% 
               dtm_transform(filter_commons_transformer, term_freq = c(common = 0.001, uncommon = 0.975)))
```



```
##    user  system elapsed 
##   0.298   0.078   0.377
```
3-5 times faster - not bad. 
Now compare tf-idf transformation:

```r
system.time( dtm_tm_tfidf <- weightTfIdf(dtm_tm, normalize = T))
```



```
## Warning in weightTfIdf(dtm_tm, normalize = T): empty document(s): 6782
## 26135 26136 26137 26138 26139 26140 26141 26142 26143 26144 26145 27664
## 60895 60896 60897 60898 60899 60900 88953 106921 122685 141442 141443
## 141449 141454 152656
```



```
##    user  system elapsed 
##   0.214   0.026   0.240
```



```r
# common = 1 => do not remove common terms
# timings slightly greate than weightTfIdf, because all transformations optimized for 
# dgCMatrix format, which is standart for sparse matrices in R
system.time( dtm_tmlite_tfidf <- dtm_tmlite %>% 
               dtm_transform(tfidf_transformer))
```



```
##    user  system elapsed 
##   0.325   0.083   0.408
```



```r
# for dtm in dgCMatrix timings should be equal
dtm_tmlite_dgc<-  as(dtm_tmlite, "dgCMatrix")
system.time( dtm_tmlite_tfidf <- dtm_tmlite_dgc %>% 
               dtm_transform(tfidf_transformer))
```



```
##    user  system elapsed 
##   0.214   0.045   0.259
```
Equal timings -  great (and surprise for me) - within the last year **tm** authors have significantly improved its performace!

## Future work
Project has [issue tracker on github](https://github.com/dselivanov/tmlite/issues) where I'm filing feature requests and notes for future work. Any ideas are very appreciated.

If you like it, you can **help**:

- Test and leave feedback on [github issuer tracker](https://github.com/dselivanov/tmlite/issues) (preferably) or directly by email.
    - package is tested on linux and OS X platforms, so Windows users are especially welcome
- Fork and start contributing. Vignettes, docs, tests, use cases are very welcome.
- Or just give me a star on [project page](https://github.com/dselivanov/tmlite) :-)

### Short-term plans
- add tests
- add n-gram tokenizers
- add methods for tokenization in C++ (at the moment tokenization takes almost half of runtime)
- switch to murmur3 hash and add second hash function to reduce probability of collision
- push dictionary and stopwords filtering into C++ code

### Middle-term plans
- add **[word2vec](https://code.google.com/p/word2vec/) wrapper**. It is strange, that R community still didn't have it.
- add corpus serialization

### Long-term plans
- integrate models like it is done in [gensim](https://radimrehurek.com/gensim/)
- try to implement out-of-core transformations like [gensim](https://radimrehurek.com/gensim/) does
