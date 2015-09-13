Reasonable question will - why new package? R already have such great package as [tm](https://cran.r-project.org/web/packages/tm/) and companion package [tau](https://cran.r-project.org/web/packages/tau/)?

I'll try to answer (**IMHO**):

1. **tm** has a lot of functions - in fact reference manual contains more than 50 pages. But it's tm's API is very messy. A lot of packages depends on it , so it is hard redesign it.
2. **tm** is not very efficient (from my experience). I found it is slow and more important it is very RAM unfriendly and RAM-greedy. (I'll provide few examples below).
3. can only handle documents that fit in RAM.
4. As I understand it is designed more for academia researchers, then data science practioners. It perfectly process document metadata, but not very efficient in Document-Term matrix manipulations.

So I developed [tmlite](https://github.com/dselivanov/tmlite) to address these issues. 

## Key features

The philosophy of the package is inspired by unix philosphy - [Do One Thing and Do It Well](https://en.wikipedia.org/wiki/Unix_philosophy#Do_One_Thing_and_Do_It_Well). R ecosystem contains a lot of packages that are well suited for text modeling. Here are my favourite:

- [lda](https://cran.r-project.org/web/packages/lda/index.html) blazing fast package for topic modeling.
- [glmnet](https://cran.r-project.org/web/packages/glmnet/index.html) for L1, L2 linear models.
- [xgboost](https://cran.r-project.org/web/packages/xgboost/) for gradient boosting. 
- [LiblineaR](https://cran.r-project.org/web/packages/LiblineaR/index.html) - wrapper of `liblinear` svm library.

They are all excelent and very efficient packages, so we **tmlite** will be focuses not on modeling, but on framework - Document-Matrix construction and manipulation - base for any text-mining analysis.

**Key features**:

1. Flexible and easy functional-style API.
2. Efficient **streaming** corpus construction. **tmlite**'s provides API for construction corporas from `character` vectors and more important - [connections](https://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html). So it is possible (and easy!) to construct Document-Term matrices for collections of documents thar are don't fit in the memory.
3. Fast - core functions are written in C++, thanks to [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html) authors.
4. Has two main corpus classes - 
    - `DictCorpus` - traditional dictionary-based container used for Document-Term matrix construction.
    - `HashCorpus` - container that implements [feature hashing](https://en.wikipedia.org/wiki/Feature_hashing) or **"hashing trick"**. Similar to [scikit-learn FeatureHasher](http://scikit-learn.org/stable/modules/feature_extraction.html#feature-hashing) and  [gensim corpora.hashdictionary](https://radimrehurek.com/gensim/corpora/hashdictionary.html).

## Quick reference
Quick example, based on kaggle [Bag of Words Meets Bags of Popcorn](https://www.kaggle.com/c/word2vec-nlp-tutorial) competition [labeledTrainData.tsv.zip](https://www.kaggle.com/c/word2vec-nlp-tutorial/download/labeledTrainData.tsv.zip).
load libraries
```r
library(tmlite)
library(magrittr)
library(lda)
```
Simple preprocessing function. Only one trick - read only third column - text of the review.
```r
preprocess_fun <- function(x) {
  # file is tab-sepatated
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
con <- file('~/Downloads/labeledTrainData.tsv', open = 'r', blocking = F)
corp <- create_dict_corpus(src = con, 
                   preprocess_fun = preprocess_fun, 
                   # simple_tokenizer - split string by whitespace
                   tokenizer = simple_tokenizer, 
                   # read by batch of 1000 documents
                   batch_size = 1000,
                   # skip first row - header
                   skip = 1)
```
Now we want to do some topic modeling using **lda** package. So we have to create Document-Term matrix
in `lda-c` format. It is easy with `get_dtm` function:
```r
dtm <- get_dtm(corpus = corp, type = "LDA_C")
vocab <- corp$dict %>% names
fit <- lda.collapsed.gibbs.sampler(documents = dtm, 
                                       K = 30, 
                                       vocab = vocab,
                                       num.iterations = 100, 
                                       alpha = 0.1, 
                                       eta = 0.05,
                                       initial = NULL,
                                       burnin = 0,
                                       compute.log.likelihood = TRUE, 
                                       trace = 1L)
```
