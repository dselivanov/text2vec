[![Travis-CI Build Status](https://travis-ci.org/dselivanov/text2vec.svg?branch=master)](https://travis-ci.org/dselivanov/text2vec)
[![License](http://img.shields.io/:license-mit-blue.svg?style=flat)](http://badges.mit-license.org)

# Tutorials

1. [Vectorization](http://dsnotes.com/blog/text2vec/2015/11/09/text2vec/).
2. [GloVe on english wikipedia](http://dsnotes.com/blog/text2vec/2015/12/01/glove-enwiki/).

# Features

**text2vec** is a package for which the main goal is to provide an **efficient framework** with **concise API** for **text analysis** and **natural language processing (NLP)** in R. It is inspired by [gensim](http://radimrehurek.com/gensim/) - an excellent python library for NLP.

## Core functionality

At the moment we cover two following topics:  

1. Fast text vectorization on arbitrary n-grams.
    - using vocabulary
    - using feature hashing
2. State-of-the-art [GloVe](http://www-nlp.stanford.edu/projects/glove/) word embeddings.

## Efficiency  

- The core of the functionality is **carefully written in C++**. Also this means text2vec is **memory friendly**.
- Some parts (GloVe training) are fully **parallelized** using an excellent [RcppParallel](http://rcppcore.github.io/RcppParallel/) package. This means, **parallel features work on OS X, Linux, Windows and Solaris(x86) without any additinal tuning/hacking/tricks**.
- **Streaming API**, this means users don't have to load all the data into RAM. **text2vec** allows processing streams of chunks.

## API

- Built around [iterator](https://en.wikipedia.org/wiki/Iterator) abstraction.
- Concise, provides only a few functions, which do their job well.
- Don't (and probably will not in future) provide trivial very high-level functions.

# Terminology and what is under the hood

As stated before, text2vec is built around streaming API and **iterators**, which allows the constructin of the **corpus** from *iterable* objects. Here we touched 2 main concepts:

1. **Corpus**.  In text2vec it is an object, which contains tokens and other information / metainformation which is used for text vectorization and other processing. We can be efficiently insert documents into corpus, because,  technically, **Corpus** is an C++ class, wrapped with *Rcpp-modules* as *reference class* (which has reference semantics!). Usually user should not care about this, but should keep in mind nature of such objects. Particularly important, that user have to remember, that he can't save/serialize such objects using R's `save*()` methods. But good news is that he can easily and efficiently extract corresponding R objects from corpus and work with them in a usual way.
1. **Iterators**. If you are not familliar with them in `R's` context, I highly recommend to review vignettes of [iterators](https://cran.r-project.org/web/packages/iterators/) package. A big advantage of this abstraction is that  it allows us to be **agnostic of type of input** - we can transparently change it by just providing correct iterator.

# Contributors are very welcome
Project has [issue tracker on github](https://github.com/dselivanov/text2vec/issues) where I'm filing feature requests and notes for future work. Any ideas are very appreciated.

If you like it, you **can help**:

- Test and leave feedback on [github issuer tracker](https://github.com/dselivanov/text2vec/issues) (preferably) or directly by email.
    - package is tested on linux and OS X platforms, so Windows users are especially welcome.
- Fork and start contributing. Vignettes, docs, tests, use cases are very welcome.
- Or just give me a star on [project page](https://github.com/dselivanov/text2vec) :-)
