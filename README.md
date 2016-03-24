[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/text2vec)](http://cran.r-project.org/package=text2vec)
[![Travis-CI Build Status](https://travis-ci.org/dselivanov/text2vec.svg?branch=master)](https://travis-ci.org/dselivanov/text2vec)
[![License](http://img.shields.io/:license-mit-blue.svg?style=flat)](http://badges.mit-license.org)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/text2vec)](http://cran.rstudio.com/package=text2vec)
[![Follow](https://img.shields.io/twitter/follow/dselvan0v.svg?style=social)](https://twitter.com/intent/follow?screen_name=dselvan0v)

# Tutorials

To learn how to use this package, see the package vignettes.

1. [Text vectorization](https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html): `vignette("text-vectorization", package = "text2vec")`
2. [GloVe word embeddings](https://cran.r-project.org/web/packages/text2vec/vignettes/glove.html): `vignette("glove", package = "text2vec")`

See also the [text2vec articles](http://dsnotes.com/tags/text2vec/) on my blog.

# Features

**text2vec** is a package that provides an efficient framework with a concise API for text analysis and natural language processing (NLP) in R. It is inspired by [gensim](http://radimrehurek.com/gensim/), an excellent Python library for NLP.

The core functionality at the moment includes

1. Fast text vectorization on arbitrary n-grams, using vocabulary or feature hashing.
2. State-of-the-art [GloVe](http://www-nlp.stanford.edu/projects/glove/) word embeddings.

## Efficiency  

The core of this package is carefully written in C++, which means text2vec is fast and memory friendly. Some parts (GloVe training) are fully parallelized using the excellent [RcppParallel](http://rcppcore.github.io/RcppParallel/) package. This means that parallel processing works on OS X, Linux, Windows and Solaris (x86) without any additional hacking or tricks. In addition, there is a higher-level parallelization for text vectorization and vocabulary construction on top of the [foreach](https://cran.r-project.org/package=foreach) package, and text2vec has a streaming API so that users don't have to load all of the data into RAM.

## API

The API is built around the [iterator](https://en.wikipedia.org/wiki/Iterator) abstraction. The API is concise, providing only a few functions which do their job well. The package does not (and probably will not in the future) provide trivial very high-level functions. But other packages can build on top of the framework that text2vec provides.

## Terminology 

As stated before, text2vec is built around streaming API and iterators, which allow for the construction of the corpus from interable objects. Here we touch on two main concepts:

1. **Corpus**.  In text2vec a corpus is an object which contains tokens and other information or meta-information which is used for text vectorization and other processing. We can be efficiently insert documents into corpus because, technically, **Corpus** is an C++ class, wrapped with *Rcpp-modules* as *reference class* (which has reference semantics). Usually the user will not care about this, but should keep in mind the way such objects work in R. Particularly important, that has to remember that you cannot save or serialize such objects using R's `save*()` methods. But you can easily and efficiently extract corresponding R objects from the corpus, such as document-term matrices or term-co-occurence matrices and work with them in a usual way.
2. **Iterators**. If you are not familliar with iterators in the context of R, I recommend that you review the vignettes of the [iterators](https://cran.r-project.org/web/packages/iterators/) package. A big advantage of this abstraction is that it allows us to be agnostic about the type of input: we can transparently change the input by just providing correct iterator.

# Contributors are welcome

The package has [issue tracker on GitHub](https://github.com/dselivanov/text2vec/issues) where I'm filing feature requests and notes for future work. Any ideas are appreciated.

If you like it, you can help by

- testing and leaving feedback on the [GitHub issuer tracker](https://github.com/dselivanov/text2vec/issues) (preferably) or directly by email.
- forking and contributing. Vignettes, docs, tests, and use cases are very welcome.
- or just by giving me a star on [project page](https://github.com/dselivanov/text2vec) :-)
