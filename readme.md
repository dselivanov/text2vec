[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/text2vec)](http://cran.r-project.org/package=text2vec)
[![Travis-CI Build Status](https://travis-ci.org/dselivanov/text2vec.svg?branch=master)](https://travis-ci.org/dselivanov/text2vec)
[![License](http://img.shields.io/:license-mit-blue.svg?style=flat)](http://badges.mit-license.org)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/text2vec)](http://cran.r-project.org/package=text2vec)
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

The core of this package is carefully written in C++, which means text2vec is fast and memory friendly. Some parts (GloVe training) are fully parallelized using the excellent [RcppParallel](http://rcppcore.github.io/RcppParallel/) package. This means that parallel processing works on OS X, Linux, Windows and Solaris (x86) without any additional hacking or tricks. In addition, there is a higher-level parallelization for text vectorization and vocabulary construction on top of the [foreach](https://cran.r-project.org/package=foreach) package, and text2vec has a streaming API so that users don't have to load all of the data into RAM.

The API is built around the [iterator](https://en.wikipedia.org/wiki/Iterator) abstraction. The API is concise, providing only a few functions which do their job well. The package does not (and probably will not in the future) provide trivial very high-level functions. But other packages can build on top of the framework that text2vec provides.

# Contributing

The package has [issue tracker on GitHub](https://github.com/dselivanov/text2vec/issues) where I'm filing feature requests and notes for future work. Any ideas are appreciated.

Contributors are welcome. You can help by

- testing and leaving feedback on the [GitHub issuer tracker](https://github.com/dselivanov/text2vec/issues) (preferably) or directly by e-mail.
- forking and contributing. Vignettes, docs, tests, and use cases are very welcome.
- by giving me a star on [project page](https://github.com/dselivanov/text2vec) :-)
