[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/text2vec)](https://cran.r-project.org/package=text2vec)
[![Travis-CI Build Status](https://travis-ci.org/dselivanov/text2vec.svg?branch=master)](https://travis-ci.org/dselivanov/text2vec)
[![codecov](https://codecov.io/gh/dselivanov/text2vec/branch/master/graph/badge.svg)](https://codecov.io/gh/dselivanov/text2vec/branch/master)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/text2vec)](https://cran.r-project.org/package=text2vec)
[![Follow](https://img.shields.io/twitter/follow/dselivanov_.svg?style=social)](https://twitter.com/intent/follow?screen_name=dselivanov_)


You've just discovered **[text2vec](http://text2vec.org)**!

**text2vec** is an R package which provides an efficient framework with a concise API for text analysis and natural language processing (NLP). 

Goals which we aimed to achieve as a result of development of `text2vec`:

* **Concise** - expose as few functions as possible
* **Consistent** - expose unified interfaces, no need to explore new interface for each task
* **Flexible** - allow to easily solve complex tasks
* **Fast** - maximize efficiency per single thread, transparently scale to multiple threads on multicore machines
* **Memory efficient** - use streams and iterators, not keep data in RAM if possible

# Tutorials

To learn how to use this package, see [text2vec.org](http://text2vec.org) and the package vignettes.
See also the [text2vec articles](http://dsnotes.com/tags/text2vec/) on my blog.

# Features

The core functionality at the moment includes

1. Fast text vectorization on arbitrary n-grams, using vocabulary or feature hashing.
2. [GloVe](http://www-nlp.stanford.edu/projects/glove/) word embeddings.
3. Topic modeling with:
  - Latent Dirichlet Allocation
  - Latent Sematic Analysis
4. Similarities/distances between 2 matrices
  - Cosine
  - Jaccard
  - [Relaxed Word Mover's Distance](http://vene.ro/blog/word-movers-distance-in-python.html)
  - Euclidean

# Performance
![htop](https://raw.githubusercontent.com/dselivanov/text2vec/gh-pages/images/htop.png)
*Author of the package is a little bit obsessed about efficiency.*

This package is efficient because it is carefully written in C++, which also means that text2vec is memory friendly. Some parts, such as training GloVe word embeddings, are fully parallelized using the excellent [RcppParallel](http://rcppcore.github.io/RcppParallel/) package. This means that the word embeddings are computed in parallel on OS X, Linux, Windows, and Solaris (x86) without any additional tuning or tricks.
Other emrassingly parallel tasks such as vectorization can use any parallel backend wich supports [foreach](https://cran.r-project.org/package=foreach) package. So they can achieve near-linear scalability with number of available cores. 
Finally, a streaming API means that  users do not have to load all the data into RAM. 

# Contributing

The package has [issue tracker on GitHub](https://github.com/dselivanov/text2vec/issues) where I'm filing feature requests and notes for future work. Any ideas are appreciated.

Contributors are welcome. You can help by:

- testing and leaving feedback on the [GitHub issuer tracker](https://github.com/dselivanov/text2vec/issues) (preferably) or directly by e-mail
- forking and contributing (chech [code style guide](https://github.com/dselivanov/text2vec/wiki/Code-style-guide)). Vignettes, docs, tests, and use cases are very welcome
- by giving me a star on [project page](https://github.com/dselivanov/text2vec) :-)

# License

GPL (>= 2)
