[![Travis-CI Build Status](https://travis-ci.org/dselivanov/text2vec.svg?branch=redesign)](https://travis-ci.org/dselivanov/text2vec)
[![License](http://img.shields.io/:license-mit-blue.svg?style=flat)](http://badges.mit-license.org)
# Features
Unfortunately, current R's infrastructure around text-mining tasks can't provide reliable tools for efficient and memory-friendly analysis pipeline.
**text2vec** is a package which main goal is to provide **efficient framework** with **concise API** for **text analysis** and **natural language processing (NLP)** in R. It is inspired by [gensim](http://radimrehurek.com/gensim/) - excellent matured python library for NLP.

## Core functionality

At the moment we cover two following topics:  

1. Fast text vectorization on arbitrary n-grams.
    - using vocabulary
    - using feature hashing
2. State-of-the-art [GloVe](http://www-nlp.stanford.edu/projects/glove/) word embeddings.

## Efficiency  

- Core of the functionality is **carefully written in C++**. Also this means text2vec is **memory friendly**.
- Some parts are fully **parallelized** using excellent [RcppParallel](http://rcppcore.github.io/RcppParallel/) package. 
- **Streaming API**, this means users don't have to load all the data into RAM. **text2vec** allows to process stream of chunks.

## API

- Build around [iterator](https://en.wikipedia.org/wiki/Iterator) abstraction.
- Concise, provides only few functions, which do their job well.
- Don't (and probably will not in future) provide trivial very high-level functions.

# Terminology and what is under the hood

As stated before, text2vec is built around streaming API and **iterators**, which allows to construct **corpus** from *iterable* objects. Here we touched 2 main concepts:

1. **Corpus**.  In text2vec it is an object, which contains tokens and other information / metainformation which is used for text vectorization and other processing. We can be efficiently insert documents into corpus, because,  technically, **Corpus** is an C++ class, wrapped with *Rcpp-modules* as *reference class* (which has reference semantics!). Usually user should not care about this, but should keep in mind nature of such objects. Particularly important, that user have to remember, that he can't save/serialize such objects using R's `save*()` methods. But good news is that he can easily and efficiently extract corresponding R objects from corpus and work with them in a usual way.
1. **Iterators**. If you are not familliar with them in `R's` context, I highly recommend to review vignettes of [iterators](https://cran.r-project.org/web/packages/iterators/) package. A big advantage of this abstraction is that  it allows us to be **agnostic of type of input** - we can transparently change it just providing correct iterator.

# Text vectorization

Historically, most of the text-mining and NLP modelling is related to [Bag-of-words](https://en.wikipedia.org/wiki/Bag-of-words_model) or [Bag-of-ngrams](https://en.wikipedia.org/wiki/N-gram). Despite of simplicity these models usually demonstrates good performance on text categorization/classification tasks. But, in contrast to theoretical simplicity and practical efficiency, building *bag-of-words* models involves technical challenges (especially within `R` framework, because of its typical copy-on-modify semantics). Lets briefly review some details of typical analysis pipeline:

1. Usually reseacher have to construct [Document-Term matrix](https://en.wikipedia.org/wiki/Document-term_matrix) (DTM) from imput documents. Or in other words, **vectorize text** - create mapping from words/ngrams to [vector space](https://en.wikipedia.org/wiki/Vector_space_model).
1. Fit model on this DTM. This can include:
    - text classification
    - topic modeling
    - ...
1. Tune, validate model.
1. Apply model on new data.

# Future work

### Contributors are very welcome
Project has [issue tracker on github](https://github.com/dselivanov/text2vec/issues) where I'm filing feature requests and notes for future work. Any ideas are very appreciated.

If you like it, you can **help**:

- Test and leave feedback on [github issuer tracker](https://github.com/dselivanov/text2vec/issues) (preferably) or directly by email.
    - package is tested on linux and OS X platforms, so Windows users are especially welcome
- Fork and start contributing. Vignettes, docs, tests, use cases are very welcome.
- Or just give me a star on [project page](https://github.com/dselivanov/text2vec) :-)
