loadModule("VocabCorpus", TRUE)
loadModule("HashCorpus", TRUE)
loadModule("VocabularyBuilder", TRUE)
loadModule("GloveFitter", TRUE)

.onUnload = function(libpath) {# nocov start
  library.dynam.unload("text2vec", libpath)
}# nocov end

.onAttach = function(libname, pkgname) {# nocov start
  # Runs when attached to search() path such as by library() or require()
  if (interactive()) {
    packageStartupMessage("text2vec is still in beta version - APIs can be changed.
                          \nFor tutorials and examples visit http://text2vec.org")
  }
}# nocov end
