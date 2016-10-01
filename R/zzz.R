loadModule("VocabCorpus", TRUE)
loadModule("HashCorpus", TRUE)
loadModule("VocabularyBuilder", TRUE)
loadModule("GloveFitter", TRUE)

.onUnload = function(libpath) {
  library.dynam.unload("text2vec", libpath)
}

.onAttach = function(libname, pkgname) {
  # Runs when attached to search() path such as by library() or require()
  if (interactive()) {
    packageStartupMessage("text2vec is still in beta version - API can be changed.
                          \nFor tutorials and examples visit http://text2vec.org")
  }
}
