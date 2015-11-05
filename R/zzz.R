loadModule("VocabCorpus", TRUE)
loadModule("HashCorpus", TRUE)
loadModule("VocabularyBuilder", TRUE)
loadModule("GloveFitter", TRUE)

.onUnload <- function (libpath) {
  library.dynam.unload("text2vec", libpath)
}
