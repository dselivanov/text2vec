.onUnload = function(libpath) {# nocov start
  library.dynam.unload("text2vec", libpath)
}# nocov end

.onAttach = function(libname, pkgname) {# nocov start
  # Runs when attached to search() path such as by library() or require()
  if (interactive()) {
    packageStartupMessage(
"text2vec is still in beta version - APIs can be changed.
For tutorials and examples visit http://text2vec.org.

For FAQ refer to
  1. https://stackoverflow.com/questions/tagged/text2vec?sort=newest
  2. https://github.com/dselivanov/text2vec/issues?utf8=%E2%9C%93&q=is%3Aissue%20label%3Aquestion
If you have questions please post them at StackOverflow and mark with 'text2vec' tag."
)
  }
}# nocov end
