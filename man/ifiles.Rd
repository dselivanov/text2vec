% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterators.R
\name{ifiles}
\alias{ifiles}
\alias{idir}
\alias{ifiles_parallel}
\title{Creates iterator over text files from the disk}
\usage{
ifiles(file_paths, reader = readLines)

idir(path, reader = readLines)

ifiles_parallel(file_paths, reader = readLines, ...)
}
\arguments{
\item{file_paths}{\code{character} paths of input files}

\item{reader}{\code{function} which will perform reading of text
files from disk, which should take a path as its first argument. \code{reader()} function should
return \bold{named character vector: elements of vector = documents,
names of the elements = document ids which will be used in DTM construction}.
If user doesn't provide named character vector, document ids will be generated as
file_name + line_number (assuming that each line is a document).}

\item{path}{\code{character} path of directory. All files in the directory will be read.}

\item{...}{other arguments (not used at the moment)}
}
\description{
The result of this function usually used in an \link{itoken} function.
}
\examples{
\dontrun{
current_dir_files = list.files(path = ".", full.names = TRUE)
files_iterator = ifiles(current_dir_files)
parallel_files_iterator = ifiles_parallel(current_dir_files, n_chunks = 4)
it = itoken_parallel(parallel_files_iterator)
dtm = create_dtm(it, hash_vectorizer(2**16), type = 'TsparseMatrix')
}
dir_files_iterator = idir(path = ".")
}
\seealso{
\link{itoken}
}
