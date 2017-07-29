## Release summary

* This is resubmission. I removed usage of unexported `print.data.table` as was asked by CRAN team.
* Not sure how to reduce checking time 
    - on my machine cheking takes ~1 min
    - package uses a lot of compiled code (heavy tempated libraries like RcppParallel, Rcpp, sparsepp)

## Test environments

* local OS X install, R 3.4.0
* ubuntu 12.04.5 (on travis-ci), R 3.4.1
* win-builder

## R CMD check results

commit 19ee0ff1

0 ERRORs | 0 WARNING | 3 NOTEs

* checking installed package size ... NOTE. 
    - Data was not touched since previous release. Vignettes were improved, 
    but have same size as in previous release.
* checking for GNU extensions in Makefiles ... NOTE
    - GNU make needed for RcppParallel
* checking compiled code ... NOTE: *Found ‘___stderrp’, possibly from ‘stderr’ (C)*
    - This is from header-only `sparsepp` library I'm linking to. I already overrided
    `exit` call in `sparsepp` namespace, but can't do the same for `fprintf` call
    in `sparsepp` internals. It is out of my control since this is 3rd party library.
