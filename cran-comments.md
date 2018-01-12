## Release summary

* clarification - I confirm that I've fixed UBSAN warnings (checked with https://github.com/rocker-org/r-devel-san-clang)
* fixed 2017->2018 year in description

## Test environments

* local OS X install, R 3.4.2
* r-devel-ubsan-clang, R 3.4.1
* ubuntu 12.04.5 (on travis-ci), R 3.4.1
* win-builder

## R CMD check results

0 ERRORs | 0 WARNING | 2 NOTEs

* checking installed package size ... NOTE. 
    - Data was not touched since previous release. Vignettes were improved, 
    but have same size as in previous release.
* checking for GNU extensions in Makefiles ... NOTE
    - GNU make needed for RcppParallel
