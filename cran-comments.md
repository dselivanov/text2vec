## Release summary

* Resumbission - chenged `stop()` c++ calls to `Rf_error`

## Test environments

* local OS X install, R 3.4.2
* ubuntu 12.04.5 (on travis-ci), R 3.4.1
* win-builder

## R CMD check results

0 ERRORs | 0 WARNING | 2 NOTEs

* checking installed package size ... NOTE. 
    - Data was not touched since previous release. Vignettes were improved, 
    but have same size as in previous release.
* checking for GNU extensions in Makefiles ... NOTE
    - GNU make needed for RcppParallel
