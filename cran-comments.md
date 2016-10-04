## Release summary

This is a resubmission:

* Solved issue with VignetteBuilder / vignette index from previous submission.

## Test environments

* local OS X install, R 3.3.1
* ubuntu 12.04.5 (on travis-ci), R 3.3.1
* win-builder

## R CMD check results

commit 837d7568

0 ERRORs | 0 WARNING | 3 NOTEs

* checking installed package size ... NOTE. 
    - This is all compiled code in the libs/ directory. Data was not touched since previous release. Added more vignettes.

* Possibly mis-spelled words in DESCRIPTION ... NOTE
    - All words are correct.

* checking for GNU extensions in Makefiles ... NOTE
    - GNU make needed for RcppParallel
