## Release summary

* New major release.

## Test environments

* local OS X install, R 3.4.0
* ubuntu 12.04.5 (on travis-ci), R 3.4.1
* win-builder

## R CMD check results

commit 20f3b63

0 ERRORs | 0 WARNING | 4 NOTEs

* checking installed package size ... NOTE. 
    - Data was not touched since previous release. Vignettes were improved, 
    but have same size as in previous release.
* Unexported object imported by a ':::' call: ‘data.table:::print.data.table’
    - I use `data.table::print` for pretty printing of `data.farmes`. 
    I can convert my internal `data.frame` into `data.table` but don't want do that 
    because of performance penalties and copying of attributes back and forth.
    `data.table:::print.data.table` is very stable function.
* checking for GNU extensions in Makefiles ... NOTE
    - GNU make needed for RcppParallel
* checking compiled code ... NOTE: *Found ‘___stderrp’, possibly from ‘stderr’ (C)*
    - This is from header-only `sparsepp` library I'm linking to. I already overrided
    `exit` call in `sparsepp` namespace, but can't do the same for `fprintf` call
    in `sparsepp` internals. It is out of my control since this is 3rd party library.
