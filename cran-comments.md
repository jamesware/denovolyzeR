## Test environments
* local OS X install, R 3.1.3
* ubuntu 12.04 (on travis-ci), R 3.1.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There is 1 NOTE: this is my first submission to CRAN.

On ubuntu (travis-ci) there is a second note:
"checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped"
I believe this is because not repo is set on the travis-ci virtual machine.  This NOTE is not present on other platforms.

## Downstream dependencies
There are no downstream dependencies
