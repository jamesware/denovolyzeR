## Updated version
This is a major release -  addresses a bug whereby data columns were incorrectly labelled under some circumstances if user overrode default parameters 

## Test environments
* local OS X install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs on my local install, nor on ubuntu.

On win-builder there is one note:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'James Ware <j.ware@imperial.ac.uk>'

Possibly mis-spelled words in DESCRIPTION:
  De (2:32)
  Novo (2:35)
  de (10:56, 12:36, 13:59, 16:59)
  denovolyzeR (11:32, 15:41)
  novo (10:59, 12:39, 13:62, 16:62)
  toolset (10:28)
  
These spellings are all as intended, and this part of the DESCRIPTION is unchanged relative to the previous release.
  
## Downstream dependencies
There are no downstream dependencies
