<!-- README.md is generated from README.Rmd. Please edit that file -->



**denovolyzeR**: an `R` package for the statistical analysis of *de novo* variants
==================================================================================

This repository is the home of **denovolyzeR**, an `R` implementation of a statistical framework for the analysis of *de novo* genetic variants.

The statistical framework is published in [Nature Genetics](http://www.nature.com/doifinder/10.1038/ng.3050).
To cite the R package, please cite our manuscript in [Current Protocols in Human Genetics](http://onlinelibrary.wiley.com/doi/10.1002/0471142905.hg0725s87/abstract), also available on [PubMed Central](http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4606471/), as follows:

> Ware, J.S., Samocha, K.E., Homsy, J., and Daly, M.J. 2015. Interpreting de novo variation in human disease using denovolyzeR. Curr. Protoc. Hum. Genet. 87:7.25.1-7.25.15. doi: 10.1002/0471142905.hg0725s87

The functions in this package calculate whether a study population carry more *de novo* variants than expected.

The latest released version can be installed from CRAN with

``` {.r}
install.packages("denovolyzeR")
```

The latest development version can be installed from GitHub using devtools:

``` {.r}
## Check whether devtools is installed
"devtools" %in% installed.packages()

## install devtools if required
install.packages("devtools")

## install denovolyzeR
library(devtools)
install_github("jamesware/denovolyzeR")
```

Alternative mutational probability tables that can be downloaded for use with **denovolyzeR** can be found [here](/alternativeProbabilityTables/)

[![Travis-CI Build Status](https://travis-ci.org/jamesware/denovolyzeR.png?branch=master)](https://travis-ci.org/jamesware/denovolyzeR)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/denovolyzeR)](http://cran.r-project.org/package=denovolyzeR)
