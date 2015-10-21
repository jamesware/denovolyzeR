---
layout: post
title: denovolyzeR
---

**denovolyzeR**: an `R` package for the statistical analysis of *de novo* variants.

[![Travis-CI Build Status](https://travis-ci.org/jamesware/denovolyzeR.png?branch=master)](https://travis-ci.org/jamesware/denovolyzeR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/denovolyzeR)](http://cran.r-project.org/package=denovolyzeR)

The statistical framework is published in [Nature Genetics](http://www.nature.com/doifinder/10.1038/ng.3050)

The functions in this package calculate whether a study population carry more *de novo* variants than expected.

### Installation

The latest released version can be installed from CRAN with

```
install.packages("denovolyzeR")
```

The latest development version can be installed from GitHub using devtools:

```
## Install devtools if not already installed
if(!"devtools" %in% installed.packages()){
	install.packages("devtools")
}

## install denovolyzeR
devtools::install_github("jamesware/denovolyzeR")
```

Alternative mutational probability tables that can be downloaded for use with **denovolyzeR** can be found [here](/alternativeProbabilityTables/)




### Download

Hyde is developed on and hosted with GitHub. Head to the <a href="https://github.com/poole/hyde">GitHub repository</a> for downloads, bug reports, and features requests.

Thanks!


This website is built using [Jekyll](http://jekyllrb.com) and [Hyde](http://hyde.getpoole.com).