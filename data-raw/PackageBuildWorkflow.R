#.libPaths()
library("devtools")
library("roxygen2")
library("knitr")
library("dplyr")
#library("testthat")

## Initialise version control
## Move to shell to link to github:
# git remote add origin https://github.com/jamesware/denovolyzeR.git
## Run once to create new package framework
# devtools::create("~/Documents/MyR/denovolyzeR")
## Run once to initialise vignette
# devtools::use_vignette("denovolyzeR_intro")
## Run once to establish data-raw folder (ignored when building package)
# devtools::use_data_raw()

## Compile raw data into internal data format
parseRawData.Rmd
#####

## Run everytime documentation updated
# devtools::document() ## (or press Cmd + Shift + D in RStudio)

## Run when functions or documents updated, to reload
## reloads in workspace, but doesn't install
# devtools::load_all()  #  Cmd + Shift + L

## Run each time vignette updated
# devtools::build_vignettes()

## does full install, except for vignettes
## Build & reload, Cmd + Shift + B (Rstudio only)


## installs to libPath - not needed in Rstudio envt?
## devtools::install()

## Make binary package for distribution...
#devtools::build(binary=TRUE)
## To install elsewhere from binary
# install.packages("PATHTO/mytest_0.1.tgz",repos=NULL)

# Add data to data folder
devtools::use_data(x)
# Add data to sysdata.R
devtools::use_data(x, internal = TRUE)
