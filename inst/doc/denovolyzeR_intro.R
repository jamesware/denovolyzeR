## ------------------------------------------------------------------------
# Install the package if you haven't already.
# OPTION 1 - install the latest release from CRAN:
# install.packages("denovolyzeR")  

# OPTION 2 - install the latest development version from GitHub.  Either download and install, or use devtools:
# if(!"devtools" %in% installed.packages()){
#     install.packages("devtools")
# }
# devtools::install_github("jamesware/denovolyzeR")

## ------------------------------------------------------------------------
library(denovolyzeR)
# have a look at the example data:
dim(autismDeNovos)
head(autismDeNovos)

## ------------------------------------------------------------------------
denovolyzeByClass(genes=autismDeNovos$gene,
                  classes=autismDeNovos$class,
                  nsamples=1078)

## ------------------------------------------------------------------------
denovolyzeMultiHits(genes=autismDeNovos$gene,
                    classes=autismDeNovos$class,
                    nsamples=1078)

## ------------------------------------------------------------------------
denovolyzeMultiHits(genes=autismDeNovos$gene,
                    classes=autismDeNovos$class,
                    nsamples=1078,
                    nperms=1000)

## ------------------------------------------------------------------------
sum(autismDeNovos$class %in% c("frameshift","non","splice"))

## ------------------------------------------------------------------------
denovolyzeMultiHits(genes=autismDeNovos$gene,
                    classes=autismDeNovos$class,
                    nsamples=1078,
                    nperms=1000,
                    nVars="expected")

## ------------------------------------------------------------------------
head(
denovolyzeByGene(genes=autismDeNovos$gene,
                 classes=autismDeNovos$class,
                 nsamples=1078)
  )

## ----geneset-------------------------------------------------------------
nrow(fmrpGenes); head(fmrpGenes)

denovolyzeByClass(genes=autismDeNovos$gene,
                  classes=autismDeNovos$class,
                  nsamples=1078,
                  includeGenes=fmrpGenes$geneName)

denovolyzeMultiHits(genes=autismDeNovos$gene,
                    classes=autismDeNovos$class,
                    nsamples=1078,
                    nperms=1000,
                    includeGenes=fmrpGenes$geneName)

## ----viewProbabilityTable------------------------------------------------
head(
  viewProbabilityTable()
  )

head(
  viewProbabilityTable(format="long")
  )

