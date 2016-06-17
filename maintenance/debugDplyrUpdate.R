# email from Hadley Wickham reporting intended update of dplyr
# R CMD check on denovolyzeR had noted a problem - the vignette was throwing an error
# installed development version of dplyr to ensure compatibility
# error arised from internal mismatch in column names ("observed" inconsistently abbreviated as "obs")

withr::with_libpaths(new="../Library.ArchiveVersions/",install.packages("dplyr"))

install.packages("Rcpp")
install.packages("tibble",type="both")
devtools::install_github("hadley/dplyr")

library(dplyr)
packageVersion("dplyr")

head(autismDeNovos)

denovolyzeByClass(genes=autismDeNovos$gene,
                  classes=autismDeNovos$class,
                  nsamples=1078)
