#' Checks input for errors
#'
#' An internal function to check inputs
#'
#' @inheritParams denovolyze
#'
#' @return tbc   , with or without error message
#'

parseInput <- function(genes=genes,
                       classes=genes,
                       nsamples=nsamples,
                       groupBy=groupBy,
                       includeGenes=includeGenes,
                       includeClasses=includeClasses,
                       geneId=geneId,
                       signifP=signifP,
                       roundExpected=roundExpected){


## check inputs have same length
if(length(genes)!=length(classes)){
  stop('The number of genes (genes) and number of variant consequences (classes) do not match')
}

## checks on genenames:
# capitalisation
genes <- toupper(genes)
# recognised

# no NA
if(sum(is.na(genes))>0){
  stop('gene name can not be NA')
}
#pass capitalised gene names to parent function
assign("genes",genes,pos=sys.frame(sys.parent()))


## checks on variant classes:
# capitalisation
# recognised
# no NA
if(sum(is.na(classes))>0){
  stop('variant class can not be NA')
}
# match to SO?

## checks on nsamples
# is integer
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {abs(x - round(x)) < tol}
if(!is.wholenumber(nsamples)){
  stop('nsamples must be an integer')
}

## checks on groupBy
groupBy <- tolower(groupBy)
if(!groupBy %in% c("gene","class")){
  stop(paste("\"",groupBy,"\" is not a valid groupBy option",sep=""))
}
# passes lower case groupBy to parent function
assign("groupBy",groupBy,pos=sys.frame(sys.parent()))


## checks on includeGenes

## checks on includeClasses

## checks on geneId


## checks on roundExpected

## checks on signifP

}



# TESTS
# denovolyze(genes=autismDeNovos$gene, classes=autismDeNovos$class, nsamples=1078)
# denovolyze(genes=autismDeNovos$gene, classes=autismDeNovos$class[-1], nsamples=1078)
# denovolyze(genes=c("KCNQ1","RYR2",NA), classes=c("lof","lof","lof"), nsamples=10)
# denovolyze(genes=autismDeNovos$gene, classes=autismDeNovos$class, nsamples=1078.3)
# denovolyze(genes=autismDeNovos$gene, classes=autismDeNovos$class, nsamples=1078, groupBy="CLASS")
# denovolyze(genes=autismDeNovos$gene, classes=autismDeNovos$class, nsamples=1078, groupBy="genie")

