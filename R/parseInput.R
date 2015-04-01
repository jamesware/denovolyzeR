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
                       group.by=group.by,
                       include.gene=include.gene,
                       include.class=include.class,
                       gene.id=gene.id,
                       signif.p=signif.p,
                       round.expected=round.expected){


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

## checks on group.by
group.by <- tolower(group.by)
if(!group.by %in% c("gene","class")){
  stop(paste("\"",group.by,"\" is not a valid group.by option",sep=""))
}
# passes lower case group.by to parent function
assign("group.by",group.by,pos=sys.frame(sys.parent()))


## checks on include.gene

## checks on include.class

## checks on gene.id


## checks on round.expected

## checks on signif.p

}



# TESTS
# denovolyze(genes=autismDeNovos$gene, classes=autismDeNovos$dnmClass, nsamples=1078)
# denovolyze(genes=autismDeNovos$gene, classes=autismDeNovos$dnmClass[-1], nsamples=1078)
# denovolyze(genes=c("KCNQ1","RYR2",NA), classes=c("lof","lof","lof"), nsamples=10)
# denovolyze(genes=autismDeNovos$gene, classes=autismDeNovos$dnmClass, nsamples=1078.3)
# denovolyze(genes=autismDeNovos$gene, classes=autismDeNovos$dnmClass, nsamples=1078, group.by="CLASS")
# denovolyze(genes=autismDeNovos$gene, classes=autismDeNovos$dnmClass, nsamples=1078, group.by="genie")

