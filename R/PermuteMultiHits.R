#' Permutes x variants across a genelist, and counts genes with multiple hits
#'
#' An internal function called by denovolyzeMultiHits
#'
#' @inheritParams denovolyze
#' @param x Total number of de novo variants observed in dataset
#' @param y Number of genes with >1 de novo variant (of class "class") in the population
#' @param nperms Number permutations
#' @param class In c("lof","mis","syn","prot")
#'
#' @seealso \code{\link{denovolyzeMultiHits}}
#'
#' @return Returns a named vector of 5 values
#'

PermuteMultiHits <- function(x,y,nperms=100,
                             class="lof",
                             gene.id="hgncID",
                             include.gene="all",
                             pDNM=denovolyzer:::pDNM) {

  #x = total number of DNM observed
  #y = no of genes with >1 DNM in class of interest
  #nperms = number permutations, defaults to 100
  #class = type of DNM assessed, defaults to "lof"

  # Use specified gene ID
  names(pDNM)[names(pDNM)==gene.id] <- "gene"
  pDNM$gene <- toupper(as.character(pDNM$gene))
  include.gene <- toupper(as.character(include.gene))

  # If a list of genes for inclusion is specified, restrict analysis to these genes
  if(include.gene[1]!="ALL"){
    pDNM <- pDNM[pDNM$gene %in% include.gene,]
  }
  probtable <- pDNM[pDNM$class==class,c("gene","value")]
  mycounts<- NA
  for (i in 1:nperms) {
    DNMsim <- sample(probtable$gene,x,replace=T,prob=probtable$value)
    mycounts[i] <- length(unique(DNMsim[duplicated(DNMsim)]))
  }
  empirical.p<-length(which(mycounts >= y))/nperms
  #output <- data.frame(matrix(nrow=1,ncol=5))
  output <- vector(length=5)
  output <-c(y,mean(mycounts),max(mycounts),empirical.p,x)
  names(output) <- c("ObsGenes","AvgExpGenes","MaxExpGenes","Empirical.P","TotalObsDNM")
  return(output)

}
