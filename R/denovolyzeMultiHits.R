#' Determine significance of genes with multiple \emph{de novos}
#'
#' Are there more genes containing >1 \emph{de novos} than expected?
#'
#' See vignette (denovostats_intro) for more information.
#'
#' @param dnm.genes A vector of genes containing de novo variants
#' @param dnm.classes A vector of classes of de novo variants.  Supported
#'   classes are "syn", "mis", "lof", "prot"
#' @param nsamples Number of individuals considered in de novo analysis#' @param
#'   nperms Number of permutations
#' @param include Genes to include in analysis. "all" or a vector of gene names
#' @param expectedDNMs Select whether expected number of multihits is determined
#'   by expected total de novos, or actual total
#' @param gene.id Gene identifier used. Currently only hgnc.id
#'
#' @return Returns a data.frame
#'
#' @keywords keywords
#'
#' @export
#'
#' @examples
#' CalculateMultiHitBurdens(dnm.genes=caseDeNovos$GeneID,
#' dnm.classes=caseDeNovos$VariantClass,
#' nsamples=1227)

denovolyzeMultiHits <- function(dnm.genes,dnm.classes,nsamples,nperms=100,include.gene="all",expectedDNMs="actual",gene.id="hgncID") {
  # 2 options: the simulation draws N DNMs from the gene list.  N could be the actual number of variants seen in the population (case or control), or the expected number (based on DNM model).  The former is more conservative.  Kaitlin used the latter.
  # Set expectedDNMs="actual" or "expected

  # Use specified gene ID
  names(pDNM)[names(pDNM)==gene.id] <- "gene"
  pDNM$gene <- toupper(as.character(pDNM$gene))
  include.gene <- toupper(as.character(include.gene))

  # If a list of genes for inclusion is specified, restrict analysis to these genes
  if(include.gene[1]!="ALL"){
    pDNM <- pDNM[pDNM$gene %in% include.gene,]
    excludedgenes <- sum(!dnm.genes %in% include.gene)
    if(excludedgenes > 0) {warning("De novo list includes ",excludedgenes," genes not specified for inclusion. These will not be analysed.")}
    dnm.classes <- dnm.classes[dnm.genes %in% include.gene]
    dnm.genes <- dnm.genes[dnm.genes %in% include.gene]
  }

  doPermute <- function(class){
    nextvars <- dnm.genes[dnm.classes %in% c(class)]
    if(expectedDNMs == "actual") {
      x=length(nextvars)
    } else if(expectedDNMs == "expected") {
      x=2*sum(pDNM$value[pDNM$class==class])*nsamples
    }
    y=length(unique(nextvars[duplicated(nextvars)]))
    output <- PermuteMultiHits(x,y,nperms=nperms,class=class,include.gene=include.gene)
    rownames(output) <- class
    return(output)
  }

 output <- list()
  for (dnmClass in unique(dnm.classes)){
    output[[dnmClass]] <- doPermute(dnmClass)
    }

  return(output)
}
