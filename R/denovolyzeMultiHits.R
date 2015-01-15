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
#' @param include.gene Genes to include in analysis. "all" or a vector of gene
#'   names
#' @param expectedDNMs Select whether expected number of multihits is determined
#'   by expected total de novos, or actual total
#' @param gene.id Gene identifier used. Currently only hgnc.id
#' @param include.class variant classes to tabulate in output.  Valid classes =
#'   "syn","mis","non","splice","frameshift","lof","prot","all".
#' @return Returns a data.frame
#'
#' @keywords keywords
#'
#' @export
#'
#' @examples
#' denovolyzeMultiHits(dnm.genes=autismDeNovos$gene,
#'                     dnm.classes=autismDeNovos$dnmClass,
#'                     nsamples=1078)
#'
#' denovolyzeMultiHits(dnm.genes=autismDeNovos$gene,
#'                     dnm.classes=autismDeNovos$dnmClass,
#'                     include.class=c("syn","mis","non","splice","frameshift","lof","prot","prot_dam","all"),
#'                     nsamples=1078)




denovolyzeMultiHits <- function(dnm.genes,dnm.classes,nsamples,
                                nperms=100,
                                include.gene="all",
                                include.class=c("syn","mis","lof","prot","all"),
                                expectedDNMs="actual",
                                gene.id="hgncID",
                                pDNM=denovolyzeR:::pDNM
) {
  # 2 options: the simulation draws N DNMs from the gene list.
  # N could be the actual number of variants seen in the population (case or control), or the expected number (based on DNM model).
  # The former is more conservative.  Samocha et al used the latter.
  # Set expectedDNMs="actual" or "expected

  # Use specified gene ID
  names(pDNM)[names(pDNM)==gene.id] <- "gene"
  pDNM$gene <- toupper(as.character(pDNM$gene))
  include.gene <- toupper(as.character(include.gene))

  # If a list of genes for inclusion is specified, restrict analysis to these genes
  if(include.gene[1]!="ALL"){
    pDNM <- pDNM[pDNM$gene %in% include.gene,]
    excludedgenes <- sum(!dnm.genes %in% include.gene)
    if(excludedgenes > 0) {
      warning("De novo list includes ",excludedgenes," genes not specified for inclusion. These will not be analysed.")
    }
    dnm.classes <- dnm.classes[dnm.genes %in% include.gene]
    dnm.genes <- dnm.genes[dnm.genes %in% include.gene]
  }

  doPermute <- function(class,classgroup=class){
    nextvars <- dnm.genes[dnm.classes %in% classgroup]
    if(expectedDNMs == "actual") {
      x=length(nextvars)
    } else if(expectedDNMs == "expected") {
      x=2*sum(pDNM$value[pDNM$class==class])*nsamples
    }
    y=length(unique(nextvars[duplicated(nextvars)]))
    output <- denovolyzeR:::PermuteMultiHits(x,y,nperms=nperms,class=class,gene.id=gene.id,include.gene=include.gene,pDNM=pDNM)
    #rownames(output) <- class
    return(output)
  }

  ### Calculate probabilities for classes represented in data
  output <- list()
  myclasses=c("syn","mis","mis_dam","mis_cons","mis_svm","startloss",
              "stoploss","non","splice","frameshift","lof")
  myclasses <- myclasses[myclasses %in% dnm.classes]
  for (class in myclasses){
    output[[class]] <- doPermute(class)
  }

  ### Calculate probabilities for "aggregate classes"
  ### LOF (if variants are annotated with increased granularity), protein-altering, and total

  if(!"lof" %in% myclasses){
    output[["lof"]] <- doPermute(class="lof",classgroup=c("non","splice","frameshift","startloss","stoploss"))
  }
  output[["prot"]] <- doPermute(class="prot",classgroup=c("non","splice","frameshift","startloss","stoploss",
                                                          "mis","mis_other","mis_dam","mis_cons","mis_svm"))
  output[["prot_dam"]] <- doPermute(class="prot",classgroup=c("non","splice","frameshift","startloss","stoploss",
                                                              "mis_dam","mis_cons","mis_svm"))
  output[["all"]] <- doPermute(class="all",classgroup=c("non","splice","frameshift","startloss","stoploss",
                                                        "mis_dam","mis_cons","mis_svm","syn"))

  output <- output[names(output) %in% include.class]
  return(t(data.frame(output)))
}
