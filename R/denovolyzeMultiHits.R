#' Determine significance of genes with multiple \emph{de novos}
#'
#' Are there more genes containing >1 \emph{de novos} than expected?
#'
#' See vignette (denovostats_intro) for more information.
#'
#' @inheritParams denovolyze
#'
#' @param nperms Number of permutations
#' @param expectedDNMs Select whether expected number of multihits is determined
#'   by expected total de novos, or actual total
#'
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



denovolyzeMultiHits <- function(dnm.genes,dnm.classes,nsamples,
                                nperms=100,
                                include.gene="all",
                                include.class=c("syn","mis","lof","prot","all"),
                                expectedDNMs="actual",
                                gene.id="hgncID",
                                probTable=NULL,
                                mis_filter=NULL) {

  options(stringsAsFactors = FALSE)
  # 2 options: the simulation draws N DNMs from the gene list.
  # N could be the actual number of variants seen in the population (case or control), or the expected number (based on DNM model).
  # The former is more conservative.  Samocha et al used the latter.
  # Set expectedDNMs="actual" or "expected

  if(is.null(probTable)){probTable <- pDNM}
  if(!is.null(mis_filter)){names(probTable)[names(probTable)==mis_filter] <- "mis_filter"}

  # Record all variant classes in probability table.  Only calculate stats for annotated variant classes.
  allVariantClasses <- probTable %>% select(class) %>% unique %>% unlist

  # Use specified gene ID
  names(probTable)[names(probTable)==gene.id] <- "gene"
  probTable$gene <- toupper(as.character(probTable$gene))
  include.gene <- toupper(as.character(include.gene))

  # If a list of genes for inclusion is specified, restrict analysis to these genes
  if(include.gene[1]!="ALL"){
    probTable <- probTable[probTable$gene %in% include.gene,]
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
      x=2*sum(probTable$value[probTable$class==class])*nsamples
    }
    y=length(unique(nextvars[duplicated(nextvars)]))
    output <- PermuteMultiHits(x,y,nperms=nperms,class=class,gene.id=gene.id,include.gene=include.gene,probTable=probTable)
    #rownames(output) <- class
    return(output)
  }

  ### Calculate probabilities for non-overlapping classes represented in data
  output <- list()
  myclasses=c("syn","mis_filter","startloss",
              "stoploss","non","splice","frameshift")
  myclasses <- myclasses[myclasses %in% dnm.classes]
  myclasses <- myclasses[myclasses %in% allVariantClasses]
  for (class in myclasses){
    output[[class]] <- doPermute(class)
  }

  ### Calculate probabilities for "aggregate classes"
  output[["mis"]] <- doPermute(class="mis",classgroup=c("mis","mis_filter"))

  output[["lof"]] <- doPermute(class="lof",classgroup=c("non","splice","frameshift","startloss","stoploss","lof"))

  output[["prot"]] <- doPermute(class="prot",classgroup=c("non","splice","frameshift","startloss","stoploss","lof",
                                                          "mis","mis_filter"))
  if("prot_dam" %in% allVariantClasses){
    output[["prot_dam"]] <- doPermute(class="prot_dam",classgroup=c("non","splice","frameshift","startloss","stoploss","lof",
                                                              "mis_filter"))
    }
  output[["all"]] <- doPermute(class="all",classgroup=c("non","splice","frameshift","startloss","stoploss","lof",
                                                        "mis","mis_filter","syn"))

  output <- output[names(output) %in% include.class]
  return(t(data.frame(output)))
}
