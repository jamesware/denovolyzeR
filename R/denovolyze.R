#' Evaluates burden of \emph{de novo} variation against expectation
#'
#' Determines whether the test population carry more \emph{de novo} variants
#' than expected. Variants may be grouped by variant class (e.g. are there more
#' LOF variants than expected, across the whole dataset?), or by gene (are there
#' more variants of a given class in SCN2A?).
#'
#' Analyses can be restricted to a subset of genes, and/or a subset of variant classes
#'
#' See vignette (denovolyeR intro) for more information.
#'
#' @param dnm.genes A vector of genes containing de novo variants.
#' @param dnm.classes A vector of classes of de novo variants.  Supported
#'   classes are "syn", "mis", "non", "splice", "frameshift" and "lof".
#' @param nsamples Number of individuals considered in de novo analysis.
#' @param group.by Results can be tabulated by gene, or by variant class
#' @param include.class Which variant classes are tabulated in output
#' @param include.gene Genes to include in analysis. "all" or a vector of gene names.
#' @param gene.id Gene identifier used. Currently only hgncID. (refseqID may work)
#' @param signif.p Number of sig figs used to round p values in output.
#' @param round.expected Number of decimal places used to round expected burdens
#'   in output.
#'
#' @return Returns a data frame
#'
#' @export denovolyze denovolyzeByClass denovolyzeByGene
#'
#' @import dplyr
#' @importFrom reshape melt
#' @importFrom reshape cast
#' @importFrom reshape recast
#'
#' @examples
#'
#' ### denovolyze
#'
#' denovolyze(dnm.genes=autismDeNovos$gene,
#'            dnm.classes=autismDeNovos$dnmClass,
#'            nsamples=1078)
#'
#' ### denovolyzeByClass
#'
#' denovolyzeByClass(dnm.genes=autismDeNovos$gene,
#'                   dnm.classes=autismDeNovos$dnmClass,
#'                   nsamples=1078)
#'
#' # this convenience function is identical to:
#'
#' denovolyze(dnm.genes=autismDeNovos$gene,
#'            dnm.classes=autismDeNovos$dnmClass,
#'            nsamples=1078,
#'            group.by="class",
#'            include.class=c("syn","mis","lof","prot","all"),
#'            include.gene="all"
#'            )
#'
#' ### denovolyzeByGene
#'
#' denovolyzeByGene(dnm.genes=autismDeNovos$gene,
#'                  dnm.classes=autismDeNovos$dnmClass,
#'                  nsamples=1078)
#'
#' # this is identical to:
#'
#' denovolyze(dnm.genes=autismDeNovos$gene,
#'            dnm.classes=autismDeNovos$dnmClass,
#'            nsamples=1078,
#'            group.by="gene",
#'            include.class=c("lof","prot"),
#'            include.gene="all"
#'            )
#'



denovolyze <- function(dnm.genes,dnm.classes,nsamples,
                       group.by="class",include.gene="all",
                       include.class=c("syn","mis","non","splice","frameshift","lof","prot","all"),
                       gene.id="hgncID",signif.p=3,round.expected=1){

  # this line defines variables in order to pass R CMD check
  # these are column names used in dplyr::select(x) statement, but R CMD CHECK interprets them as global variables without visible binding
  gene <- value <- enrichment <- Row.names <- ends_with <- NULL

  # check inputs
  parseInput(dnm.genes,
  dnm.classes,
  nsamples,
  group.by,
  include.gene,
  include.class,
  gene.id,
  signif.p,
  round.expected)

  # Use specified gene ID
  names(pDNM)[names(pDNM)==gene.id] <- "gene"
  if(toupper(include.gene[1])=="ALL" & length(include.gene==1)){include.gene <- toupper(pDNM$gene)}

  # Use uppercase when matching gene symbols
  pDNM$gene <- toupper(as.character(pDNM$gene))
  include.gene <- toupper(as.character(include.gene))
  dnm.genes <- toupper(as.character(dnm.genes))
  include.class <- tolower(as.character(include.class))

  # annotate lof & prot variant classes
  input <- data.frame(gene=dnm.genes,class=dnm.classes)
  input$class.1[input$class %in% c("splice","frameshift","non")] <- "lof"
  input$class.2[input$class %in% c("splice","frameshift","non","lof","mis")] <- "prot"
  input$class.3 <- "all"
  input <- melt(input,id.vars="gene") %>% select(gene, class = value)  %>% filter(!is.na(class))

  # tabulate observed & expected numbers, either by gene or by class
  if(group.by=="class"){
    observed <-
      input %>%
      filter(gene %in% include.gene, class %in% include.class) %>%
      group_by(class) %>%
      summarise(
        observed = n()
      )
  observed$class <- factor(observed$class, levels=c(c("syn","mis","non","splice","frameshift","lof","prot","all")))
  observed <- observed[order(observed$class),]

  expected <- pDNM %>%
    filter(gene %in% include.gene, class %in% include.class) %>%
    group_by(class) %>%
    summarise(
      expected = 2*sum(value, na.rm=T)*nsamples
    )

  output <- left_join(observed,expected,by=c("class"))

  } else if(group.by=="gene"){

    observed <-
      input %>%
      filter(gene %in% include.gene, class %in% include.class) %>%
      group_by(gene,class) %>%
      summarise(
        observed = n()
      )

    expected <- pDNM %>%
      filter(gene %in% include.gene, class %in% include.class) %>%
      group_by(gene,class) %>%
      summarise(
        expected = 2*sum(value, na.rm=T)*nsamples
      )

    output <- merge(observed,expected,by=c("gene","class"),all=T)
  }
  #include.class <- c("lof","prot")

  # calculate poisson stats and enrichments
  output[is.na(output)] <- 0
  output$enrichment <- signif(output$observed/output$expected,signif.p)
  output$p.value <- signif(ppois(output$observed-1,lambda=output$expected,lower.tail=F),signif.p)
  output$expected <- round(output$expected,round.expected)

  # when analysing by gene, apply additional formatting to arrange results for different variant classes side by side
  if(group.by=="gene"){

    output <-
      output %>%
      select(-enrichment) %>%
      recast(id.var=c("gene","class"), formula = gene ~ variable ~ class)

    output2 <- list()
    for (i in seq(along=include.class)){
      output2[[i]] <- as.data.frame(output[,,i])
      names(output2[[i]]) <- paste(include.class[i],names(output2[[i]]),sep=".")
    }

    output3 <- output2[[1]]
    for (i in seq(along=include.class)[-1]){
      output3 <- merge(output3,output2[[i]],by="row.names",all=T)
      rownames(output3) <- output3$Row.names
      output3 <- output3 %>% select(-Row.names)
    }

    my.index <- output3 %>% select(ends_with("p.value")) %>% apply(MARGIN=1,min, na.rm=T) %>% order()
    output <- output3[my.index,]
  }

  class(output) <- "data.frame"
  return(output)
}

#' @describeIn denovolyze denovolyzeByClass

denovolyzeByClass <- function(dnm.genes,dnm.classes,nsamples,
                              group.by="class",
                              include.gene="all",
                              include.class=c("syn","mis","lof","prot","all"),
                              gene.id="hgncID",
                              signif.p=3,round.expected=1){
  denovolyze(dnm.genes,dnm.classes,nsamples,group.by,include.gene,include.class,gene.id,signif.p,round.expected)
}

#' @describeIn denovolyze denovolyzeByGene

denovolyzeByGene <- function(dnm.genes,dnm.classes,nsamples,
group.by="gene",
include.gene="all",
include.class=c("lof","prot"),
gene.id="hgncID",
signif.p=3,round.expected=1){
  denovolyze(dnm.genes,dnm.classes,nsamples,group.by,include.gene,include.class,gene.id,signif.p,round.expected)
}

