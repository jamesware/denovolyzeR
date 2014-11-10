#' Evaluates burden of \emph{de novo} variation against expectation
#'
#' Determines whether the test population carry more \emph{de novo} variants
#' than expected.  Results are tabulated by variant class.
#'
#'
#' See also denovolyze, and vignette (denovolyeR intro) for more information.
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
#' @export
#'
#' @examples
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



denovolyzeByClass <- function(dnm.genes,dnm.classes,nsamples,
                              group.by="class",
                              include.gene="all",
                              include.class=c("syn","mis","lof","prot","all"),
                              gene.id="hgncID",
                              signif.p=3,round.expected=1){
  denovolyze(dnm.genes,dnm.classes,nsamples,group.by,include.gene,include.class,gene.id,signif.p,round.expected)
}

