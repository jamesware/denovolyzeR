#' Evaluates burden of \emph{de novo} variation against expectation
#'
#' Determines whether the test population carry more \emph{de novo} variants
#' than expected.  Results are tabulated by gene.  By default results are
#' returned for loss-of-function variants, and for all protein-altering
#' variants.
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
#' @param include.gene Genes to include in analysis. "all" or a vector of gene
#'   names.
#' @param gene.id Gene identifier used. Currently only hgncID. (refseqID may
#'   work)
#' @param signif.p Number of sig figs used to round p values in output.
#' @param round.expected Number of decimal places used to round expected burdens
#'   in output.
#'
#' @return Returns a data frame
#'
#' @seealso \code{\link{denovolyze}}
#' @seealso \code{\link{denovolyzeByClass}}
#'
#' @export
#'
#' @examples
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

denovolyzeByGene <- function(dnm.genes,dnm.classes,nsamples,
                             group.by="gene",
                             include.gene="all",
                             include.class=c("lof","prot"),
                             gene.id="hgncID",
                             signif.p=3,round.expected=1){
  denovolyze(dnm.genes,dnm.classes,nsamples,group.by,include.gene,include.class,gene.id,signif.p,round.expected)
}
