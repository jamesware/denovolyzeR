#' Displays underlying \emph{de novo} probability tables
#'
#' Tabulates probability of \emph{de novo} variant for each protein-coding variant class, for each gene.  Values are probability of a \emph{de novo} variant per chromosome per generation.  i.e. expected number of de novos for a given gene/class = \eqn{p * 2 * nsamples}.
#'
#' @export

viewDeNovoProbs <- function(long=TRUE){
  if(long){
    return(pDNM)
  } else {
    return(cast(pDNM, refseqID + hgncID ~ class))
  }
}
