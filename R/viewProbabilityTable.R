#' Displays underlying \emph{de novo} probability tables
#'
#' Tabulates probability of \emph{de novo} variant for each protein-coding variant class, for each gene.  Values are probability of a \emph{de novo} variant per chromosome per generation.  i.e. expected number of de novos for a given gene/class = \eqn{p * 2 * nsamples}.
#' @param format option to display table in wide format (default; one line per gene), or long format
#' @export

# --------------------

viewProbabilityTable <- function(format="wide"){
  if(format=="long"){
    return(pDNM)
  } else if (format=="wide") {
    return(
      reshape::cast(pDNM, hgncID + hgncSymbol + enstID + ensgID + geneName ~ class)
      )
  }
}

# test1 <- reshape::cast(pDNM, hgncID + hgncSymbol + enstID + ensgID + geneName ~ class) %>% arrange(enstID)
# test2 <- reshape2::dcast(pDNM, hgncID + hgncSymbol + enstID + ensgID + geneName ~ class) %>% arrange(enstID)
# head(test1)
# head(test2)
#
# all.equal(test1,test2)
# str(test1)
# str(test2)
# only difference between the 2 functions is loss of "attributes" that do not seem to be relevant
