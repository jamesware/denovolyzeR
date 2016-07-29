#to test denovolylzeR line by line
# genes = autismDeNovos$gene
# classes = autismDeNovos$class
# nsamples = 1078
# includeClasses=c("syn","mis","misD",
#                  "non","stoploss","startgain",
#                  "splice","frameshift","lof","prot",
#                  "protD", "all")
# includeGenes="all"

# genes = testData$gene
# classes = testData$class
# nsamples = 100
# includeClasses=c("non","splice","syn","mis","lof")
# includeGenes=c("KCNQ1","GLRA2")
# groupBy="gene"
# # groupBy="class"
#
#
#
# geneId="geneName"
# signifP=3
# roundExpected=1
# probTable=NULL
# misD=NULL
#
#
# #old
# output %>%
#   dplyr::select(-enrichment) %>%
#   reshape::recast(id.var=c("gene","class"), formula = gene ~ variable ~ class)
#
# #new
# output %>%
#   dplyr::select(-enrichment) %>%
#   reshape2::melt() %>%
#   reshape2::dcast(formula = gene ~ class + variable)
