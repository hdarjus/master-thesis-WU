# data is assumed to be in an xts object called cluster.data

library("parallel")
library("stochvollev")



cl <- makeForkCluster(4)
clusterSetRNGStream(cl, 42)
