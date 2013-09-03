# Evelyn Mitchell (c) 2013
# Clustering a group of instruments based on similarity of returns
# 
# Following on jab's request, and modifying Soren's code to use the
# fastcluster library from
# http://math.stanford.edu/~muellner/fastcluster.html
#
# fastcluster is available for both R and python
#
# Posted to r-sig-finance 2013-06-23

require(quantmod)
require(fastcluster)
require(graphics)
symList <- c('MSN','GOOG','YHOO', 'BA', 'SI', 'BP', 'AMD','AMGN.MX')
getSymbols(symList)

# Matrix of daily returns. Or use weekly, monthly returns...
returns.mat <- NULL
for (sym in symList) returns.mat<- cbind( returns.mat,
                                          dailyReturn( Ad(get(sym)) ) )

colnames(returns.mat) <- symList
returns.mat
na.omit(returns.mat)

hc <- hclust(dist(t(na.omit(returns.mat))), "ave")
plot(hc)
plot(hc, hang = -1)
