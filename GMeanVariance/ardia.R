#' Conditional Value At Risk portfolio allocation example
#' 
#'
#' This is a demonstration of using DEOptim, a package for Differential Evolution Optimization,
#' to optimize the wights of a group of ETFs to satisfy several different criteria 
#' (Minimize Robust conditional Value at Risk cVaR [MinCVaR_Rob], minimize conditional VaR covariance 
#' on daily returns[ MinCVaR_cov_daily], minimize Standard Deviation covariance [MinSD_cov], and 
#' Maximize Sharpe Ratio covariance [MaxSR_cov])
#' 
#' Inputs: A list of ETF symbols, and the benchmark Symbol
#' 
#' 
#' TODO: walkforward testing, to check for accidental prediciton with next period returns (HT Chris McDonald)

# debugging
#options(warn=2)
#options(error=dump.frames)

library(PerformanceAnalytics)
library(DEoptim)
library(quantmod)
library(robust)
library(doParallel)

# make a Cluster of 8 processors to run the optimization in parallel

cl <- makeCluster(8)
registerDoParallel(cl)

#
etflist <- c("TLT","SPY","XLB","XLV","XLP","XLY","XLE","XLF","XLI","XLK","XLU")
getSymbols(etflist,from="2003-12-01", to = "2013-05-01")
benchlist <- "SPY"
tickers <- etflist 
outfile <- "out.txt"
## Make matrix of Returns code stolen from vignette of deoptim
P <- NULL
seltickers <- NULL
write('By Month \n', file=outfile)
for(ticker in tickers){
  tmp <- Ad(to.monthly(eval(parse(text=ticker))))
  wrtline <- paste("ticker ", ticker, "tmp", tmp, "\n")
  write(wrtline, file=outfile)
  if(is.null(P)){ timeP <- time(tmp) }
  if(any(time(tmp)!=timeP)) next
  else P <- cbind(P,as.numeric(tmp))
  wrtline <- paste("ticker ", ticker, "date ", timeP, " tmp", tmp, "\n")
  write(wrtline, file=outfile)
  seltickers <- c(seltickers,ticker)
}

P <- xts(P,order.by=timeP)
colnames(P) <- seltickers
#
# TODO: use simple returns, not log
#
write('P', file=outfile)
write(P, file=outfile)
R <- diff(log(P))
write("R log returns of P", file=outfile)
write(R, file=outfile)
#head(R)
#chart.CumReturns(P,legend.loc="topleft", main="Cumulative Daily Returns")
#chart.CumReturns(TLT$TLT.Adjusted,legend.loc="topleft", main="Cumulative Daily Returns")
R <- R[-1,]
returnsDiscrete <- CalculateReturns(P, method='discrete')
write("Discrete returns", file=outfile)
write(returnsDiscrete, file=outfile)
returnsLog <- CalculateReturns(P, method='log')
write("Log returns", file=outfile)
write(returnsLog, file=outfile)

#head(returnsLog)
#head(returnsDiscrete)


numperiods = nrow(P)
numreturns = nrow(R)
numinstruments = ncol(P)

#chart.CumReturns(R, legend.loc="topleft", 
                 main="Cumulative Monthly Returns",
                 colorset=rich10equal )
#chart.Boxplot(R)

## Make Matrix of daily returns
Pday <- NULL
seltickers <- NULL
for(ticker in tickers){
  tmp <- Ad(to.daily(eval(parse(text=ticker))))
  if(is.null(Pday)){ timeP <- time(tmp) }
  if(any(time(tmp)!=timeP)) next
  else Pday <- cbind(Pday,as.numeric(tmp))
  seltickers <- c(seltickers,ticker)
}
Pday <- xts(Pday,order.by=timeP)
colnames(Pday) <- seltickers
#
# TODO: use simple returns, not log
#
Rday <- diff(log(Pday))
Rday <- Rday[-1,]

#Benchmarks
P2 <- NULL
seltickers <- NULL
for(ticker in benchlist){
  tmp <- Ad(to.monthly(eval(parse(text=ticker))))
  if(is.null(P2)){ timeP <- time(tmp) }
  if(any(time(tmp)!=timeP)) next
  else P2 <- cbind(P2,as.numeric(tmp))
  seltickers <- c(seltickers,ticker)
}
P2 <- xts(P2,order.by=timeP)
colnames(P2) <- seltickers
#
# TODO: use simple returns, not log
#
R2 <- diff(log(P2))
R2 <- R2[-1,]

initw <- rep(1/ncol(R),ncol(R))

objectivefunsd <- function(w){
  if(sum(w)==0){
    w <- w + 1e-2 
  }
  w <- w / sum(w)
  targ <- StdDev(R=rollR, weights=w, portfolio_method="component", sigma=sigma)
  out <- targ$StdDev
  return(out)
}
objectivefunsr <- function(w){
  if(sum(w)==0){
    w <- w + 1e-2 
  }
  w <- w / sum(w)
  targ =  (t(w)%*%mu) /(t(w)%*%sigma%*%w)
  rtarg <- -targ
  return(rtarg)
}

source("C:/R-Package/returnanalytics/pkg/PortfolioAnalytics/R/random_portfolios.R")
source("C:/R-Package/returnanalytics/pkg/PortfolioAnalytics/R/constraints.R")

#N <- ncol(R)
N <- numinstruments
minw <- 0
maxw <- 1
lower <- rep(minw,N)
upper <- rep(maxw,N)

eps <- 0.025

# set by to .001 for more precision
weight_seq <- generatesequence(min=minw,max=maxw,by=.001,rounding=3)
rpconstraint <- constraint( assets=N, min_sum=1-eps, max_sum=1+eps, min=lower, max=upper, weight_seq=weight_seq)

rp <- random_portfolios(rpconstraints=rpconstraint,permutations=N*10)
rp <- rp/rowSums(rp)
controlDE <- list(reltol=0.00001, steptol=150, itermax=2000, 
                  trace=250, NP=as.numeric(nrow(rp)),
                  initialpop=rp,strategy=6, c=0)
# Remove the set.seed() line to change the RNG start. 
# It is set to replicate results for testing.
set.seed(1234)

preturn <- R

for (p in 1:numinstruments){
  preturn[,p] <- 0
}
optweights <- R
cnames <- colnames(preturn)
zeros <- c(0)

for ( i in 2:numinstruments){
  
  zeros <- cbind(zeros, 0) 
} # add a zero row for the current month to preturn
colnames(zeros) <- cnames
nextdate <- tail(time(optweights)+1/frequency(optweights), n=1)
newrow <- as.xts(zeros, order.by=c(nextdate))
optweights <- rbind(R, newrow) # add a next period 0 row
preturn <- rbind(preturn, newrow)

for (z in 1:numinstruments){
  optweights[,z] <- 0
}



##Max Sharpe With implied E(R)




for ( i in 2:numreturns){
  rollR <- first(R, i)
  #mu <- colMeans(rollR)
  sigma <- cov(rollR)
  weightvec <- DEoptim(fn=objectivefunsd, lower=lower,
                       upper=upper, control=controlDE)
  
  ## Use Opt Weights and Sigma to get E(R)
  sigmaw = sigma%*%weightvec$optim$bestmem
  bhat = max(sum((sigmaw - mean(sigmaw))*t((rollR[i]-mean(rollR[i])))) / sum((sigmaw - mean(sigmaw))^2),0)
  ahat = mean(rollR[i]) - bhat * mean(sigmaw)
  
  mu = ahat+(bhat*sigmaw)
  
  weightvec <- DEoptim(fn=objectivefunsr, lower=lower,
                       upper=upper, control=controlDE)
  
  preturn[i+1,] <- weightvec$optim$bestmem*R[i]
  optweights[i+1,] <- weightvec$optim$bestmem
}

optweights2 <- optweights/rowSums(optweights)
portreturn_maxsr_cov <- optweights2*R
portreturn_maxsr_cov <- rowSums(portreturn_maxsr_cov)
portreturn_maxsr_cov <- xts(portreturn_maxsr_cov, order.by=index(R))

colnames(portreturn_maxsr_cov) <- "MaxSR_cov"
OOSweights_maxsr_cov <- weightvec$optim$bestmem/sum(weightvec$optim$bestmem)

blob=merge.xts(portreturn_maxsr_cov,R2)





chart.CumReturns(blob, wealth.index=TRUE, legend.loc="topleft")
