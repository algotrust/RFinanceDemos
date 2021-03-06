#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012, revised April 2013
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Paragraph 3.3: luxor SMA paramset optimization

###

source('luxor.include.R')
source('luxor.getSymbols.R')

### blotter

initPortf(portfolio.st, symbols='GBPUSD', initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD')

### quantstrat

initOrders(portfolio.st, initDate=initDate)

load.strategy(strategy.st)

### doMC

require(doMC)
registerDoMC(cores=8)

#require(doParallel)
#registerDoParallel(cores=2)

#require(doRedis)
#registerDoRedis('jobs')

results <- apply.paramset(strategy.st, paramset.label='SMA', portfolio.st=portfolio.st, account.st=account.st, nsamples=.nsamples, verbose=TRUE)

###

stats <- results$tradeStats

print(stats)

save(stats, file='luxor.3.paramset.SMA.RData')

