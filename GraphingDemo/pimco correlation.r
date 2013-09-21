#do a pretty correlation on heat map on all Pimco Funds (institutional share class) that have existed longer 5 years
#symbol list obtained from
#http://investments.pimco.com/Products/pages/PlOEF.aspx?Level1=ulProducts&Center=ulProducts&Level2=liulProductsMutualFunds
#pasted into Excel and sorted by 5 yr return
#then copied and pasted transpose
#saved to csv
#and pasted in this ticker list eliminating the money fund and adding Vanguard S&P 500 for reference
tckrs <- c("PISIX","PSKIX","PSDIX","PSTKX","PCRIX","PFIIX","PHMIX","PFCIX","PCDIX","PTSHX","PFMIX","PLMIX","PSPTX","PCIMX","PSTIX","PNYIX","PLDTX","PLDIX","PTLDX","PAAIX","PXTIX","PHIYX","PSCSX","PAUIX","PTRIX","PGBIX","PFORX","PELBX","PDMIX","PMDRX","PEBIX","PDIIX","PRRSX","PMBIX","PTSAX","PTTRX","PIGLX","PRRIX","PFUIX","PIMIX","PIGIX","PRAIX","PLRIX","PGOVX","PEDIX","VFINX")

for (i in 1:length(tckrs)) {
  ifelse (i == 1,
          pimco <- get(getSymbols(tckrs[i],from="2000-01-01",adjust=TRUE))[,4],
          pimco <- merge(pimco,get(getSymbols(tckrs[i],get="all",from="2000-01-01",adjust=TRUE))[,4]))
}
#remove .close from each of the symbols
colnames(pimco) <- tckrs
pimco.clean <- na.omit(pimco)
pimco.roc <- ROC(pimco.clean,n=1,type="discrete")
pimco.roc[1,] <- 0

#do correlation table
ca <- cor(pimco.roc)

#get colors to use for heat map
brew <- brewer.pal(name="RdBu",n=5)
#get color ramp
cc.brew <- colorRampPalette(brew)
#apply color ramp
cc <- cc.brew(nrow(ca))
#do heatmap and sort by degree of correlation to VFINX (Vanguard S&P 500)
heatmap(ca[order(ca[,ncol(ca)]),order(ca[,ncol(ca)])],symm=TRUE,Rowv=NA,Colv=NA,col=cc,RowSideColors=cc,main="")
title(main="Correlation Table (Ordered by Correlation with Vanguard S&P 500-VFINX)",font.main=1,outer=TRUE,line=-1,cex.main=1.3)

heatmap(ca[order(ca[,ncol(ca)]),order(ca[,ncol(ca)])],symm=TRUE,col=cc,RowSideColors=cc,main="")
title(main="Correlation Table (Ordered by Dendrogram)",font.main=1,outer=TRUE,line=-1,cex.main=1.3)


#do colors based on correlation but with gray so visible when labelling
cc.palette <- colorRampPalette(c(cc[1],"gray60",cc[length(cc)]))
cc.levpalette <- cc.palette(nrow(ca))
cc.levels <- level.colors(ca[order(ca[,ncol(ca)-1]),ncol(ca)-1], at = do.breaks(c(-1,1),nrow(ca)),
                          col.regions = cc.levpalette)
dotchart(ca[order(ca[,ncol(ca)]),ncol(ca)],col=cc.levels,pch=19,cex=0.75)
title(main="Correlation to Vanguard S&P 500 (VFINX)",font.main=1,outer=TRUE,line=-1,cex.main=1.3)