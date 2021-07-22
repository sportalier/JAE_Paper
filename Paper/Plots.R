# Plots for the manuscript
# 06 October 2020
# by Sebastien Portalier

library(lattice)
library(latticeExtra)

#### tree model fitting ####
setwd("C:/Users/sebca/Documents/Master_Uottawa/SBW_models/Fitting")
provbud = read.csv('Budburst_Prov_Error.csv')

mypanel1 = function(x,...){
  panel.histogram(x,...)
  panel.text(-100,3,labels='A',cex=2)
}
p1 = histogram(~Error,data = provbud,col='grey70',xlim=c(-110,110),ylab='Percentage from total',xlab = 'Residuals',
          par.settings=list(axis.text=list(cex=1.2),par.xlab.text=list(cex=2),par.ylab.text=list(cex=1.6),
                            axis.components=list(top=list(tck=0),right=list(tck=0))),
          panel=mypanel1,
          breaks = 50)

mypanel2 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.text(45.4,-77,labels='B',cex=2)
}
p2 = xyplot(provbud$Error~provbud$Latitude,xlab='Latitude',ylab='Residuals',ylim=c(-100,100),col='black',pch=20,
       panel = mypanel2,
       par.settings=list(axis.text=list(cex=1.2),par.xlab.text=list(cex=2),par.ylab.text=list(cex=1.6),
                         axis.components=list(top=list(tck=0),right=list(tck=0)))
       )

setwd('C:/Users/sebca/Documents/Master_Uottawa/SBW_models/Manuscripts/Paper_1')
png('Tree_Model_Fitting.png',height = 480,width = 800)
par(mar=c(5,5,4,2),mfrow=c(1,2))
#zozo = c(p1,p2,layout=c(2,1),merge.legends=F)
print(p1,position=c(0,0,0.5,1),split=c(1,1,1,1),more=T)
print(p2,position=c(0.5,0,1,1),split=c(1,1,1,1),more=F)
dev.off()

#### sensitivity ####
setwd('C:/Users/sebca/Documents/Master_Uottawa/SBW_models/Sensitivity')

prcctree = read.csv('PRCC_Uniforc_Model.csv')

prcc = prcctree[,1]
nam = c(expression(paste(italic('t')[0])),expression(paste(italic('b')[f])), expression(paste(italic('c')[f])),expression(paste(italic('F')*'*')))
x0 = seq(1,4)
y0 = rep(0,4)
x1 = x0
y1 = prcc[c(4,1,2,3)]

prccbudworm = read.csv('PRCC_Budworm_Model.csv')

prccbud = prccbudworm[,1]
nambud = c(expression(paste(italic(beta[1]))),expression(paste(italic(beta[2]))), expression(paste(italic(beta[3]))), expression(paste(italic(beta[4]))),expression(paste(italic('t')[b])),expression(paste(italic('t')[m])))
x0bud = seq(1,6)
y0bud = rep(0,6)
x1bud = x0bud
y1bud = prccbud


mypanel3 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.segments(x0=x0bud,y0=y0bud,x1=x1bud,y1=y1bud,lwd=3)
  panel.abline(h=0,lwd=1.5)
  panel.text(0.8,-0.9,'C',cex = 2)
}
p3 = xyplot(0~0,type='n',xlim=c(0.5,6.5),ylim=c(-1.1,1.1),xlab = 'Parameters',ylab = 'PRCC',#ylab = 'Partial Rank Correlation Coefficient',
       par.settings=list(axis.text=list(cex=1.2),par.xlab.text=list(cex=2),par.ylab.text=list(cex=1.8),
                         axis.components=list(top=list(tck=0),right=list(tck=0))),
       panel = mypanel3,
       scales=list(x=list(at=seq(1,6),labels=c(nambud)),y=list(at=seq(-1,1,by=0.5)))
      )

mypanel4 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.segments(x0=x0,y0=y0,x1=x1,y1=y1,lwd=3)
  panel.abline(h=0,lwd=1.5)
  panel.text(0.7,-0.9,'D',cex = 2)
}
p4 = xyplot(0~0,type='n',xlim=c(0.5,4.5),ylim=c(-1.1,1.1),xlab = 'Parameters',ylab = 'PRCC',#ylab = 'Partial Rank Correlation Coefficient',
       par.settings=list(axis.text=list(cex=1.2),par.xlab.text=list(cex=2),par.ylab.text=list(cex=1.8),
                         axis.components=list(top=list(tck=0),right=list(tck=0))),
       panel = mypanel4,
       scales=list(x=list(at=seq(1,4),labels=c(nam)),y=list(at=seq(-1,1,by=0.5)))
      )

setwd('C:/Users/sebca/Documents/Master_Uottawa/SBW_models/Manuscripts/Paper_1')
png('Sensitivity.png',height = 500,width = 800)
par(mar=c(5,5,4,2))
print(p1, split = c(1,1,2,2), more = T)
print(p2, split = c(2,1,2,2), more = T)
print(p3, split = c(1,2,2,2), more = T)
print(p4, split = c(2,2,2,2), more = F)
dev.off()

