#### plots ####
library(lattice)
library(latticeExtra)

setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Predictions")

rcp26 = read.csv('Predicted_RCP_26.csv',colClasses = c('Site'='factor'))
rcp45 = read.csv('Predicted_RCP_45.csv',colClasses = c('Site'='factor'))
rcp85 = read.csv('Predicted_RCP_85.csv',colClasses = c('Site'='factor'))
model26 = rep('RCP26',nrow(rcp26))
rcp26$Model = model26
model45 = rep('RCP45',nrow(rcp45))
rcp45$Model = model45
model85 = rep('RCP85',nrow(rcp85))
rcp85$Model = model85

total = rbind(rcp26,rcp45,rcp85)
rm(rcp26,rcp45,rcp85,model26,model45,model85)

setwd("~/Master_Uottawa/SBW_models/Budworm_data")
past = read.csv('Results_Past_BioSim.csv')
Site = rep(0,nrow(past))
for (i in 1:nrow(past)){
  if (past$Site[i]=='Site1'){
    Site[i]=1
  }
  if (past$Site[i]=='Site2'){
    Site[i]=2
  }
  if (past$Site[i]=='Site3'){
    Site[i]=3
  }
  if (past$Site[i]=='Site4'){
    Site[i]=4
  }
  if (past$Site[i]=='Site5'){
    Site[i]=5
  }
  if (past$Site[i]=='Site6'){
    Site[i]=6
  }
}
Modelpast = rep('rcp0',nrow(past))
past$Site=as.factor(Site)
past$Model=Modelpast

total = rbind(past,total)
rm(past)

mycolors <- grey.colors(3, start = 0.9, end = 0.3)
trellis.par.set(superpose.symbol = list(fill = mycolors,col=mycolors))

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")

png('Budburst_Future.png',width = 800,height = 480)
mycolors <- grey.colors(4, start = 0.9, end = 0.3)
#mycolors = c('blue','pink','red','darkred')
trellis.par.set(superpose.symbol = list(fill = mycolors,col=mycolors))
bwplot(Budburst~Site, data = total, groups = Model,
       xlab = 'Latitude',ylab = 'Budburst date (Julian days)',
       pch = "|", box.width = 1/6,
       panel = panel.superpose,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=3),par.ylab.text=list(cex=2.7),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1),labels=c('44.5','45.5','46.5','47.5','48.5','49.5'),
                          alternating=1))
)
dev.off()

png('Emergence_Future.png',width = 800,height = 480)
mycolors <- grey.colors(4, start = 0.9, end = 0.3)
#mycolors = c('blue','pink','red','darkred')
trellis.par.set(superpose.symbol = list(fill = mycolors,col=mycolors))
bwplot(Emergence~Site, data = total, groups = Model,
       xlab = 'Latitude',ylab = 'Emergence date (Julian days)',
       pch = "|", box.width = 1/6,
       panel = panel.superpose,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=3),par.ylab.text=list(cex=2.7),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1),labels=c('44.5','45.5','46.5','47.5','48.5','49.5'),
                          alternating=1))
)
dev.off()

png('Mismatch_Future.png',width = 800,height = 480)
mycolors <- grey.colors(4, start = 0.9, end = 0.3)
#mycolors = c('blue','pink','red','darkred')
trellis.par.set(superpose.symbol = list(fill = mycolors,col=mycolors))
bwplot(Mismatch~Site, data = total, groups = Model,
       xlab = 'Latitude',ylab = 'Mismatch (days)',
       pch = "|", box.width = 1/6,
       panel = panel.superpose,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=3),par.ylab.text=list(cex=2.7),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1),labels=c('44.5','45.5','46.5','47.5','48.5','49.5'),
                          alternating=1))
)
dev.off()

#### 4 hourly data ####
setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Predictions")
past = read.csv('4_hourly_Past_Insect.csv',colClasses = c('Site'='factor'))

rcp26 = read.csv('4_hourly_Predicted_RCP_26_Insect.csv',colClasses = c('Site'='factor'))
rcp45 = read.csv('4_hourly_Predicted_RCP_45_Insect.csv',colClasses = c('Site'='factor'))
rcp85 = read.csv('4_hourly_Predicted_RCP_85_Insect.csv',colClasses = c('Site'='factor'))
model26 = rep('RCP26',nrow(rcp26))
rcp26$Model = model26
model45 = rep('RCP45',nrow(rcp45))
rcp45$Model = model45
model85 = rep('RCP85',nrow(rcp85))
rcp85$Model = model85

total = rbind(rcp26,rcp45,rcp85)
rm(rcp26,rcp45,rcp85,model26,model45,model85)
Modelpast = rep('rcp0',nrow(past))
past$Model=Modelpast

total = rbind(past,total)
rm(past,Modelpast)

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")

png('Emergence_Future_4_hourly.png',width = 800,height = 480)
#mycolors <- grey.colors(4, start = 0.9, end = 0.3)
mycolors = c('blue','pink','red','darkred')
trellis.par.set(superpose.symbol = list(fill = mycolors,col=mycolors))
bwplot(Emergence~Site, data = total, groups = Model,
       xlab = 'Latitude',ylab = 'Emergence date (Julian days)',
       pch = "|", box.width = 1/6,
       panel = panel.superpose,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=3),par.ylab.text=list(cex=2.7),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1),labels=c('44.5','45.5','46.5','47.5','48.5','49.5'),
                          alternating=1))
)
dev.off()

png('Budburst_Future_4_hourly.png',width = 800,height = 480)
#mycolors <- grey.colors(4, start = 0.9, end = 0.3)
mycolors = c('blue','pink','red','darkred')
trellis.par.set(superpose.symbol = list(fill = mycolors,col=mycolors))
bwplot(Budburst~Site, data = total, groups = Model,
       xlab = 'Latitude',ylab = 'Budburst date (Julian days)',
       pch = "|", box.width = 1/6,
       panel = panel.superpose,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=3),par.ylab.text=list(cex=2.7),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1),labels=c('44.5','45.5','46.5','47.5','48.5','49.5'),
                          alternating=1))
)
dev.off()


png('Mismatch_Future_Full_4_hourly.png',width = 800,height = 480)
#mycolors <- grey.colors(4, start = 0.9, end = 0.3)
mycolors = c('blue','pink','red','darkred')
trellis.par.set(superpose.symbol = list(fill = mycolors,col=mycolors))
bwplot(Mismatch~Site, data = total, groups = Model,
       xlab = 'Latitude',ylab = 'Mismatch (days)',
       pch = "|", box.width = 1/6,
       panel = panel.superpose,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=3),par.ylab.text=list(cex=2.7),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1),labels=c('44.5','45.5','46.5','47.5','48.5','49.5'),
                          alternating=1))
)
dev.off()


# mycolors2 <- grey.colors(4, start = 0.9, end = 0.3)
# mycolors <- grey.colors(3, start = 0.9, end = 0.3)
mycolors = c('white',"#C3C3C3","#969696","#4D4D4D")
mypanel1 = function(x,y,...){
  panel.superpose(x,y,...)
  panel.text(0.7,103,'A',cex = 1.5)
  panel.text(3.9,158,'*',cex = 1.7,col='grey50')
  panel.text(4.9,158,'*',cex = 1.7,col='grey50')
  panel.text(5.9,158,'*',cex = 1.7,col='grey50')
  panel.text(2.25,158,'*',cex = 1.7)
  panel.text(3.25,158,'*',cex = 1.7)
  panel.text(4.25,158,'*',cex = 1.7)
  panel.text(5.25,158,'*',cex = 1.7)
  panel.text(6.25,158,'*',cex = 1.7)
}
p1 = bwplot(Emergence~Site, data = total, groups = Model,ylim=c(95,162),
       xlab = 'Latitude',ylab = 'Emergence date',
       pch = "|", box.width = 1/6,
       panel = mypanel1,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=2),par.ylab.text=list(cex=1.8),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2),
                         superpose.symbol = list(fill = mycolors,col=mycolors)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1),labels=c('44.5','45.5','46.5','47.5','48.5','49.5'),
                          alternating=1))
      )

mypanel2 = function(x,y,...){
  panel.superpose(x,y,...)
  panel.text(0.7,125,'B',cex = 1.5)
  panel.text(3.9,156,'*',cex = 1.7,col='grey50')
  panel.text(4.9,156,'*',cex = 1.7,col='grey50')
  panel.text(5.9,156,'*',cex = 1.7,col='grey50')
  panel.text(2.25,156,'*',cex = 1.7)
  panel.text(3.25,156,'*',cex = 1.7)
  panel.text(4.25,156,'*',cex = 1.7)
  panel.text(5.25,156,'*',cex = 1.7)
  panel.text(6.25,156,'*',cex = 1.7)
}
p2 = bwplot(Budburst~Site, data = total, groups = Model,ylim=c(120,159),
       xlab = 'Latitude',ylab = 'Budburst date',
       pch = "|", box.width = 1/6,
       panel = mypanel2,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=2),par.ylab.text=list(cex=1.8),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2),
                         superpose.symbol = list(fill = mycolors,col=mycolors)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1),labels=c('44.5','45.5','46.5','47.5','48.5','49.5'),
                          alternating=1))
      )


mypanel3 = function(x,y,...){
  panel.superpose(x,y,...)
  panel.text(0.7,-25,'C',cex = 1.5)
  panel.text(4.9,3.5,'*',cex = 1.7,col='grey50')
  panel.text(5.9,3.5,'*',cex = 1.7,col='grey50')
  panel.text(2.25,3.5,'*',cex = 1.7)
  panel.text(3.25,3.5,'*',cex = 1.7)
  panel.text(4.25,3.5,'*',cex = 1.7)
  panel.text(5.25,3.5,'*',cex = 1.7)
  panel.text(6.25,3.5,'*',cex = 1.7)
}
p3 = bwplot(Mismatch~Site, data = total, groups = Model,ylim=c(-28,6),
       xlab = 'Latitude',ylab = 'Mismatch (days)',
       pch = "|", box.width = 1/6,
       panel = mypanel3,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=2),par.ylab.text=list(cex=1.8),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2),
                         superpose.symbol = list(fill = mycolors,col=mycolors)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1),labels=c('44.5','45.5','46.5','47.5','48.5','49.5'),
                          alternating=1))
      )

#setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1/JAE_Paper/Paper")
png('Total_Boxplots.png',width = 800,height = 550)
par(mar=c(5,6,4,3))
print(p1, split = c(1,1,2,2), more = T)
print(p2, split = c(2,1,2,2), more = T)
print(p3, split = c(1,2,2,2), more = F)
dev.off()


setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Predictions")

rcp26_2 = read.csv('Predicted_RCP_26.csv',colClasses = c('Site'='factor'))
rcp45_2 = read.csv('Predicted_RCP_45.csv',colClasses = c('Site'='factor'))
rcp85_2 = read.csv('Predicted_RCP_85.csv',colClasses = c('Site'='factor'))
bud1 = rcp26_2$Budburst
bud2 = rcp45_2$Budburst
bud3 = rcp85_2$Budburst

budburstold = c(bud1,bud2,bud3)

rm(rcp26_2,rcp45_2,rcp85_2,bud1,bud2,bud3)

setwd("~/Master_Uottawa/SBW_models/Budworm_data")
past = read.csv('Results_Past_BioSim.csv')
bud1 = past$Budburst
budburstold = c(bud1,budburstold)
rm(past,bud1)

total$Oldbudburst = budburstold
rm(budburstold)
total$OldMismatch = total$Emergence - total$Oldbudburst

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")

png('Mismatch_Future_Insect_4_hourly.png',width = 800,height = 480)
mycolors <- grey.colors(4, start = 0.9, end = 0.3)
trellis.par.set(superpose.symbol = list(fill = mycolors,col=mycolors))
bwplot(OldMismatch~Site, data = total, groups = Model,
       xlab = 'Latitude',ylab = 'Mismatch (days)',
       pch = "|", box.width = 1/6,
       panel = panel.superpose,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=3),par.ylab.text=list(cex=2.7),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1),labels=c('44.5','45.5','46.5','47.5','48.5','49.5'),
                          alternating=1))
)
dev.off()

#### figure Frithjof ####
t1 = 87.36 #84
bf = -1.32 #-0.12
cf = 7.14 #12.5
fstar = 18.6 #17.71

MeanTemp = 6.9;  # Fredericton mean temp
AmpliTemp = 15.5; # Fredericton amplitude temp
phase = 200;  # Ottawa temperature phase
dt = 1/24; # dt in 1-hour intervals
t = seq(1,365,by=dt);  # days in a year

xmean1 = MeanTemp + AmpliTemp * cos(2*pi*(t-phase)/365);

rft = 1/(1+exp(bf*(xmean1-cf)))
rft = rft[2300:2900]
rft2 = c(rep(rft[1],100),rft)
rft3 = xmean1 - 5

#pdf('Conceptual.pdf',width = 12, height = 7)
png('Conceptual.png',width = 800, height = 550)
par(mfrow=c(2,2),mai=c(0,0,1,1),mar=c(1,1,1,1))
plot(rft,type='l',xlab="",ylab = "",axes = F,lwd = 2)
box()
title(ylab = 'Accumulation rate (R(x))',mgp = c(1,1,0),cex.lab=1.2)
title(xlab = 'Temperature (x)',mgp = c(1,1,0),cex.lab=1.2)
lines(rft2, lty = 2, lwd = 2)
text(300,0.8,'Consumer')
text(500,0.5,'Resource')

plot(xmean1[150:5000],ylim=c(-14,25),type='l',lwd=2,xlab="",ylab = "",axes = F)
axis(1,at=0,labels = 'Jan 1')
box()
title(ylab = 'Temperatures',mgp = c(1,1,0),cex.lab=1.2)
title(xlab = 'Time (t)',mgp = c(1,1,0),cex.lab=1.2)
lines(rft3[150:5000], col = 'grey50',lwd=2)
text(2450,20,'lower latitude /')
text(2450,15,'global change')
text(4200,10,'higher latitude /',col = 'grey40')
text(4200,5,'without global change',col = 'grey40')

rft4 = rft - 0.1
rft5 = rft2 - 0.2
rft6 = rft5 - 0.15

plot(rft,type='l',ylim=c(-0.34,1),xlab="",ylab = "",axes = F, lwd= 2)
box()
title(ylab = 'Accumulation rate (R(x))',mgp = c(1,1,0),cex.lab=1.2)
title(xlab = 'Time',mgp = c(1,1,0),cex.lab=1.2)
lines(rft5, lty = 2, lwd = 2)
lines(rft4, col = 'grey50', lwd = 2)
lines(rft6, col = 'grey50', lwd = 2, lty = 2)
text(300,0.8,'Consumer')
text(550,0.2,'Resource')

toto = rft
toto2 = rft4+0.2
toto3 = rft5+0.2
ft = cumsum(toto)
ft2 = cumsum(toto2)
ft3 = cumsum(toto3)

plot(ft3, type = 'l',ylim=c(0,160),xlim=c(0,535),lty=2,xlab="",ylab = "",axes = F, lwd= 2)
box()
title(ylab = 'Accumulated quantity',mgp = c(1,1,0),cex.lab=1.2)
title(xlab = 'Time',mgp = c(1,1,0),cex.lab=1.2)
lines(ft2,lwd=2,col='grey50')
lines(ft,lwd=2)
abline(h = 130, lwd = 2, col = 'grey70',lty=3)
text(70,140, 'F = 1')
vot1x = which(ft2>=130)[1]
vot1y = ft2[vot1x]
abline(v = vot1x ,col='grey50')
vot2x = which(ft>=130)[1]
vot2y = ft[vot2x]
abline(v = vot2x)
vot3x = which(ft3>=130)[1]
vot3y = ft[vot3x]
abline(v = vot3x ,lty = 2)
arrows(x0 = vot1x, x1 = vot2x, y0 = 130, y1 = 130, lwd = 2, col = 'grey50',angle = 25, code = 3, length = 0.1)
arrows(x0 = vot2x, x1 = vot3x, y0 = 130, y1 = 130, lwd = 2, angle = 25, code = 3, length = 0.1)
text(325,155,'Within species shift',col='grey50')
text(480,60,'Mismatch')
text(480,40,'between')
text(480,20,'species')

dev.off()

### with Lattice
t1 = 87.36 #84
bf = -1.32 #-0.12
cf = 7.14 # 12.5
fstar = 18.6 #17.71

MeanTemp = 6.9;  # Fredericton mean temp
AmpliTemp = 15.5; # Fredericton amplitude temp
phase = 200;  # Ottawa temperature phase
dt = 1/24; # dt in 1-hour intervals
t = seq(1,365,by=dt);  # days in a year

xmean1 = MeanTemp + AmpliTemp * cos(2*pi*(t-phase)/365);

rft = 1/(1+exp(bf*(xmean1-cf)))
rft = rft[2300:2900]
rft2 = c(rep(rft[1],100),rft)
rft3 = xmean1 - 5

rft4 = rft - 0.1
rft5 = rft2 - 0.2
rft6 = rft5 - 0.15

toto = rft
toto2 = rft4+0.2
toto3 = rft5+0.2
ft = cumsum(toto)
ft2 = cumsum(toto2)
ft3 = cumsum(toto3)

vot1x = which(ft2>=130)[1]
vot1y = ft2[vot1x]
vot2x = which(ft>=130)[1]
vot2y = ft[vot2x]
vot3x = which(ft3>=130)[1]
vot3y = ft[vot3x]

mypanel1 = function(x,y, ...){
  panel.xyplot(x,y, ...)
  panel.text(300,0.8,'Consumer', cex = 1.4)
  panel.text(500,0.5,'Resource', cex = 1.4)
  panel.text(-20,0.01,'A', cex = 1.4)
}
a1 = xyplot(rft~seq(1,length(rft)),type = 'l',lwd = 2, col = 'black',
       ylab = 'Accumulation rate (R(x))', xlab = 'Temperature (x)',
       panel = mypanel1,
       par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.7),
                         axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
       scales=list(x=list(draw=F),y=list(draw=F))
       )
a2 = xyplot(rft2~seq(1,length(rft2)), type = 'l', lty = 2, lwd = 2, col = 'black')
atot = a1 + as.layer(a2)

mypanel2 = function(x,y, ...){
  panel.xyplot(x,y, ...)
  panel.text(2450,20,'lower latitude /',cex = 1.4)
  panel.text(2450,17,'global change',cex = 1.4)
  panel.text(4150,10,'higher latitude /',col = 'grey50',cex = 1.2)
  panel.text(4150,7,'without global change',col = 'grey50',cex = 1.2)
  panel.text(-145,-12,'B', cex = 1.4)
}
b1 = xyplot(xmean1[150:5000]~seq(1,length(xmean1[150:5000])),ylim=c(-14,25),type='l',lwd=2,col = 'black',
       xlab = 'Time (t)', ylab = 'Temperatures',
       panel = mypanel2,
       par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.9),
                         axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
       scales=list(x=list(draw=F),y=list(draw=F))
       )
b2 = xyplot(rft3[150:5000]~seq(1,length(rft3[150:5000])), type='l', col = 'grey60',lwd=2)
btot = b1+as.layer(b2)

mypanel3 = function(x,y, ...){
  panel.xyplot(x,y, ...)
  panel.text(300,0.8,'Consumer',cex = 1.4)
  panel.text(550,0.2,'Resource',cex = 1.4)
  panel.text(-20,-0.3, 'C', cex = 1.4)
}
c1 = xyplot(rft~seq(1,length(rft)),type='l',lwd=2,col = 'black',
       xlab = 'Time (t)', ylab = 'Accumulation rate (R(x))', ylim=c(-0.38,1.1),
       panel = mypanel3,
       par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.7),
                         axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
       scales=list(x=list(draw=F),y=list(draw=F))
       )
c2 = xyplot(rft5~seq(1,length(rft5)), type='l', lty = 2, lwd = 2, col = 'black')
c3 = xyplot(rft4~seq(1,length(rft4)), type='l', lwd = 2, col = 'grey60')
c4 = xyplot(rft6~seq(1,length(rft6)), type='l', lty = 2, lwd = 2, col = 'grey60')
ctot = c1+as.layer(c2)+as.layer(c3)+as.layer(c4)

mypanel4 = function(x,y, ...){
  panel.xyplot(x,y, ...)
  panel.text(70,140, 'F = 1',cex = 1.4)
  panel.abline(v = vot1x ,col='grey50')
  panel.abline(v = vot2x,col = 'black')
  panel.abline(v = vot3x ,lty = 2, col = 'black')
  panel.abline(h = 130, lwd = 2, col = 'grey70',lty=3)
  panel.text(300,155,'Within species shift',cex = 1.1)
  panel.text(480,55,'Mismatch',cex = 1.1,col='grey50')
  panel.text(480,45,'between',cex = 1.1,col='grey50')
  panel.text(480,35,'species',cex = 1.1,col='grey50')
  panel.text(20,15,'D', cex = 1.4)
  panel.arrows(x0 = vot1x, x1 = vot2x, y0 = 130, y1 = 130, lwd = 2, col = 'black',angle = 25, code = 3, length = 0.1)
  panel.arrows(x0 = vot2x, x1 = vot3x, y0 = 130, y1 = 130, lwd = 2, col = 'grey50',angle = 25, code = 3, length = 0.1)
}
d1 = xyplot(ft3~seq(1,length(ft3)),type = 'l',ylim=c(-1,160),xlim=c(0,540),lty=2,lwd = 2, col = 'grey50',
       ylab = 'Accumulated quantity', xlab = 'Time (t)',
       panel = mypanel4,
       par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.9),
                         axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
       scales=list(x=list(draw=F),y=list(draw=F))
       )
d2 = xyplot(ft2~seq(1,length(ft2)),type='l',lwd=2,col='black')
d3 = xyplot(ft~seq(1,length(ft)),type='l',lwd=2, col = 'grey50')
dtot = d1+as.layer(d2)+as.layer(d3)

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
png('Conceptual.png', width = 800, height = 550)
par(mar=c(5,6,4,3))
print(atot, split = c(1,1,2,2), more = T)
print(btot, split = c(2,1,2,2), more = T)
print(ctot, split = c(1,2,2,2), more = T)
print(dtot, split = c(2,2,2,2), more = F)
dev.off()

# par(mar=c(5,6,4,3))
# zozo=c(ctot,dtot,atot,btot,layout=c(2,2),merge.legends=F)
# print(zozo,position=c(0,0,1,1),split=c(1,1,1,1),more=F)
#### new figure 1 ####
library(lattice)
library(latticeExtra)

t1 = 87.36 #84
bf = -1.32 #-0.12
cf = 7.14 # 12.5
fstar = 18.6 #17.71
cf2 = 8.14
bf2 = -1.16

MeanTemp = 6.9;  # Fredericton mean temp
AmpliTemp = 15.5; # Fredericton amplitude temp
phase = 200;  # Ottawa temperature phase
dt = 1/24; # dt in 1-hour intervals
t = seq(1,365,by=dt);  # days in a year

xmean1 = MeanTemp + AmpliTemp * cos(2*pi*(t-phase)/365);

rft = 1/(1+exp(bf*(xmean1-cf)))
rft = rft[2300:2900]
rft2 = 1/(1+exp(bf2*(xmean1-cf2)))
rft2 = rft2[2300:2900]
xmean2 = xmean1 - 5

mypanel1 = function(x,y, ...){
  panel.xyplot(x,y, ...)
  panel.text(300,0.8,'Consumer', cex = 1.4)
  panel.text(500,0.5,'Resource', cex = 1.4)
  panel.text(-20,0.01,'A', cex = 1.4)
}
a1 = xyplot(rft~seq(1,length(rft)),type = 'l',lwd = 2, col = 'black',
            ylab = 'Accumulation rate (R(x))', xlab = 'Temperature (x)',
            panel = mypanel1,
            par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.7),
                              axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
            scales=list(x=list(draw=F),y=list(draw=F))
)
a2 = xyplot(rft2~seq(1,length(rft2)), type = 'l', lty = 2, lwd = 2, col = 'black')
atot = a1 + as.layer(a2)

mypanel2 = function(x,y, ...){
  panel.xyplot(x,y, ...)
  panel.text(2450,20,'lower latitude /',cex = 1.4)
  panel.text(2450,17,'global change',cex = 1.4)
  panel.text(4150,10,'higher latitude /',col = 'grey50',cex = 1.2)
  panel.text(4150,7,'without global change',col = 'grey50',cex = 1.2)
  panel.text(-145,-12,'B', cex = 1.4)
}
b1 = xyplot(xmean1[150:5000]~seq(1,length(xmean1[150:5000])),ylim=c(-14,25),type='l',lwd=2,col = 'black',
            xlab = 'Time (t)', ylab = 'Temperatures',
            panel = mypanel2,
            par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.9),
                              axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
            scales=list(x=list(draw=F),y=list(draw=F))
)
b2 = xyplot(xmean2[150:5000]~seq(1,length(xmean2[150:5000])), type='l', col = 'grey60',lwd=2)
btot = b1+as.layer(b2)

#xmean2 = xmean1 - 1
xmean2 = (MeanTemp-1) + (AmpliTemp-2) * cos(2*pi*(t-phase)/365);
rft3 = 1/(1+exp(bf*(xmean2-cf)))
rft3 = rft3[2300:2900]
rft4 = 1/(1+exp(bf2*(xmean2-cf2)))
rft4 = rft4[2300:2900]
rft5 = rft2 - 0.2
rft6 = rft4 -0.2

mypanel3 = function(x,y, ...){
  panel.xyplot(x,y, ...)
  panel.text(300,0.8,'Consumer',cex = 1.4)
  panel.text(550,0.2,'Resource',cex = 1.4)
  panel.text(-20,-0.3, 'C', cex = 1.4)
}
c1 = xyplot(rft~seq(1,length(rft)),type='l',lwd=2,col = 'black',
            xlab = 'Time (t)', ylab = 'Accumulation rate (R(x(t)))', ylim=c(-0.38,1.1),
            panel = mypanel3,
            par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.7),
                              axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
            scales=list(x=list(draw=F),y=list(draw=F))
)
c2 = xyplot(rft5~seq(1,length(rft5)), type='l', lty = 2, lwd = 2, col = 'black')
c3 = xyplot(rft3~seq(1,length(rft3)), type='l', lwd = 2, col = 'grey60')
c4 = xyplot(rft6~seq(1,length(rft6)), type='l', lty = 2, lwd = 2, col = 'grey60')
ctot = c1+as.layer(c2)+as.layer(c3)+as.layer(c4)

toto = rft
toto2 = rft2 #rft4+0.2
toto3 = rft3 #rft5+0.2
ft = cumsum(toto)
ft2 = cumsum(toto2)
ft3 = cumsum(toto3)
ft4 = cumsum(rft4)

vot2x = which(ft2>=90)[1]
vot2y = ft2[vot2x]
vot1x = which(ft>=130)[1]
vot1y = ft[vot1x]
vot3x = which(ft3>=130)[1]
vot3y = ft3[vot3x]
vot4x = which(ft4>=90)[1]
vot4y = ft4[vot4x]

mypanel4 = function(x,y, ...){
  panel.xyplot(x,y, ...)
  panel.text(170,140, expression(paste('F'[e])),cex = 1.4)
  panel.text(170,100, expression(paste('F'[b])),cex = 1.4)
  panel.abline(v = vot1x,lty = 2, col='grey70')
  panel.abline(v = vot2x,lty = 2, col = 'grey70')
  panel.abline(v = vot3x ,lty = 2, col = 'grey70')
  panel.abline(v = vot4x ,lty = 2, col = 'grey70')
  panel.abline(h = 130, lwd = 2, col = 'grey70',lty=3)
  panel.abline(h = 90, lwd = 2, col = 'grey70',lty=3)
  #panel.text(480,155,'Within species shift',cex = 1.1)
  panel.text(485,138,'shift',cex = 1.1)
  panel.text(520,98,'shift',cex = 1.1)
  panel.text(550,117,'Mismatch',cex = 1.1,col='grey50')
  panel.text(445,117,'Mismatch',cex = 1.1,col='black')
  #panel.text(540,45,'between',cex = 1.1,col='grey50')
  #panel.text(540,35,'species',cex = 1.1,col='grey50')
  panel.text(160,15,'D', cex = 1.4)
  panel.text(vot1x,10,expression(paste('t'[e2]^'*')),cex = 1.4)
  panel.text(vot2x,10,expression(paste('t'[b2]^'*')),cex = 1.4)
  panel.text(vot3x,10,expression(paste('t'[e1]^'*')),cex = 1.4)
  panel.text(vot4x,10,expression(paste('t'[b1]^'*')),cex = 1.4)
  panel.arrows(x0 = vot1x, x1 = vot3x, y0 = 130, y1 = 130, lwd = 2, col = 'black',angle = 25, code = 3, length = 0.1)
  panel.arrows(x0 = (vot2x+2), x1 = vot2x, y0 = 90, y1 = 90, lwd = 2, col = 'black',angle = 25, code = 2, length = 0.1)
  panel.arrows(x0 = (vot4x-2), x1 = vot4x, y0 = 90, y1 = 90, lwd = 2, col = 'black',angle = 25, code = 2, length = 0.1)
  panel.segments(x0 = (vot4x-2), x1 = (vot2x+2), y0 = 90, y1 = 90, lwd =2, lty = 2, col = 'black')
  panel.arrows(x0 = vot3x, x1 = vot4x, y0 = 110, y1 = 110, lwd = 2, col = 'grey50',angle = 25, code = 3, length = 0.1)
  panel.arrows(x0 = vot1x, x1 = vot2x, y0 = 110, y1 = 110, lwd = 2, col = 'black',angle = 25, code = 3, length = 0.1)
}
d1 = xyplot(ft3~seq(1,length(ft3)),type = 'l',ylim=c(-1,160),xlim=c(150,600),lwd = 2, col = 'grey50',
            ylab = 'Accumulated quantity', xlab = 'Time (t)',
            panel = mypanel4,
            par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.9),
                              axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
            #scales=list(x=list(draw=F),y=list(draw=F))
            #scales=list(x=list(at=c(vot1x,vot2x,vot3x,vot4x),cex = 1.4,
            #                   labels=c(expression(paste('t'[e2]^'*')),expression(paste('t'[b2]^'*')),
            #                                                          expression(paste('t'[e1]^'*')),expression(paste('t'[b1]^'*')))),
            #            y=list(draw=F))
            scales=list(x=list(draw=F),y=list(draw=F))
)
d2 = xyplot(ft4~seq(1,length(ft4)),type='l',lwd=2,lty=2,col='grey50')
d3 = xyplot(ft~seq(1,length(ft)),type='l',lwd=2, col = 'black')
d4 = xyplot(ft2~seq(1,length(ft2)),type='l',lwd=2,lty=2, col = 'black')
dtot = d1+as.layer(d2)+as.layer(d3)+as.layer(d4)

# presentation
mypanel4 = function(x,y, ...){
  panel.xyplot(x,y, ...)
  panel.text(170,140, expression(paste('F'[e])),cex = 1.4)
  panel.text(170,100, expression(paste('F'[b])),cex = 1.4)
  panel.abline(v = vot1x,lty = 2, col='pink')
  panel.abline(v = vot2x,lty = 2, col = 'pink')
  panel.abline(v = vot3x ,lty = 2, col = 'lightblue')
  panel.abline(v = vot4x ,lty = 2, col = 'lightblue')
  panel.abline(h = 130, lwd = 2, col = 'grey70',lty=3)
  panel.abline(h = 90, lwd = 2, col = 'grey70',lty=3)
  #panel.text(480,155,'Within species shift',cex = 1.1)
  panel.text(485,138,'shift',cex = 1.1)
  panel.text(520,98,'shift',cex = 1.1)
  panel.text(550,117,'Mismatch',cex = 1.1,col='blue')
  panel.text(445,117,'Mismatch',cex = 1.1,col='red')
  #panel.text(540,45,'between',cex = 1.1,col='grey50')
  #panel.text(540,35,'species',cex = 1.1,col='grey50')
  panel.text(160,15,'D', cex = 1.4)
  panel.text(vot1x,10,expression(paste('t'[e2]^'*')),cex = 1.4,col='red')
  panel.text(vot2x,10,expression(paste('t'[b2]^'*')),cex = 1.4,col='red')
  panel.text(vot3x,10,expression(paste('t'[e1]^'*')),cex = 1.4,col='blue')
  panel.text(vot4x,10,expression(paste('t'[b1]^'*')),cex = 1.4,col='blue')
  panel.arrows(x0 = vot1x, x1 = vot3x, y0 = 130, y1 = 130, lwd = 2, col = 'black',angle = 25, code = 3, length = 0.1)
  panel.arrows(x0 = (vot2x+2), x1 = vot2x, y0 = 90, y1 = 90, lwd = 2, col = 'black',angle = 25, code = 2, length = 0.1)
  panel.arrows(x0 = (vot4x-2), x1 = vot4x, y0 = 90, y1 = 90, lwd = 2, col = 'black',angle = 25, code = 2, length = 0.1)
  panel.segments(x0 = (vot4x-2), x1 = (vot2x+2), y0 = 90, y1 = 90, lwd =2, lty = 2, col = 'black')
  panel.arrows(x0 = vot3x, x1 = vot4x, y0 = 110, y1 = 110, lwd = 2, col = 'blue',angle = 25, code = 3, length = 0.1)
  panel.arrows(x0 = vot1x, x1 = vot2x, y0 = 110, y1 = 110, lwd = 2, col = 'red',angle = 25, code = 3, length = 0.1)
}
d1 = xyplot(ft3~seq(1,length(ft3)),type = 'l',ylim=c(-1,160),xlim=c(150,600),lwd = 2, col = 'blue',
            ylab = 'Accumulated quantity', xlab = 'Time (t)',
            panel = mypanel4,
            par.settings=list(axis.text=list(cex=2),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.9),
                              axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
            #scales=list(x=list(draw=F),y=list(draw=F))
            #scales=list(x=list(at=c(vot1x,vot2x,vot3x,vot4x),cex = 1.4,
            #                   labels=c(expression(paste('t'[e2]^'*')),expression(paste('t'[b2]^'*')),
            #                                                          expression(paste('t'[e1]^'*')),expression(paste('t'[b1]^'*')))),
            #            y=list(draw=F))
            scales=list(x=list(draw=F),y=list(draw=F))
)
d2 = xyplot(ft4~seq(1,length(ft4)),type='l',lwd=2,lty=2,col='blue')
d3 = xyplot(ft~seq(1,length(ft)),type='l',lwd=2, col = 'red')
d4 = xyplot(ft2~seq(1,length(ft2)),type='l',lwd=2,lty=2, col = 'red')
dtot = d1+as.layer(d2)+as.layer(d3)+as.layer(d4)

# png('figure_1D.png',width = 780,height = 480)
# print(dtot)
# dev.off()

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1/JAE_Paper/Paper")
png('Conceptual.png', width = 800, height = 600)
par(mar=c(5,6,4,3))
print(atot, split = c(1,1,2,2), more = T)
print(btot, split = c(2,1,2,2), more = T)
print(ctot, split = c(1,2,2,2), more = T)
print(dtot, split = c(2,2,2,2), more = F)
dev.off()

#### warm spell ####
# derivative curves #
# for balsam fir, except the date t1
t1 = 87;  # starting date for heat accumulation
bf = -1.32;
cf = 7.14;
Fb = 18.6;

# spruce budworm
beta1 = 0.194;
beta2 = 3.0;
beta3 = 5.84;
beta4 = 0.034;
tb = 2.5;
tm = 35;
t1s = 31+28

xm1 = seq(-30,35,length.out = 1000);

# SBW
RM1primesbw = beta1*((beta3*exp(beta2-(beta3*(xm1-tb))/(tm-tb)))/((tm-tb)*(exp(beta2-(beta3*(xm1-tb))/(tm-tb))+1)^2)-exp(((xm1-tb)/(tm-tb)-1)/beta4)/(beta4*(tm-tb)))

# TREE
RM1primetree2 = -bf*exp(bf*(xm1-cf))/(1+exp(bf*(xm1-cf)))^2;
RM1primetree = RM1primetree2/30

sbwmaxindex = which(RM1primesbw==max(RM1primesbw))
treemaxindex = which(RM1primetree==max(RM1primetree))
sbwmax = xm1[sbwmaxindex]
treemax = xm1[treemaxindex]

# temperatures
MeanTemp = 6.9;  # Fredericton mean temp
AmpliTemp = 6.5; # Fredericton amplitude temp
phase = 200;  # Ottawa temperature phase
dt = 5; # dt in 1-hour intervals
t = seq(1,365,by=dt);  # days in a year

xmean1 = MeanTemp + AmpliTemp * cos(2*pi*(t-phase)/365);

xmean2 = (xmean1[1:41]/1000)-0.0006

# polygon
x1 = treemax-0.1
x2 = treemax+0.9
y1 = 0.004
y2 = 0.007

labs = c(expression(paste("c"[r]*" = t"[s])),expression(paste("c"[c])))

plot(RM1primetree~xm1,type='l',col='grey60',lwd=3,axes=F,xlab='Time',ylab=" ",xlim=c(0,27),ylim=c(0,0.012),cex.lab=1.5)
axis(1,at=c(treemax,sbwmax),labels=labs,cex.axis=1.2)
box()
lines(RM1primesbw~xm1,lwd=3)
abline(v = sbwmax, col='black',lty=2,lwd=3)
abline(v = treemax, col='grey60',lty=2,lwd=3)
lines(xmean2~seq(-10,30),col = 'black',lty = 3,lwd=2)
polygon(x=c(x1,x2,x2,x1),y=c(y1,y1,y2,y2),col='grey80',lty=0)
text((x1+0.5),0.0075,expression(paste(Delta *'t')),cex=1.2)
text((x1-0.5),0.0055,expression(paste(Delta *'x')),cex=1.2)
text(24,0.011,'Temperature',cex=1.2)
text(4,0.008,'Warm spell',cex=1.2,col='grey70')
arrows(x0=4,x1=(cf-0.1),y0=0.0075,y1=0.006,length = 0,col='grey70',lwd=3)


#### mismatch ####
t1 = seq(1,365);  # days in a year
t2 = t1 + 100
x1 = t1^3
x2 = ((t2^3)/5) + 1e7

x3 = x1[length(x1):1]
x4 = x2[length(x2):1]

par(mar=c(5,6,4,3))
plot(x3,type='l',lwd=3,axes=F,xlab = 'Temperature',ylab=expression(paste('t'^'*'*'(x)')),cex.lab=1.5,xlim=c(0,360),ylim=c(-1e7,5e7))
lines(x4,col='grey70',lwd=3)
abline(h=-3e6,lwd=3,lty = 2)
box()
axis(1,at=c(45,235),labels = c('mismatch decreases','mismatch increases as x increases'),cex.axis=1.2,tick = F)
text(325,-6e6,expression(paste('t'[0]*'+F'[1])),cex = 1.3)
text(325,6e6,expression(paste('t'^'*'*'=t'[0]*'+F'[1]*'(1+e'^'b1(x-c)'*')')),cex = 1.3)
text(325,1.5e7,expression(paste('t'^'*'*'=t'[0]*'+F'[2]*'(1+e'^'b2(x-c)'*')')),cex = 1.3,col='grey40')
arrows(x0=25,x1=25,y0=x3[25],y1=x4[25],length = 0.1, angle = 30, col='grey40',lwd=3, code = 3)
arrows(x0=200,x1=200,y0=x3[200],y1=x4[200],length = 0.1, angle = 30, col='grey40',lwd=3, code = 3)
text(170,1.25e7,'mismatch',cex = 1.3)
points(93,x3[93],pch=19,cex=1.5)
abline(v=93,lty=3,lwd=3,col='grey50')


#### speel and mismatch lattice ####
library(lattice)
library(latticeExtra)

### linear approximation
MeanTemp = 6.9;  # Fredericton mean temp
AmpliTemp = 15.5; # Fredericton amplitude temp
phase = 200;  # Ottawa temperature phase
dt = 1/24; # dt in 1-hour intervals
t = seq(1,365,by=dt);  # days in a year

# for balsam fir, except the date t1
t1 = 87;  # starting date for heat accumulation
bf = -1.32;
cf = 7.14;
Fb = 18.6;

# spruce budworm
beta1 = 0.194;
beta2 = 3.0;
beta3 = 5.84;
beta4 = 0.034;
tb = 2.5;
tm = 35;
t1s = 31+28

LL=31;

emergenceres = rep(0,LL)
budburstres = rep(0,LL)

indexresults = 1
for (nn in 1:LL){
  
  AddMean = (nn-1)*0.1;
  xmean1 = MeanTemp+AddMean + AmpliTemp * cos(2*pi*(t-phase)/365);
  #xm1 = xmean1[seq(1,length(xmean1),by=4)]
  tt = t[1:phase/dt];
  xm1 = xmean1[1:(phase/dt)];
  
  # larvae
  rt2 = rep(0,length(xm1))
  for (i in 1:length(xm1)){
    if (xm1[i] >= tb && xm1[i] <= tm){
      tau = (xm1[i]-tb)/(tm-tb)
      expon = beta2-beta3*tau
      rt2[i] = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))/24
    }else{
      rt2[i] = 0
    }
  }
  
  rt2[1:(t1s*24)] = 0
  rt2tot = cumsum(rt2)
  index = which(rt2tot>=1)
  emergence = index[1]/24
  #emergence = moulting + 58
  
  # from tree
  
  rft = 1/(1+exp(bf*(xm1-cf)))/24
  rft[1:(t1*24)] = 0
  
  ft = cumsum(rft)
  
  index = which(ft>=Fb)
  budburst = index[1]/24
  
  emergenceres[indexresults] = emergence
  budburstres[indexresults] = budburst
  indexresults = indexresults+1
}

# NOW COMPARE WITH THE LINEAR APPROXIMATION 
# TREE
RM1prime = -bf*exp(bf*(xm1-cf))/(1+exp(bf*(xm1-cf)))^2;
burstindex = min(which((ft-Fb)>0));

INT = sum(RM1prime[1:burstindex])*dt;

SLOPE = -INT/(rft[burstindex]*24)
ord = rep(0,2)
ord[1] = budburstres[1]
ord[2] = budburstres[1]+SLOPE*(LL-1)*0.1

# NOW COMPARE WITH THE LINEAR APPROXIMATION
# SBW
RM1prime = beta1*((beta3*exp(beta2-(beta3*(xm1-tb))/(tm-tb)))/((tm-tb)*(exp(beta2-(beta3*(xm1-tb))/(tm-tb))+1)^2)-exp(((xm1-tb)/(tm-tb)-1)/beta4)/(beta4*(tm-tb)))

#RM1prime = -b*exp(b*(xm1-c))/(1+exp(b*(xm1-c)))^2;
burstindex = min(which((rt2tot-1)>0));

INT = sum(RM1prime[1:burstindex])*dt;

SLOPE2 = -INT/(rt2[burstindex]*24)
ord2 = rep(0,2)
ord2[1] = emergenceres[1]
ord2[2] = emergenceres[1]+SLOPE2*(LL-1)*0.1

### spell
# tree
t1 = 87;  # starting date for heat accumulation
bf = -1.32;
cf = 7.14;
Fb = 18.6;

# spruce budworm
beta1 = 0.194;
beta2 = 3.0;
beta3 = 5.84;
beta4 = 0.034;
tb = 2.5;
tm = 35;
t1s = 31+28

xm1 = seq(-30,35,length.out = 1000);

# SBW
RM1primesbw = beta1*((beta3*exp(beta2-(beta3*(xm1-tb))/(tm-tb)))/((tm-tb)*(exp(beta2-(beta3*(xm1-tb))/(tm-tb))+1)^2)-exp(((xm1-tb)/(tm-tb)-1)/beta4)/(beta4*(tm-tb)))

# TREE
RM1primetree2 = -bf*exp(bf*(xm1-cf))/(1+exp(bf*(xm1-cf)))^2;
RM1primetree = RM1primetree2/30

sbwmaxindex = which(RM1primesbw==max(RM1primesbw))
treemaxindex = which(RM1primetree==max(RM1primetree))
sbwmax = xm1[sbwmaxindex]
treemax = xm1[treemaxindex]

# temperatures
MeanTemp = 6.9;  # Fredericton mean temp
AmpliTemp = 6.5; # Fredericton amplitude temp
phase = 200;  # Ottawa temperature phase
dt = 5; # dt in 1-hour intervals
t = seq(1,365,by=dt);  # days in a year

xmean1 = MeanTemp + AmpliTemp * cos(2*pi*(t-phase)/365);

xmean2 = (xmean1[1:41]/1000)-0.0006

# polygon
x1 = treemax-0.1
x2 = treemax+0.8
y11 = 0.0039
y12 = 0.0044
y21 = 0.007
y22 = 0.0075

labs = c(expression(paste("c"[1]*" = t"[s])),expression(paste("c"[2])))

mypanel1 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(v = sbwmax, col='black',lty=2,lwd=3)
  panel.abline(v = treemax, col='grey60',lty=2,lwd=3)
  panel.text((x1+0.5),0.0077,expression(paste(Delta *'t')),cex=1.2)
  panel.text((x1-0.5),0.0055,expression(paste(Delta *'x')),cex=1.2)
  panel.text(24,0.011,'Temperature',cex=1.2)
  panel.text(4,0.008,'Warm spell',cex=1.2,col='grey70')
  panel.text(5.5,0.01,expression(paste("R'"[b]*'(x)')),cex=1.2,col='grey60')
  panel.text(22,0.009,expression(paste("R'"[e]*'(x)')),cex=1.2,col='black')
  panel.polygon(x=c(x1,x2,x2,x1),y=c(y11,y12,y22,y21),col='grey80',lty=0)
  panel.arrows(x0=4,x1=(cf-0.1),y0=0.0075,y1=0.006,length = 0,col='grey70',lwd=3)
  panel.text(0.5,0.0005,'B',cex = 1.5)
}
p11 = xyplot(RM1primetree~xm1,type='l',col='grey60',lwd=3,
            xlab='Time',ylab=" ",xlim=c(0,27),ylim=c(0,0.012),
            panel = mypanel1,
            par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.9),
                              axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
            scales=list(x=list(at=c(treemax,sbwmax),labels=labs),y=list(draw=F))
            )
p12 = xyplot(RM1primesbw~xm1,lwd=3,type='l',col='black')
p13 = xyplot(xmean2~seq(-10,30),type='l',col = 'black',lty = 3,lwd=2)

p1 = p11+as.layer(p12)+as.layer(p13)

# no overlap
# tree
t1 = 87;  # starting date for heat accumulation
bf = -1.32;
cf = 7.14;
Fb = 18.6;

# tree2
t2 = 87;  # starting date for heat accumulation
bf2 = -1.32;
cf2 = 19.34;
Fb2 = 18.6;

# TREE
RM1primetree21 = -bf2*exp(bf2*(xm1-cf2))/(1+exp(bf2*(xm1-cf2)))^2;
RM1primetree2 = RM1primetree21/30
treemaxindex2 = which(RM1primetree2==max(RM1primetree2))[1]
treemax2 = xm1[treemax2]

mypanel2 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(v = treemax2, col='black',lty=2,lwd=3)
  panel.abline(v = treemax, col='grey60',lty=2,lwd=3)
  panel.text((x1+0.5),0.0077,expression(paste(Delta *'t')),cex=1.2)
  panel.text((x1-0.5),0.0055,expression(paste(Delta *'x')),cex=1.2)
  panel.text(24,0.011,'Temperature',cex=1.2)
  panel.text(4,0.008,'Warm spell',cex=1.2,col='grey70')
  panel.text(5.5,0.01,expression(paste("R'"[b]*'(x)')),cex=1.2,col='grey60')
  panel.text(22,0.009,expression(paste("R'"[e]*'(x)')),cex=1.2,col='black')
  panel.polygon(x=c(x1,x2,x2,x1),y=c(y11,y12,y22,y21),col='grey80',lty=0)
  panel.arrows(x0=4,x1=(cf-0.1),y0=0.0075,y1=0.006,length = 0,col='grey70',lwd=3)
  panel.text(0.5,0.0005,'A',cex = 1.5)
}
p21 = xyplot(RM1primetree~xm1,type='l',col='grey60',lwd=3,
             xlab='Time',ylab=" ",xlim=c(0,27),ylim=c(0,0.012),
             panel = mypanel2,
             par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.9),
                               axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
             scales=list(x=list(at=c(treemax,treemax2),labels=labs),y=list(draw=F))
)
p22 = xyplot(RM1primetree2~xm1,lwd=3,type='l',col='black')
p23 = xyplot(xmean2~seq(-10,30),type='l',col = 'black',lty = 3,lwd=2)

p2 = p21+as.layer(p22)+as.layer(p23)

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
png('Figure2.png',width = 1000, height = 480)
print(p2, split = c(1,1,2,1), more = T)
print(p1, split = c(2,1,2,1), more = F)
dev.off()

### mismatch
t1 = seq(1,365);  # days in a year
t2 = t1 + 100
tx1 = t1^3
tx2 = ((t2^3)/5) + 1e7

tx3 = tx1[length(tx1):1]
tx4 = tx2[length(tx2):1]

mypanel2 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(h=-3e6,lwd=3,lty = 2)
  panel.text(315,-6e6,expression(paste('t'[0]*'+F'[1])),cex = 1.3)
  panel.text(315,6e6,expression(paste('t'^'*'*'=t'[0]*'+F'[1]*'(1+e'^'b1(x-c)'*')')),cex = 1.3)
  panel.text(315,1.5e7,expression(paste('t'^'*'*'=t'[0]*'+F'[2]*'(1+e'^'b2(x-c)'*')')),cex = 1.3,col='grey40')
  panel.arrows(x0=25,x1=25,y0=tx3[25],y1=tx4[25],length = 0.1, angle = 30, col='grey40',lwd=3, code = 3)
  panel.arrows(x0=200,x1=200,y0=tx3[200],y1=tx4[200],length = 0.1, angle = 30, col='grey40',lwd=3, code = 3)
  panel.text(170,1.25e7,'mismatch',cex = 1.3)
  panel.points(93,tx3[93],pch=19,cex=1.5,col='black')
  panel.abline(v=93,lty=3,lwd=3,col='grey50')
  panel.text(10,-0.7e7,'C',cex = 1.5)
  
}
p21 = xyplot(tx3~seq(1:length(tx3)),type='l',lwd=3,xlim=c(0,360),ylim=c(-1e7,5e7),col='black',
             xlab = 'Temperature',ylab=expression(paste('t'^'*'*'(x)')),
             panel = mypanel2,
             par.settings=list(axis.text=list(cex=1.3),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.9),
                               axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
             scales=list(x=list(at=c(45,235),labels = c('mismatch decreases','mismatch increases as x increases')),y=list(draw=F))
      )

p22 = xyplot(tx4~seq(1,length(tx4)),type='l',col='grey70',lwd=3)

p2 = p21+as.layer(p22)

#### linear appraoximation ####
mypanel3 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.text(0.15,116,'A',cex = 1.5)
}
p31 = xyplot(emergenceres~seq(1,length(emergenceres)),pch=19,ylim=c(115,133), col = 'black',
             xlab='increase in average temperature',ylab=expression(paste('t'^'*')),
             panel = mypanel3,
             par.settings=list(axis.text=list(cex=1.3),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.9),
                               axis.components=list(top=list(tck=0),right=list(tck=0))),
             scales=list(x=list(at=seq(0,30,by=5),labels = seq(0,3,by=0.5)),y=list(at = seq(115,135,by = 5)))
      )
p32 = xyplot(budburstres~seq(1,length(budburstres)),pch=19,col='grey70')
p33 = xyplot(ord~c(1,LL),type='l',col='grey70',lty=2,lwd=1.5)
p34 = xyplot(ord2~c(1,LL),type='l',lty=2,lwd=1.5,col='black')

p3 = p31+as.layer(p32)+as.layer(p33)+as.layer(p34)

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1/JAE_Paper/Paper/Supplements")
cairo_pdf('FigureS1.pdf', width = 12,height = 7)
print(p3, more = F)
dev.off()

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
png('Figure2.png',width = 1000, height = 480)
print(p3, split = c(1,1,2,1), more = T)
print(p1, split = c(2,1,2,1), more = F)
dev.off()

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
cairo_pdf('Figure2.pdf',width = 14, height = 7)
print(p3, split = c(1,1,2,1), more = T)
print(p1, split = c(2,1,2,1), more = F)
dev.off()

# setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
# svg('Figure2.svg',width = 16, height = 8)
# print(p3, split = c(1,1,2,2), more = T)
# print(p1, split = c(2,1,2,2), more = T)
# print(p2, split = c(1,2,2,2), more = F)
# dev.off()

# setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
# cairo_pdf('Warm_Spell.pdf',width = 16, height = 8)
# print(p1, split = c(1,1,2,1), more = T)
# print(p2, split = c(2,1,2,1), more = F)
# dev.off()
# 
# setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
# svg('Warm_Spell.svg',width = 16, height = 8)
# print(p1, split = c(1,1,2,1), more = T)
# print(p2, split = c(2,1,2,1), more = F)
# dev.off()
# 
# 
# setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
# cairo_pdf('Warm_Spell.pdf',width = 16, height = 8)
# print(p1)
# dev.off()
# 
# setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
# svg('Shift.svg',width = 16, height = 8)
# print(p2)
# dev.off()

#### spell for presentation ####
# tree
t1 = 87;  # starting date for heat accumulation
bf = -1.32;
cf = 7.14;
Fb = 18.6;

# spruce budworm
beta1 = 0.194;
beta2 = 3.0;
beta3 = 5.84;
beta4 = 0.034;
tb = 2.5;
tm = 35;
t1s = 31+28

xm1 = seq(-30,35,length.out = 1000);

# SBW
RM1primesbw = beta1*((beta3*exp(beta2-(beta3*(xm1-tb))/(tm-tb)))/((tm-tb)*(exp(beta2-(beta3*(xm1-tb))/(tm-tb))+1)^2)-exp(((xm1-tb)/(tm-tb)-1)/beta4)/(beta4*(tm-tb)))

# TREE
RM1primetree2 = -bf*exp(bf*(xm1-cf))/(1+exp(bf*(xm1-cf)))^2;
RM1primetree = RM1primetree2/30

sbwmaxindex = which(RM1primesbw==max(RM1primesbw))
treemaxindex = which(RM1primetree==max(RM1primetree))
sbwmax = xm1[sbwmaxindex]
treemax = xm1[treemaxindex]

# temperatures
MeanTemp = 6.9;  # Fredericton mean temp
AmpliTemp = 6.5; # Fredericton amplitude temp
phase = 200;  # Ottawa temperature phase
dt = 5; # dt in 1-hour intervals
t = seq(1,365,by=dt);  # days in a year

xmean1 = MeanTemp + AmpliTemp * cos(2*pi*(t-phase)/365);

xmean2 = (xmean1[1:41]/1000)-0.0006

# polygon
ki = 0.0038
x1 = 14
x2 = 14.9
y11 = 0.0039 + ki
y12 = 0.0044 + ki
y21 = 0.007 + ki
y22 = 0.0075 + ki
ts = 14.2

labs = c(expression(paste("c"[1])),expression(paste("t"[s])),expression(paste("c"[2])))

mypanel1 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(v = sbwmax, col='black',lty=2,lwd=3)
  panel.abline(v = treemax, col='grey60',lty=2,lwd=3)
  panel.text((x1+0.5),(0.0077+ki),expression(paste(Delta *'t')),cex=1.2)
  panel.text((x1-0.5),(0.0055+ki),expression(paste(Delta *'x')),cex=1.2)
  panel.text(24,0.011,'Temperature',cex=1.2)
  panel.text(11,0.01,'Warm spell',cex=1.2,col='grey70')
  panel.text(5.5,0.01,expression(paste("R'"[1]*'(x)')),cex=1.2,col='grey60')
  panel.text(22,0.009,expression(paste("R'"[2]*'(x)')),cex=1.2,col='black')
  panel.polygon(x=c(x1,x2,x2,x1),y=c(y11,y12,y22,y21),col='grey80',lty=0)
  panel.arrows(x0=11.2,x1=(x1-0.1),y0=(0.0095),y1=(0.008),length = 0,col='grey70',lwd=3)
  panel.abline(v = x1, col = 'grey60', lty = 2, lwd = 3)
  #panel.text(0.5,0.0005,'B',cex = 1.5)
}
p11 = xyplot(RM1primetree~xm1,type='l',col='grey60',lwd=3,
             xlab='Time',ylab=" ",xlim=c(0,27),ylim=c(0,0.012),
             panel = mypanel1,
             par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=1.9),par.ylab.text=list(cex=1.9),
                               axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
             scales=list(x=list(at=c(treemax,x1,sbwmax),labels=labs),y=list(draw=F))
)
p12 = xyplot(RM1primesbw~xm1,lwd=3,type='l',col='black')
p13 = xyplot(xmean2~seq(-10,30),type='l',col = 'black',lty = 3,lwd=2)

p1 = p11+as.layer(p12)+as.layer(p13)

setwd("~/Master_Uottawa/Presentations/Lab_Meeting_08_02_2021")
cairo_pdf('Warm_Spell_2.pdf',width = 16, height = 8)
print(p1, split = c(1,1,1,1), more = F)
dev.off()


#### anova ####
setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Predictions")
past = read.csv('4_hourly_Past_Insect.csv',colClasses = c('Site'='factor'))

mod1 = aov(past$Emergence~past$Site)
summary(mod1)

plot(mod1$residuals~mod1$fitted.values)
qqnorm(mod1$residuals)
qqline(mod1$residuals)
hist(mod1$residuals,nclass = 20)

TukeyHSD(mod1)
pairwise.t.test(past$Emergence,past$Site,p.adjust.method = 'bonferroni')

mod2 = aov(past$Emergence~past$Site)
summary(mod2)

plot(mod2$residuals~mod2$fitted.values)
qqnorm(mod2$residuals)
qqline(mod2$residuals)
hist(mod2$residuals,nclass = 20)

TukeyHSD(mod2)
pairwise.t.test(past$Budburst,past$Site,p.adjust.method = 'bonferroni')

#### graphical abstract ####
library(lattice)
library(latticeExtra)

x = seq(1,1500)
sigma = 70

mu1 = 300
fx11 = 1/(sigma*sqrt(2*pi))*exp(-0.5*((x-mu1)/sigma)^2)
fx1i = which(fx11>1e-5)
mu2 = 200
fx22 = 1/(sigma*sqrt(2*pi))*exp(-0.5*((x-mu2)/sigma)^2)
fx2i = which(fx22>1e-5)

mu3 = 1200
fx33 = 1/(sigma*sqrt(2*pi))*exp(-0.5*((x-mu3)/sigma)^2)
fx3i = which(fx33>1e-5)

mu4 = 1000
fx44 = 1/(sigma*sqrt(2*pi))*exp(-0.5*((x-mu4)/sigma)^2)
fx4i = which(fx44>1e-5)

mypanel2 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.text(150,7e-3,"Resource",col='blue',cex=2)
  panel.text(350,7e-3,"Consumer",col='red',cex=2)
  panel.text(1000,7e-3,"Resource",col='blue',cex=2)
  panel.text(1250,7e-3,"Consumer",col='red',cex=2)
  
}
pp44 = xyplot(fx11[fx1i]~fx1i,type='l',lwd=3,col='red',xlim=c(0,1400),ylim=c(0,0.008),
     xlab='Time',ylab='Abundance',cex.lab=2,
     panel = mypanel2,
     par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=2.5),par.ylab.text=list(cex=2.5),
                       axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
     scales=list(x=list(draw=F),y=list(draw=F))
     )
pp45 = xyplot(fx22[fx2i]~fx2i,type='l',col='blue',lwd=3)
pp46 = xyplot(fx33[fx3i]~fx3i,type='l',col='red',lwd=3)
pp47 = xyplot(fx44[fx4i]~fx4i,type='l',col='blue',lwd=3)
p2 = pp44+as.layer(pp45)+as.layer(pp46)+as.layer(pp47)


setwd("~/Budworm/Manuscript/JAE_Paper/Paper")
png('Graphabstract1.png', width = 900, height = 600)
print(p2)
dev.off()

# setwd("~/Budworm/Manuscript/JAE_Paper/Paper")
# png('Graphabstract1.png', width = 900, height = 600)
# par(mar=c(5,5,4,2))
# plot(fx11[fx1i]~fx1i,type='l',lwd=3,col='red',xlim=c(0,1400),ylim=c(0,0.008),
#      xlab='Time',ylab='Abundance',axes=F,cex.lab=2)
# lines(fx22[fx2i]~fx2i,col='blue',lwd=3)
# lines(fx33[fx3i]~fx3i,col='red',lwd=3)
# lines(fx44[fx4i]~fx4i,col='blue',lwd=3)
# box()
# text(150,7e-3,"Resource",col='blue',cex=1.5)
# text(350,7e-3,"Consumer",col='red',cex=1.5)
# text(1000,7e-3,"Resource",col='blue',cex=1.5)
# text(1250,7e-3,"Consumer",col='red',cex=1.5)
# dev.off()

t1 = 87;  # starting date for heat accumulation
bf = -1.32;
cf = 7.14;
Fb = 18.6;

# spruce budworm
beta1 = 0.194;
beta2 = 3.0;
beta3 = 5.84;
beta4 = 0.034;
tb = 2.5;
tm = 35;
t1s = 31+28

xm1 = seq(-30,35,length.out = 1000);

# SBW
RM1primesbw = beta1*((beta3*exp(beta2-(beta3*(xm1-tb))/(tm-tb)))/((tm-tb)*(exp(beta2-(beta3*(xm1-tb))/(tm-tb))+1)^2)-exp(((xm1-tb)/(tm-tb)-1)/beta4)/(beta4*(tm-tb)))

# TREE
RM1primetree2 = -bf*exp(bf*(xm1-cf))/(1+exp(bf*(xm1-cf)))^2;
RM1primetree = RM1primetree2/30

sbwmaxindex = which(RM1primesbw==max(RM1primesbw))
treemaxindex = which(RM1primetree==max(RM1primetree))
sbwmax = xm1[sbwmaxindex]
treemax = xm1[treemaxindex]

# temperatures
MeanTemp = 6.9;  # Fredericton mean temp
AmpliTemp = 6.5; # Fredericton amplitude temp
phase = 200;  # Ottawa temperature phase
dt = 5; # dt in 1-hour intervals
t = seq(1,365,by=dt);  # days in a year

xmean1 = MeanTemp + AmpliTemp * cos(2*pi*(t-phase)/365);

xmean2 = (xmean1[1:41]/1000)-0.0006

# polygon
x1 = treemax-0.1
x2 = treemax+0.8
y11 = 0.0039
y12 = 0.0044
y21 = 0.007
y22 = 0.0075

labs = c(expression(paste("c"[1]*" = t"[s])),expression(paste("c"[2])))

mypanel1 = function(x,y,...){
  panel.xyplot(x,y,...)
  #panel.abline(v = sbwmax, col='black',lty=2,lwd=3)
  #panel.abline(v = treemax, col='grey60',lty=2,lwd=3)
  #panel.text((x1+0.5),0.0077,expression(paste(Delta *'t')),cex=1.2)
  #panel.text((x1-0.5),0.0055,expression(paste(Delta *'x')),cex=1.2)
  #panel.text(24,0.011,'Temperature',cex=1.2)
  #panel.text(4,0.008,'Warm spell',cex=1.2,col='grey70')
  #panel.text(5.5,0.01,expression(paste("R'"[b]*'(x)')),cex=1.2,col='grey60')
  #panel.text(22,0.009,expression(paste("R'"[e]*'(x)')),cex=1.2,col='black')
  #panel.polygon(x=c(x1,x2,x2,x1),y=c(y11,y12,y22,y21),col='grey80',lty=0)
  #panel.arrows(x0=4,x1=(cf-0.1),y0=0.0075,y1=0.006,length = 0,col='grey70',lwd=3)
  #panel.text(0.5,0.0005,'B',cex = 1.5)
}
p11 = xyplot(RM1primetree~xm1,type='l',col='blue',lwd=3,
             xlab='Time',ylab="Sensitivity",xlim=c(0,27),ylim=c(0,0.012),
             panel = mypanel1,
             par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=2.5),par.ylab.text=list(cex=2.5),
                               axis.components=list(top=list(tck=0),right=list(tck=0),left=list(tck=0),bottom=list(tck=0))),
             scales=list(x=list(draw=F),y=list(draw=F))
)
p12 = xyplot(RM1primesbw~xm1,lwd=3,type='l',col='red')
#p13 = xyplot(xmean2~seq(-10,30),type='l',col = 'black',lty = 3,lwd=2)

p1 = p11+as.layer(p12)#+as.layer(p13)

setwd("~/Budworm/Manuscript/JAE_Paper/Paper")
png('Graphabstract2.png', width = 900, height = 600)
print(p1)
dev.off()


#### fitting goodness of fit ####
library(lattice)
library(latticeExtra)

setwd("C:/Users/sebca/Documents/Master_Uottawa/SBW_models/Fitting")
dat=read.csv('Budburst_Prov_Error.csv')

errsq = dat$Error^2
sumsq = sum(errsq)
rmsesq = sumsq/nrow(dat)
rmse = sqrt(rmsesq)

dat = dat[dat$Budburst>100,]

mypanel1 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(a = 0, b = 1, lwd =2.5)
  panel.text(108,104,'A',cex=2)
}
oplot = xyplot(Budburst~Predicted,data = dat,col='black',pch=20,
               xlim=c(100,181),ylim=c(100,181),
               xlab='Predicted budburst',ylab='Observed budburst',
               panel = mypanel1,
               par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=2.5),par.ylab.text=list(cex=2.5),
                                 axis.components=list(top=list(tck=0),right=list(tck=0))),
               scales=list(x=list(at=c(100,120,140,160,180),labels=c(100,120,140,160,180)),
                           y=list(at=c(100,120,140,160,180),labels=c(100,120,140,160,180)))
               )

dat=read.csv('Budburst_Prov_Error.csv')
mod1 = lm((Predicted-Budburst)~Predicted, data = dat)
summary(mod1)

mypanel2 = function(x,...){
  panel.histogram(x,...)
  panel.text(-93,2,'B',cex=2)
}
histo = histogram(dat$Error,breaks=50,xlim=c(-100,100),col='grey',
                  xlab='Residuals',ylab='Percentage from total',
                  panel = mypanel2,
                  par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=2.5),par.ylab.text=list(cex=2.5),
                                    axis.components=list(top=list(tck=0),right=list(tck=0)))
                  )

mypanel3 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.text(45.4,-90,'C',cex=2)
}
latiplot = xyplot(Error~Latitude,data = dat,ylim=c(-100,100),pch=20,col='black',
                  xlab='Latitude',ylab='Residuals',
                  panel = mypanel3,
                  par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=2.5),par.ylab.text=list(cex=2.5),
                                    axis.components=list(top=list(tck=0),right=list(tck=0))),
                  scales=list(x=list(at=c(46,47,48,49),labels=c('46°N','47°N','48°N','49°N')),
                              y=list(at=c(-100,-50,0,50,100),labels=c(-100,-50,0,50,100)))
                  )

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1/JAE_Paper/Paper/Supplements")
cairo_pdf('Fitting_Plots.pdf',height = 12,width = 14)
par(mar=c(5,5,4,1))
print(oplot,split=c(1,1,2,2),more = T)
print(histo,split=c(2,1,2,2),more = T)
print(latiplot,split=c(1,2,2,2),more = F)
dev.off()

#### sensitivity ####
library(lattice)
library(latticeExtra)

setwd('C:/Users/sebca/Documents/Master_Uottawa/SBW_models/Sensitivity')
prccbudworm = read.csv('PRCC_Budworm_Model.csv')

prcc = prccbudworm[,1]
nam = c(expression(paste(beta[1])),expression(paste(beta[2])),expression(paste(beta[3])),expression(paste(beta[4])),
        expression(paste('x'[b])),expression(paste('x'[m])))
x0 = seq(1,6)
y0 = rep(0,6)
x1 = x0
y1 = prcc

prcctree = read.csv('PRCC_Uniforc_Model.csv')

prcc2 = prcctree[,1]
nam2 = c(expression(paste('b'[f])), expression(paste('c'[f])),expression(paste('F'*'*')),expression(paste('t'[1])))
x02 = seq(1,4)
y02 = rep(0,4)
x12 = x02
y12 = prcc2

mypanel1 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(h=0,lwd=1)
  panel.segments(x0,y0,x1,y1,lwd=2)
  panel.text(0.7,-0.9,'A',cex=1.5)
}
p1 = xyplot(0~0,type='n',xlim=c(0.5,6.5),ylim=c(-1,1),
            xlab = 'Parameters',ylab = 'PRCC',
            panel = mypanel1,
            par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=2.5),par.ylab.text=list(cex=2.5),
                              axis.components=list(top=list(tck=0),right=list(tck=0))),
            scales=list(x=list(at=seq(1,6),labels=nam),
                        y=list(at=c(-1.0,-0.5,0,0.5,1.0),labels=c(-1.0,-0.5,0,0.5,1.0)))
            )

mypanel2 = function(x,y,...){
  panel.xyplot(x,y,...)
  panel.abline(h=0,lwd=1)
  panel.segments(x02,y02,x12,y12,lwd=2)
  panel.text(0.6,-0.9,'B',cex=1.5)
}
p2 = xyplot(0~0,type='n',xlim=c(0.5,4.5),ylim=c(-1,1),
            xlab = 'Parameters',ylab = 'PRCC',
            panel = mypanel2,
            par.settings=list(axis.text=list(cex=1.5),par.xlab.text=list(cex=2.5),par.ylab.text=list(cex=2.5),
                              axis.components=list(top=list(tck=0),right=list(tck=0))),
            scales=list(x=list(at=seq(1,4),labels=nam2),
                        y=list(at=c(-1.0,-0.5,0,0.5,1.0),labels=c(-1.0,-0.5,0,0.5,1.0)))
            )

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1/JAE_Paper/Paper/Supplements")
cairo_pdf('Sensitivity.pdf',height = 12, width = 9)
print(p1,split = c(1,1,1,2),more=T)
print(p2,split = c(1,2,1,2),more=F)
dev.off()


#### plots per decade ####
library(lattice)
library(latticeExtra)

setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Predictions")
#past = read.csv('4_hourly_Past_Insect.csv',colClasses = c('Site'='factor'))

rcp26 = read.csv('4_hourly_Predicted_RCP_26_Insect.csv',colClasses = c('Site'='factor'))
rcp45 = read.csv('4_hourly_Predicted_RCP_45_Insect.csv',colClasses = c('Site'='factor'))
rcp85 = read.csv('4_hourly_Predicted_RCP_85_Insect.csv',colClasses = c('Site'='factor'))
model26 = rep('RCP26',nrow(rcp26))
rcp26$Model = model26
model45 = rep('RCP45',nrow(rcp45))
rcp45$Model = model45
model85 = rep('RCP85',nrow(rcp85))
rcp85$Model = model85

total = rbind(rcp26,rcp45,rcp85)
rm(rcp26,rcp45,rcp85,model26,model45,model85)

Decade = rep(0,nrow(total))
for (i in 1:nrow(total)){
  if (total$Year[i]<151){
    Decade[i] = 'D1'
  }
  if (total$Year[i]>150 && total$Year<301){
    Decade[i] = 'D2'
  }
  if (total$Year[i]>300 && total$Year<451){
    Decade[i] = 'D3'
  }
  if (total$Year[i]>450 && total$Year<601){
    Decade[i] = 'D4'
  }
  if (total$Year[i]>600 && total$Year<751){
    Decade[i] = 'D5'
  }
  if (total$Year[i]>750 && total$Year<901){
    Decade[i] = 'D6'
  }
  if (total$Year[i]>900 && total$Year<1051){
    Decade[i] = 'D7'
  }
  if (total$Year[i]>1050 && total$Year<1201){
    Decade[i] = 'D8'
  }
}

total$Decade = Decade

mycolors = c('pink','red','darkred')
trellis.par.set(superpose.symbol = list(fill = mycolors,col=mycolors))
b1 = bwplot(Emergence~Decade, data = total, groups = Model,
       xlab = 'Decade',ylab = 'Emergence date (Julian days)',
       pch = "|", box.width = 1/6,
       panel = panel.superpose,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=1.3),par.xlab.text=list(cex=2.5),par.ylab.text=list(cex=2.5),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2),
                         superpose.symbol = list(fill = mycolors,col=mycolors)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1,7.1,8.1),labels=c('20/30','30/40','40/50','50/60','60/70','70/80','80/90','90/100'),
                          alternating=1))
)

mycolors = c('pink','red','darkred')
trellis.par.set(superpose.symbol = list(fill = mycolors,col=mycolors))
b2 = bwplot(Budburst~Decade, data = total, groups = Model,
       xlab = 'Decade',ylab = 'Budburst date (Julian days)',
       pch = "|", box.width = 1/6,
       panel = panel.superpose,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=1.3),par.xlab.text=list(cex=2.5),par.ylab.text=list(cex=2.5),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2),
                         superpose.symbol = list(fill = mycolors,col=mycolors)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1,7.1,8.1),labels=c('20/30','30/40','40/50','50/60','60/70','70/80','80/90','90/100'),
                          alternating=1))
)

mycolors = c('pink','red','darkred')
trellis.par.set(superpose.symbol = list(fill = mycolors,col=mycolors))
b3 = bwplot(Mismatch~Decade, data = total, groups = Model,
       xlab = 'Decade',ylab = 'Mismatch (Julian days)',
       pch = "|", box.width = 1/6,
       panel = panel.superpose,
       panel.groups = function(x, y,..., group.number){ 
         panel.bwplot(x + (group.number-1.5)/6, y ,...)},
       par.settings=list(axis.text=list(cex=1.3),par.xlab.text=list(cex=2.5),par.ylab.text=list(cex=2.5),
                         axis.components=list(top=list(tck=0),right=list(tck=0)),plot.symbol=list(col='black',pch=1),
                         box.umbrella=list(lty=1,col='black',lwd=2),box.rectangle=list(col='black',lwd=2),
                         superpose.symbol = list(fill = mycolors,col=mycolors)),
       scales=list(x=list(at=c(1.1,2.1,3.1,4.1,5.1,6.1,7.1,8.1),labels=c('20/30','30/40','40/50','50/60','60/70','70/80','80/90','90/100'),
                          alternating=1))
)


setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1/JAE_Paper/Paper/Supplements")
pdf('Trends_Per_Decade.pdf',height = 14, width = 16)
print(b1,split=c(1,1,2,2),more=T)
print(b2,split=c(2,1,2,2),more=T)
print(b3,split=c(1,2,2,2),more=F)
dev.off()


