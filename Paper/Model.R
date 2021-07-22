# Model with 4 hours temperatures
# by Sebastien Portalier
# 05 December 2020

library(dplyr)
library(sf)
library(daymetr)

library(chillR)
library(foreach)
library(doParallel)

#### refit tree model ####
setwd("~/Master_Uottawa/SBW_models/Fitting")
# 1983
bud1983 = read.csv('Budburst_bf_1983.csv')
loco = bud1983[,1:3]
write.csv(loco,'locations_1983_New.csv',quote = F,row.names = F)
Weather.1983 <- download_daymet_batch(file_location = "locations_1983_New.csv",start = 1983,end = 1983,internal = TRUE,silent = TRUE)
# extract temperatures
totdat = list()

for (i in 1:85){
  local = Weather.1983[[i]]
  localdat = local$data
  temp = localdat[,c(1,2,7,8)]
  site = rep(i,nrow(temp))
  latitude = rep(local$latitude,nrow(temp))
  if (nrow(temp)==365){
    vecmonth = c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
    vecday = c(seq(1,31),seq(1,28),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31))
    
  }else{
    vecmonth = c(rep(1,31),rep(2,29),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
    vecday = c(seq(1,31),seq(1,29),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31))
    
  }
  tottemp = data.frame("Site"=site,'Latitude'=latitude,"Year"=temp$year,'Month'=vecmonth,'Day'=vecday,'Tmax'=temp$tmax..deg.c.,'Tmin'=temp$tmin..deg.c.)
  totdat[[i]] = tottemp
}

meanresults1983 = do.call('rbind',totdat)

write.csv(meanresults1983,'Temperatures1983_New.csv', quote = F, row.names = F)

# 2000
Weather.2000 <- download_daymet_batch(file_location = "locations_2000.csv",start = 2000,end = 2000,internal = TRUE,silent = TRUE)

tot = Weather.2000[[1]]
totdat = tot$data
vecmonth = c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
vecday = c(seq(1,31),seq(1,28),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31))
vecmonth = c(vecmonth,vecmonth)
vecday = c(vecday,vecday)
year = c(rep(1999,365),rep(2000,365))
latitude = rep(tot$latitude,730)
site = rep(1,730)
titi = data.frame("Site"=site,'Latitude'=latitude,"Year"=year,'Month'=vecmonth,'Day'=vecday,'Tmax'=totdat$tmax..deg.c.,'Tmin'=totdat$tmin..deg.c.)

write.csv(titi,'Temperatures2000_New.csv', quote = F, row.names = F)

# Provincial
dat = read.csv('Budburst_Prov.csv')

loc = dat[,1:3]
year = dat[,5]
#previousyear = year-1

temp = list()
for (i in 1:nrow(dat)){
  loco = loc[i,]
  write.csv(loco,"locations_loco.csv",row.names = FALSE,quote = FALSE)
  deb = year[i]
  Weather.prov = download_daymet_batch(file_location = "locations_loco.csv",start = deb,end = deb,internal = TRUE,silent = TRUE)
  res = Weather.prov[[1]] 
  temp[[i]] = res$data
  print(i)
}

temperature = list()

for (i in 1:nrow(dat)){
  totdat = temp[[i]]
  latitude = rep(dat$Latitude[i],nrow(totdat))
  site = rep(i,nrow(totdat))
  if (nrow(totdat)==365){
    vecmonth = c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
    vecday = c(seq(1,31),seq(1,28),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31))
    
  }else{
    vecmonth = c(rep(1,31),rep(2,29),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
    vecday = c(seq(1,31),seq(1,29),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31))
    
  }
  tottemp = data.frame("Site"=site,'Latitude'=latitude,"Year"=totdat$year,'Month'=vecmonth,'Day'=vecday,'Tmax'=totdat$tmax..deg.c.,'Tmin'=totdat$tmin..deg.c.)
  temperature[[i]] = tottemp
}
temperature = do.call('rbind',temperature)

write.csv(temperature,'Temperatures_Prov_new.csv',quote = F,row.names = F)

# data
genetemp = function(x){
  Lat = x[1,"Latitude"]
  t1 = x[,c("Year","Month","Day","Tmax","Tmin")]
  weather = fix_weather(t1)
  hourtemps = stack_hourly_temps(weather, latitude=Lat)$hourtemps
  res = data.frame(KeyID=x[1,"Site"],hourtemps)
  res = res[seq(1,nrow(res),4),]
  return(res)
}

provbud = read.csv('Budburst_Prov.csv')
provtemp = read.csv('Temperatures_Prov_New.csv')

tempprov = by(provtemp,provtemp$Site,genetemp,simplify = F)

# tempprov = list()
# index = 0
# for (i in 1:1354){
#   deb = index + 1
#   fin = index + 365
#   index= fin
#   tempprov[[i]] = provtemp[deb:fin,]
# }

bud1983 = read.csv('Budburst_bf_1983.csv')
temp1983 = read.csv('Temperatures1983_New.csv')
tempprov2 = by(temp1983,temp1983$Site,genetemp,simplify = F)

bud2000 = read.csv('Budburst_bf_2000.csv')
temp2000 = read.csv('Temperatures2000_New.csv')
tempprov3 = by(temp2000,temp2000$Site & temp2000$Year,genetemp,simplify = F)

indextot = 1355
for (i in 1:85){
  tempprov[[indextot]] = tempprov2[[i]]
  indextot = indextot+1
}
tempprov[[indextot]] = tempprov3[[1]]

expectedbudburst = c(provbud$Budburst,bud1983$Budburst,bud2000$Budburst)

rm(indextot)

# fitting
fitfunc = function(x){
  t1 =  x[1] #24 # days since 1st March #205 # days since 1st September
  bf = x[2] #-0.1789
  cf = x[3] #14.63
  fstar = x[4] #10.86
  
  predictedbudburst = rep(0,length(expectedbudburst))
  
  for (i in 1:length(expectedbudburst)){
    local = tempprov[[i]]
    temperature = local$Temp
    beg = t1*6
    temp=temperature[beg:length(temperature)]
    
    rft = 1/(1+exp(bf*(temp-cf)))/6
    
    ft = cumsum(rft)
    
    index = which(ft>=fstar)
    budburst = index[1]/6
    realdate = budburst+t1
    predictedbudburst[i] = realdate
  }
  
  # Error
  localerr = (predictedbudburst - expectedbudburst)^2
  err = sum(localerr)
  return(err)
}

t10 = 84
bf0 = -0.1936
cf0 = 10
fstar0 = 13

param = c(t10,bf0,cf0,fstar0)

res = optim(par = param, fn = fitfunc, method = 'Nelder-Mead', control = list(maxit = 10000, reltol = 1e-9))

res
# t1 = 87.36, bf = -1.32, cf = 7.14, fstar = 18.6
# error: 230171.4

param = c(t10,bf0,cf0,fstar0)

res = optim(par = param, fn = fitfunc, method = 'SANN', control = list(maxit = 10000, reltol = 1e-9))

res
# t1 = 84.29, bf = -0.12, cf = 12.5, fstar = 17.71
# error: 221963.9

#### generate temperature data ####
genetemp = function(x){
  Lat = x[1,"Latitude"]
  t1 = x[,c("Year","Month","Day","Tmax","Tmin")]
  weather = chillR::fix_weather(t1)
  hourtemps = chillR::stack_hourly_temps(weather, latitude=Lat)$hourtemps
  res = data.frame(KeyID=x[1,"KeyID"],hourtemps)
  res = res[seq(1,nrow(res),4),]
  return(res)
}

setwd("~/Master_Uottawa/SBW_models/Budworm_data")
dat = read.csv('BioSim_Temperatures_Min_Mean_Max.csv')

newdat = by(dat,list(dat$KeyID,dat$Year),genetemp,simplify = F)
newdat = do.call("rbind",newdat)

setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures")
datanew = newdat[,c('KeyID','Year','Month','Day','Hour','Temp')]
write.csv(datanew,'3_Hourly_Past_BioSim.csv',quote = F,row.names = F)

#### model past ####
setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Temperatures")
dat = read.csv('3_Hourly_Past_BioSim.csv')

# L2o 
# 1st March
beta1 = 0.194
beta2 = 3.0
beta3 = 5.84
beta4 = 0.034
tb = 2.5
tm = 35

# bf provincial
t1 = 87.36 #84
bf = -1.32 #-0.12
cf = 7.14 #12.5
fstar = 18.6 #17.71

### temperature
years = levels(as.factor(dat$Year))
numyears = length(years)
namesites = levels(as.factor(dat$KeyID))
numsites = 6
simunumb = numyears*numsites


cl = makeCluster(6)
registerDoParallel(cl)

res = foreach (k = 1:numsites) %dopar% {
  
  currentsite = subset(dat,dat$KeyID==namesites[k])
  currentsite = droplevels(currentsite)
  
  results = matrix(nrow=numyears,ncol=5)
  results = as.data.frame(results)
  names(results) = c('Year','Site','Emergence','Budburst','Mismatch')
  
  indexresults = 1
  
  # per year
  for (w in 1:numyears){
    currentyear = subset(currentsite,currentsite$Year==years[w])
    currentyear = droplevels(currentyear)
    
    temperature = subset(currentyear,currentyear$Month>=3)
    temperature = droplevels(temperature)
    
    temperatureL2 = temperature$Temp
    
    # larvae
    rt2 = rep(0,length(temperatureL2))
    for (i in 1:length(temperatureL2)){
      if (temperatureL2[i] >= tb && temperatureL2[i] <= tm){
        tau = (temperatureL2[i]-tb)/(tm-tb)
        expon = beta2-beta3*tau
        rt2[i] = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))/6
      }else{
        rt2[i] = 0
      }
    }
    
    rt2tot = cumsum(rt2)
    index = which(rt2tot>=1)
    moulting = index[1]/6
    emergence = moulting + 58
    
    # from tree
    temperature = currentyear$Temp
    beg = t1*6
    temp=temperature[beg:length(temperature)]
    
    rft = 1/(1+exp(bf*(temp-cf)))/6
    
    ft = cumsum(rft)
    
    index = which(ft>=fstar)
    budburst = index[1]/6
    realdate = budburst+t1
    
    results[indexresults,1] = w
    results[indexresults,2] = namesites[k]
    results[indexresults,3] = emergence
    results[indexresults,4] = realdate
    results[indexresults,5] = emergence - realdate
    indexresults = indexresults+1
  }
  results
}

stopImplicitCluster()
stopCluster(cl)

res = do.call("rbind",res)

setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Predictions")
write.csv(res,'4_hourly_Past_Insect_2.csv',quote = F,row.names = F)

#### model future ####
# L2o 
# 1st March
beta1 = 0.194
beta2 = 3.0
beta3 = 5.84
beta4 = 0.034
tb = 2.5
tm = 35

# bf provincial
t1 = 87.36 #84
bf = -1.32 #-0.12
cf = 7.14 #12.5
fstar = 18.6 #17.71

genetemp = function(x){
  Lat = x[1,"Latitude"]
  t1 = x[,c("Year","Month","Day","Tmax","Tmin")]
  weather = chillR::fix_weather(t1)
  hourtemps = chillR::stack_hourly_temps(weather, latitude=Lat)$hourtemps
  res = data.frame(KeyID=x[1,"KeyID"],hourtemps)
  res = res[seq(1,nrow(res),4),]
  return(res)
}

setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Temperatures")
# data
temp1 = read.csv('2001_2030_RCM_4_RCP_45.csv')
temperature1 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2011_2040_RCM_4_RCP_45.csv')
temperature2 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2021_2050_RCM_4_RCP_45.csv')
temperature3 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2031_2060_RCM_4_RCP_45.csv')
temperature4 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2041_2070_RCM_4_RCP_45.csv')
temperature5 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2051_2080_RCM_4_RCP_45.csv')
temperature6 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2061_2090_RCM_4_RCP_45.csv')
temperature7 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2071_2100_RCM_4_RCP_45.csv')
temperature8 = temp1[,c(2,3,7,9)]
rm(temp1)

# temp1 = read.csv('2001_2030_Hadley_RCP_26.csv')
# temperature1 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2011_2040_Hadley_RCP_26.csv')
# temperature2 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2021_2050_Hadley_RCP_26.csv')
# temperature3 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2031_2060_Hadley_RCP_26.csv')
# temperature4 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2041_2070_Hadley_RCP_26.csv')
# temperature5 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2051_2080_Hadley_RCP_26.csv')
# temperature6 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2061_2090_Hadley_RCP_26.csv')
# temperature7 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2071_2100_Hadley_RCP_26.csv')
# temperature8 = temp1[,c(2,3,7,9)]
# rm(temp1)

tempdat = rbind(temperature1,temperature2,temperature3,temperature4,temperature5,temperature6,temperature7,temperature8)
rm(temperature1,temperature2,temperature3,temperature4,temperature5,temperature6,temperature7,temperature8)
names(tempdat) = c("KeyID","Latitude","Tmin","Tmax")

newdat = by(tempdat,tempdat$KeyID,function(x){
  numyears = nrow(x)/365
  yeartype = seq(1,numyears)
  monthtype = seq(1,12)
  Year = NULL
  Month = NULL
  Day = NULL
  for (i in 1:numyears){
    vecyear = rep(yeartype[i],365)
    vecmonth = c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
    vecday = c(seq(1,31),seq(1,28),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31))
    Year = c(Year,vecyear)
    Month = c(Month,vecmonth)
    Day = c(Day,vecday)
  }
  data.frame(KeyID=x[,"KeyID"],Latitude=x[,'Latitude'],Year = Year, Month = Month, Day = Day, Tmax = x[,'Tmax'], Tmin = x[,'Tmin'])
  
},simplify = F)

dat = do.call("rbind",newdat)
rm(tempdat,newdat)

years = levels(as.factor(dat$Year))
numyears = length(years)
namesites = levels(as.factor(dat$KeyID))
numsites = 6
simunumb = numyears*numsites


cl = makeCluster(6)
registerDoParallel(cl)

res = foreach (k = 1:numsites) %dopar% {
  
  currentsite = subset(dat,dat$KeyID==namesites[k])
  currentsite = droplevels(currentsite)
  
  results = matrix(nrow=numyears,ncol=5)
  results = as.data.frame(results)
  names(results) = c('Year','Site','Emergence','Budburst','Mismatch')
  
  indexresults = 1
  
  # per year
  for (w in 1:numyears){
    currentyear = subset(currentsite,currentsite$Year==years[w])
    currentyear = droplevels(currentyear)
    currentyearhour = genetemp(currentyear)
    
    temperature = subset(currentyearhour,currentyearhour$Month>=3)
    temperature = droplevels(temperature)
    
    temperatureL2 = temperature$Temp
    
    # larvae
    rt2 = rep(0,length(temperatureL2))
    for (i in 1:length(temperatureL2)){
      if (temperatureL2[i] >= tb && temperatureL2[i] <= tm){
        tau = (temperatureL2[i]-tb)/(tm-tb)
        expon = beta2-beta3*tau
        rt2[i] = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))/6
      }else{
        rt2[i] = 0
      }
    }
    
    rt2tot = cumsum(rt2)
    index = which(rt2tot>=1)
    moulting = index[1]/6
    emergence = moulting + 58
    
    # from tree
    temperature = currentyearhour$Temp
    beg = t1*6
    temp=temperature[beg:length(temperature)]
    
    rft = 1/(1+exp(bf*(temp-cf)))/6
    
    ft = cumsum(rft)
    
    index = which(ft>=fstar)
    budburst = index[1]/6
    realdate = budburst+t1
    
    results[indexresults,1] = w
    results[indexresults,2] = namesites[k]
    results[indexresults,3] = emergence
    results[indexresults,4] = realdate
    results[indexresults,5] = emergence - realdate
    indexresults = indexresults+1
  }
  results
}

stopImplicitCluster()
stopCluster(cl)

res = do.call("rbind",res)

setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Predictions")
write.csv(res,'4_hourly_Predicted_RCP_45_Insect_2.csv',quote = F,row.names = F)

#### mismatch distribution ####
setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Predictions")

rcp26 = read.csv('4_hourly_Predicted_RCP_26_Insect_2.csv')
m26 = ifelse(rcp26$Mismatch < (-8),1,0)
rcp45 = read.csv('4_hourly_Predicted_RCP_45_Insect_2.csv')
m45 = ifelse(rcp45$Mismatch < (-8),1,0)
rcp85 = read.csv('4_hourly_Predicted_RCP_85_Insect_2.csv')
m85 = ifelse(rcp85$Mismatch < (-8),1,0)
now = read.csv('4_hourly_Past_Insect_2.csv')
m0 = ifelse(now$Mismatch < (-8),1,0)

m26seq = which(m26>0)

k = 0
v = m26seq[1]
m26cons = NULL
for (i in m26seq[-1]){
  if (i==(v+1)){
    k = k+1
  }else{
    m26cons = c(m26cons,k)
    k = 0
  }
  v = i
}

m45seq = which(m45>0)

k = 0
v = m45seq[1]
m45cons = NULL
for (i in m45seq[-1]){
  if (i==(v+1)){
    k = k+1
  }else{
    m45cons = c(m45cons,k)
    k = 0
  }
  v = i
}

m85seq = which(m85>0)

k = 0
v = m85seq[1]
m85cons = NULL
for (i in m85seq[-1]){
  if (i==(v+1)){
    k = k+1
  }else{
    m85cons = c(m85cons,k)
    k = 0
  }
  v = i
}

m0seq = which(m0>0)

k = 0
v = m0seq[1]
m0cons = NULL
for (i in m0seq[-1]){
  if (i==(v+1)){
    k = k+1
  }else{
    m0cons = c(m0cons,k)
    k = 0
  }
  v = i
}

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")

png('RCP26_Badyears.png',height = 480, width = 800)
hist(m26cons[m26cons>0],nclass = 40,axes=F,main = 'RCP 26',xlab = '# of consecutive bad years',ylab = 'Occurence',cex.lab = 1.5)
axis(1,at=c(1.1,1.9,2.9,3.9,4.9,5.9,6.9),labels = c('1','2','3','4','5','6','7'))
axis(2)
box()
dev.off()

png('RCP45_Badyears.png',height = 480, width = 800)
hist(m45cons[m45cons>0],nclass = 40,axes=F,main = 'RCP 45',xlab = '# of consecutive bad years',ylab = 'Occurence',cex.lab = 1.5)
axis(1,at=c(1.05,1.95,2.95,3.95),labels = c('1','2','3','4'))
axis(2)
box()
dev.off()

png('RCP85_Badyears.png',height = 480, width = 800)
hist(m85cons[m85cons>0],nclass = 40,axes=F,main = 'RCP 85',xlab = '# of consecutive bad years',ylab = 'Occurence',cex.lab = 1.5)
axis(1,at=c(1.05,1.95,2.95,3.95),labels = c('1','2','3','4'))
axis(2)
box()
dev.off()

indexref = seq(1,1200,by = 150)

res = list()

results = matrix(nrow = 48, ncol = 4)
results = as.data.frame(results)
names(results) = c('Site','Decade','Badyears','RCP')

index = 1
for (i in 1:6){
  site = sprintf('Site%d',i)
  currentsite = subset(rcp26,rcp26$Site==site)
  currentsite = droplevels(currentsite)
  for (j in 1:8){
    deb = indexref[j]
    fin = deb+149
    currentdecade = currentsite[deb:fin,]
    m26 = ifelse(currentdecade$Mismatch < (-8),1,0)
    results[index,1] = site
    results[index,2] = j
    results[index,3] = sum(m26)/150*100
    results[index,4] = 'RCP26'
    index = index+1
  }
}
res[[1]] = results

results = matrix(nrow = 48, ncol = 4)
results = as.data.frame(results)
names(results) = c('Site','Decade','Badyears','RCP')

index = 1
for (i in 1:6){
  site = sprintf('Site%d',i)
  currentsite = subset(rcp45,rcp45$Site==site)
  currentsite = droplevels(currentsite)
  for (j in 1:8){
    deb = indexref[j]
    fin = deb+149
    currentdecade = currentsite[deb:fin,]
    m26 = ifelse(currentdecade$Mismatch < (-8),1,0)
    results[index,1] = site
    results[index,2] = j
    results[index,3] = sum(m26)/150*100
    results[index,4] = 'RCP45'
    index = index+1
  }
}
res[[2]] = results

results = matrix(nrow = 48, ncol = 4)
results = as.data.frame(results)
names(results) = c('Site','Decade','Badyears','RCP')

index = 1
for (i in 1:6){
  site = sprintf('Site%d',i)
  currentsite = subset(rcp85,rcp85$Site==site)
  currentsite = droplevels(currentsite)
  for (j in 1:8){
    deb = indexref[j]
    fin = deb+149
    currentdecade = currentsite[deb:fin,]
    m26 = ifelse(currentdecade$Mismatch < (-8),1,0)
    results[index,1] = site
    results[index,2] = j
    results[index,3] = sum(m26)/150*100
    results[index,4] = 'RCP85'
    index = index+1
  }
}
res[[3]] = results

lab = c('2020-2030','2030-2040','2040-2050','2050-2060','2060-2070','2070-2080','2080-2090','2090-2100')
colo = c('black','red','blue','orange','green','violet')

setwd('C:/Users/sebca/Documents/Master_Uottawa/SBW_models/Manuscripts/Paper_1')

png('Badyears.png',height = 480,width = 800)
par(mar=c(5,5,4,2))
plot(0,0,type='n',xlim=c(0.5,8.5),ylim=c(0,50),xlab='Decades',ylab='% of bad years',axes=F,cex.lab=1.5)
axis(1,at=seq(1,8),labels=lab)
axis(2)
box()
for (i in 1:3){
  results = res[[i]]
  for (j in 1:6){
    site = sprintf('Site%d',j)
    currentsite = subset(results,results$Site==site)
    currentsite = droplevels(currentsite)
    lines(currentsite$Badyears~currentsite$Decade,lty=i,lwd=2,col=colo[j])
  }
}
legend(1,49.5,c('44.5','45.5','46.5','47.5','48.5','49.5'),col=colo,lty=1,bty='n')
legend(2.5,49.5,c('RCP26','RCP45','RCP85'),lty=c(1,2,3),lwd=2,bty='n')
dev.off()

m0=rep(0,6)
for (i in 1:6){
  site = sprintf('Site%d',i)
  currentsite = subset(now,now$Site==site)
  currentsite = droplevels(currentsite)
  toto = ifelse(currentsite$Mismatch < (-8),1,0)
  m0[i] = sum(toto)
}
m0 = m0/21*100

#### test Frithjof ####
library(weathercan)
vovo = stations_search('Ottawa')
gat=weather_dl(station_ids = 53001, start = '2016-01-01', end = '2016-12-31')
realtemp = gat$temp
daytemp = rep(0,365)
index = seq(1,length(realtemp),by=24)
for (i in 1:365){
  deb = index[i]
  fin = deb+23
  daytemp[i] = mean(realtemp[deb:fin])
}

opt = function(x){
  M = x[1]
  A = x[2]
  te = seq(0,364)
  phi = 200
  
  temperature = M+A*cos(2*pi*(te-phi)/365)
  errsq = (temperature - daytemp)^2
  err = sum(errsq)
  return(err)
}

x0 = c(6,14)
res = optim(x0,opt,control = list(maxit=1000,abstol=1e-9,reltol=1e-9))

realtemp = gat$temp
daymin = rep(0,365)
daymax = rep(0,365)
index = seq(1,length(realtemp),by=24)
for (i in 1:365){
  deb = index[i]
  fin = deb+23
  daymin[i] = min(realtemp[deb:fin])
  daymax[i] = max(realtemp[deb:fin])
}

vecmonth = c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
vecday = c(seq(1,31),seq(1,28),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31))

toto = data.frame('Site'=1,'Latitude'= 45.52,'Year'=2016,'Month'=vecmonth,'Day'=vecday,'Tmin'=daymin,'Tmax'=daymax)

### simulations
library(chillR)

# data
genetemp = function(x){
  Lat = x[1,"Latitude"]
  t1 = x[,c("Year","Month","Day","Tmax","Tmin")]
  weather = fix_weather(t1)
  hourtemps = stack_hourly_temps(weather, latitude=Lat)$hourtemps
  res = data.frame(KeyID=x[1,"Site"],hourtemps)
  res = res[seq(1,nrow(res),4),]
  return(res)
}

# hourtemp = genetemp(toto)


avetemp = seq(6.9,10,by=0.01)
te = seq(0,364)
A = 15.5
phi = 200
M = 6.9

# bf provincial
t1 = 87.36 #84
bf = -1.32 #-0.12
cf = 7.14 #12.5
fstar = 18.6 #17.71

# L2o 
# 1st March
beta1 = 0.194
beta2 = 3.0
beta3 = 5.84
beta4 = 0.034
tb = 2.5
tm = 35

vecmonth = c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
vecday = c(seq(1,31),seq(1,28),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31),seq(1,31),seq(1,30),seq(1,31),seq(1,30),seq(1,31))

results = matrix(nrow = length(avetemp),ncol=4)
results = as.data.frame(results)
names(results) = c('M','Emergence','Budburst','Mismatch')

indexresults = 1
for (w in 1:length(avetemp)){
  M = avetemp[w]
  meantemperature = M+A*cos(2*pi*(te-phi)/365)
  daymin = meantemperature-5.6
  daymax = meantemperature+5.6
  totemp = data.frame('Site'=1,'Latitude'= 45.52,'Year'=2016,'Month'=vecmonth,'Day'=vecday,'Tmin'=daymin,'Tmax'=daymax)
  hourtemp = genetemp(totemp)
  
  temperature = subset(hourtemp,hourtemp$Month>=3)
  temperature = droplevels(temperature)
  
  temperatureL2 = temperature$Temp
  
  # larvae
  rt2 = rep(0,length(temperatureL2))
  for (i in 1:length(temperatureL2)){
    if (temperatureL2[i] >= tb && temperatureL2[i] <= tm){
      tau = (temperatureL2[i]-tb)/(tm-tb)
      expon = beta2-beta3*tau
      rt2[i] = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))/6
    }else{
      rt2[i] = 0
    }
  }
  
  rt2tot = cumsum(rt2)
  index = which(rt2tot>=1)
  moulting = index[1]/6
  emergence = moulting + 58
  
  # from tree
  temperature = hourtemp$Temp
  beg = t1*6
  temp=temperature[beg:length(temperature)]
  
  rft = 1/(1+exp(bf*(temp-cf)))/6
  
  ft = cumsum(rft)
  
  index = which(ft>=fstar)
  budburst = index[1]/6
  realdate = budburst+t1
  
  results[indexresults,1] = M
  results[indexresults,2] = emergence
  results[indexresults,3] = realdate
  results[indexresults,4] = emergence - realdate
  indexresults = indexresults+1
}

setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Predictions")
write.csv(results,'Theoretical_Test.csv',quote = F,row.names = F)

results = read.csv('Theoretical_Test.csv')
mod1 = lm(results$Emergence~results$M)
mod2 = lm(results$Budburst~results$M)

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
png('Linear_Approximation.png',width = 800, height = 480)
plot(results$Emergence~results$M,ylim=c(116,130),pch=20,xlab='dx',ylab='t*',cex.lab=1.5,axes=F)
axis(1,at=seq(6.9,9.9,by=0.5),labels=seq(0,3,by=0.5),cex.axis=1.2)
axis(2,cex.axis=1.2)
box()
points(results$Budburst~results$M,col='red',pch=20)
legend(9,129,c('t* (tree)','t* (insect)'),lty=c(1,1),lwd=2,col=c('red','black'),bty='n')
dev.off()

abline(mod1$coef,lwd=1,lty=2)
abline(mod2$coef,lwd=1,lty=2,col='red')
legend(9,129,c('t* (tree)','t* (insect)','tree linear approximation','insect linear approximation'),lty=c(1,1,2,2),lwd=2,col=c('red','black','red','black'),bty='n')

# linear tree
# tstar0 = results$Budburst[1]
# deri = - bf*exp(bf*(6.9-cf))/(1+exp(bf*(6.9-cf)))^2
# res = tstar0 + deri*(avetemp-6.9)
# 
# for (i in 1:length(avetemp)){
#   x = avetemp[i]-6.9
#   deri = - bf*exp(bf*(x-cf))/(1+exp(bf*(x-cf)))^2
#   res[i] = tstar0 + deri*x
# }

#### dI/dR = t*-t1
#### dI/dt* = R(t*)

## tree
#didr = tstar0-t1
tstar0 = results$Budburst[1]
meantemperature = M+A*cos(2*pi*(te-phi)/365)
didr = 1/(1+exp(bf*(meantemperature[tstar0]-cf)))-1/(1+exp(bf*(meantemperature[t1]-cf)))
didr = 0
for (i in t1:tstar0){
  vot = 1/(1+exp(bf*(meantemperature[i]-cf)))
  didr = didr+vot
}
der = -(bf*exp(bf*(M-cf)))/(exp(bf*(M-cf))+1)^2
didt = 1/(1+exp(bf*(meantemperature[tstar0]-cf)))
dertsar = (-didr*der)/didt

tdx = tstar0 + dertsar*(avetemp-M)

## insect
emer0 = results$Emergence[1]

didr2 = 0
for (i in 1:emer0){
  tau = (meantemperature[i]-tb)/(tm-tb)
  expon = beta2-beta3*tau
  vot = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))
  didr2 = didr2+vot
}
der2 = beta1*((beta3*exp(beta2-(beta3*(M-tb))/(tm-tb)))/((tm-tb)*(exp(beta2-(beta3*(M-tb))/(tm-tb))+1)^2)-exp(((M-tb)/(tm-tb)-1)/beta4)/(beta4*(tm-tb)))

tau = (meantemperature[emer0]-tb)/(tm-tb)
expon = beta2-beta3*tau
didt2 = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))
dertsar2 = (-didr2*der2)/didt2

tdx2 = emer0 + dertsar2*(avetemp-M)


#### test Pureswaran ####
library(chillR)

setwd("~/Master_Uottawa/SBW_models/Budworm_data")
dat=read.csv("Meteo_RaO_2013-2014.csv")
names(dat)[c(9,11)] = c('Tmin','Tmax')

# data
genetemp = function(x){
  Lat = x[1,"Latitude"]
  t1 = x[,c("Year","Month","Day","Tmax","Tmin")]
  weather = fix_weather(t1)
  hourtemps = stack_hourly_temps(weather, latitude=Lat)$hourtemps
  res = data.frame(KeyID=x[1,"KeyID"],hourtemps)
  res = res[seq(1,nrow(res),4),]
  return(res)
}

toto = by(dat,dat$KeyID,genetemp,simplify = F)
toto = do.call('rbind',toto)

emergenceres = rep(0,2)
budburstres = rep(0,2)

indexresults = 1
for (w in 1:2){
  site = subset(toto,toto$KeyID==w)
  site = droplevels(site)
  
  temperature = subset(site,site$Month>=3)
  temperature = droplevels(temperature)
  
  temperatureL2 = temperature$Temp
  
  # larvae
  rt2 = rep(0,length(temperatureL2))
  for (i in 1:length(temperatureL2)){
    if (temperatureL2[i] >= tb && temperatureL2[i] <= tm){
      tau = (temperatureL2[i]-tb)/(tm-tb)
      expon = beta2-beta3*tau
      rt2[i] = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))/6
    }else{
      rt2[i] = 0
    }
  }
  
  rt2tot = cumsum(rt2)
  index = which(rt2tot>=1)
  moulting = index[1]/6
  emergence = moulting + 58
  
  # from tree
  temperature = site$Temp
  beg = t1*6
  temp=temperature[beg:length(temperature)]
  
  rft = 1/(1+exp(bf*(temp-cf)))/6
  
  ft = cumsum(rft)
  
  index = which(ft>=fstar)
  budburst = index[1]/6
  realdate = budburst+t1
  
  emergenceres[indexresults] = emergence
  budburstres[indexresults] = realdate
  indexresults = indexresults+1
}

emergenceres
# 140 141
budburstres
# 147 148

#### test analytical ####
MeanTemp = 6;  # Fredericton mean temp
AmpliTemp = 14; # Fredericton amplitude temp
phase = 200;  # Ottawa temperature phase
dt = 1/24; # dt in 1-hour intervals
t = seq(1,356,by=dt);  # days in a year

# for balsam fir, except the date t1
t1 = 87;  # starting date for heat accumulation
b = -1.32;
c = 7.14;
Fb = 18.6;

# spruce budworm
beta1 = 0.194;
beta2 = 3.0;
beta3 = 5.84;
beta4 = 0.034;
tb = 2.5;
tm = 35;
t1s = 31+28

MaxRSlope = -log(2)/b+c;

LL=21;
BurstDay = rep(0,LL);

for (nn in 1:LL){
  AddMean = (nn-1)*0.1;
  xmean1 = MeanTemp+AddMean + AmpliTemp * cos(2*pi*(t-phase)/365);
  tt = t[1:phase/dt];
  xm1 = xmean1[1:(phase/dt)];
  
  RM1 = 1/(1+exp(b*(xm1-c)));
  
  IM1 = rep(0,length(tt)); # Starting the integral
  for (kk in t1:(phase/dt-1)){
    IM1[kk+1]=IM1[kk]+RM1[kk]*dt;
  }
  
  BurstM1 = min(which((IM1-Fb)>0))*dt;
  BurstDay[nn]=BurstM1;
  rm(BurstM1)
}

# NOW COMPARE WITH THE LINEAR APPROXIMATION
RM1prime = -b*exp(b*(xm1-c))/(1+exp(b*(xm1-c)))^2;
burstindex = min(which((IM1-Fb)>0));

IIM1 = rep(0,length(tt)); # Starting the integral
for (kk in t1:(phase/dt-1)){
  IIM1[kk+1]=IIM1[kk]+RM1prime[kk]*dt;
}

INT = sum(RM1prime[1:burstindex])*dt;
IIM1[(burstindex+1)];
SLOPE = -INT/RM1[burstindex]
ord = rep(0,2)
ord[1] = BurstDay[1]
ord[2] = BurstDay[1]+SLOPE*(LL-1)*0.1

# SBW
MeanTemp = 6;  # Fredericton mean temp
AmpliTemp = 14; # Fredericton amplitude temp
phase = 200;  # Ottawa temperature phase
dt = 6/24; # dt in 4-hour intervals
t = seq(1,356,by=dt);  # days in a year

BurstDay2 = rep(0,LL);
for (nn in 1:LL){
  AddMean = (nn-1)*0.1;
  xmean1 = MeanTemp+AddMean + AmpliTemp * cos(2*pi*(t-phase)/365);
  tt = t[1:phase/dt];
  xm1 = xmean1[1:(phase/dt)];
  
  # RM1 = 1/(1+exp(b*(xm1-c)));
  RM1 = rep(0,length(xm1))
  for (i in 1:length(xm1)){
    if (xm1[i] >= tb && xm1[i] <= tm){
      tau = (xm1[i]-tb)/(tm-tb)
      expon = beta2-beta3*tau
      RM1[i] = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))/6
    }
  }
  
  IM1 = rep(0,length(tt)); # Starting the integral
  for (kk in t1s:(phase/dt-1)){
    IM1[kk+1]=IM1[kk]+RM1[kk]*dt;
  }
  
  BurstM1 = min(which((IM1-1)>0))*dt;
  BurstDay2[nn]=BurstM1;
  rm(BurstM1)
}

# NOW COMPARE WITH THE LINEAR APPROXIMATION
RM1prime = beta1*((beta3*exp(beta2-(beta3*(xm1-tb))/(tm-tb)))/((tm-tb)*(exp(beta2-(beta3*(xm1-tb))/(tm-tb))+1)^2)-exp(((xm1-tb)/(tm-tb)-1)/beta4)/(beta4*(tm-tb)))

#RM1prime = -b*exp(b*(xm1-c))/(1+exp(b*(xm1-c)))^2;
burstindex = min(which((IM1-1)>0));

IIM1 = rep(0,length(tt)); # Starting the integral
for (kk in t1s:(phase/dt-1)){
  IIM1[kk+1]=IIM1[kk]+RM1prime[kk]*dt;
}

INT = sum(RM1prime[1:burstindex])*dt;
IIM1[(burstindex+1)];
SLOPE2 = -INT/RM1[burstindex]
ord2 = rep(0,2)
ord2[1] = BurstDay2[1]
ord2[2] = BurstDay2[1]+SLOPE2*(LL-1)*0.1

plot(BurstDay2)
lines(ord2~c(1,LL))

#### other approach
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
  #emergence = moulting + 59
  
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
RM1prime = -bf*exp(bf*(xm1tree-cf))/(1+exp(bf*(xm1tree-cf)))^2;
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

setwd("C:/Users/sebca/Documents/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
png('Linear_Approximation.png',width = 800, height = 480)
par(mar=c(5,5,4,2))
plot(emergenceres,pch=19,ylim=c(115,133),xlab='increase in average temperature',ylab='emergence/budburst',axes = F,cex.lab=1.5)
axis(1,at=seq(0,30,by=5),labels = seq(0,3,by=0.5))
axis(2)
box()
points(budburstres,pch=19,col='green')
lines(ord~c(1,LL),col='green',lty=2,lwd=1.5)
lines(ord2~c(1,LL),lty=2,lwd=1.5)
legend(25,130,c('Tree','Insect'),col=c('green','black'),lty=2,bty='n')
dev.off()

#### derivative curves ####
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

# THE LINEAR APPROXIMATION
# SBW
RM1primesbw = beta1*((beta3*exp(beta2-(beta3*(xm1-tb))/(tm-tb)))/((tm-tb)*(exp(beta2-(beta3*(xm1-tb))/(tm-tb))+1)^2)-exp(((xm1-tb)/(tm-tb)-1)/beta4)/(beta4*(tm-tb)))

# THE LINEAR APPROXIMATION 
# TREE
RM1primetree2 = -bf*exp(bf*(xm1-cf))/(1+exp(bf*(xm1-cf)))^2;
RM1primetree = RM1primetree2/30

# larvae
rt2 = rep(0, length(xm1))
for (i in 1:length(xm1)){
  if (xm1[i]>=tb && xm1[i]<=tm){
    tau = (xm1[i]-tb)/(tm-tb)
    expon = beta2-beta3*tau
    rt2[i] = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))
  }
}

rft = 1/(1+exp(bf*(xm1-cf)))

rt22 = rt2/10
rft2 = rft/50

setwd("C:/Users/sebca/Documents/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
png('Derivatives.png',width = 800,height = 480)
par(mar=c(5,5,4,1))
plot(RM1primetree~xm1,type='l',col='grey60',lwd=3,xlab='Temperature',ylab="R'(x(t))",xlim=c(-20,35),ylim=c(0,0.02),cex.lab=1.5)
lines(RM1primesbw~xm1,lwd=3)
lines(rt22~xm1,lwd=3,lty=2)
lines(rft2~xm1,lwd=3,lty=2,col='grey60')
legend(-15,0.015,c("R'(x(t)) Insect","R(x(t)) Insect","R'(x(t)) Tree","R(x(t)) Tree"),lty=c(1,2,1,2),lwd=2,col=c('black','black','grey60','grey60'),bty='n')
dev.off()

#### test R(x(t)) at t* ####
setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Temperatures")
dat = read.csv('3_Hourly_Past_BioSim.csv')

# L2o 
# 1st March
beta1 = 0.194
beta2 = 3.0
beta3 = 5.84
beta4 = 0.034
tb = 2.5
tm = 35

# bf provincial
t1 = 87.36 #84
bf = -1.32 #-0.12
cf = 7.14 #12.5
fstar = 18.6 #17.71

### temperature
years = levels(as.factor(dat$Year))
numyears = length(years)
namesites = levels(as.factor(dat$KeyID))
numsites = 6
simunumb = numyears*numsites


cl = makeCluster(6)
registerDoParallel(cl)

res = foreach (k = 1:numsites) %dopar% {
  
  currentsite = subset(dat,dat$KeyID==namesites[k])
  currentsite = droplevels(currentsite)
  
  results = matrix(nrow=numyears,ncol=4)
  results = as.data.frame(results)
  names(results) = c('Year','Site','Emergence','Budburst')
  
  indexresults = 1
  
  # per year
  for (w in 1:numyears){
    currentyear = subset(currentsite,currentsite$Year==years[w])
    currentyear = droplevels(currentyear)
    
    temperature = subset(currentyear,currentyear$Month>=3)
    temperature = droplevels(temperature)
    
    temperatureL2 = temperature$Temp
    
    # larvae
    rt2 = rep(0,length(temperatureL2))
    for (i in 1:length(temperatureL2)){
      if (temperatureL2[i] >= tb && temperatureL2[i] <= tm){
        tau = (temperatureL2[i]-tb)/(tm-tb)
        expon = beta2-beta3*tau
        rt2[i] = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))/6
      }else{
        rt2[i] = 0
      }
    }
    
    rt2tot = cumsum(rt2)
    index = which(rt2tot>=1)
    moulting = rt2[index[1]]
    
    # from tree
    temperature = currentyear$Temp
    beg = t1*6
    temp=temperature[beg:length(temperature)]
    
    rft = 1/(1+exp(bf*(temp-cf)))/6
    
    ft = cumsum(rft)
    
    index = which(ft>=fstar)
    budburst = rft[index[1]]
    
    results[indexresults,1] = w
    results[indexresults,2] = namesites[k]
    results[indexresults,3] = moulting
    results[indexresults,4] = budburst
    indexresults = indexresults+1
  }
  results
}

stopImplicitCluster()
stopCluster(cl)

res = do.call("rbind",res)

setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Predictions")
write.csv(res,'Test_R_at_Emergence.csv',quote = F,row.names = F)

res = read.csv('Test_R_at_Emergence.csv')

temperature = seq(-20,35,length.out = 1000)

# L2o 
# 1st March
beta1 = 0.194
beta2 = 3.0
beta3 = 5.84
beta4 = 0.034
tb = 2.5
tm = 35

# bf provincial
t1 = 87.36 #84
bf = -1.32 #-0.12
cf = 7.14 #12.5
fstar = 18.6 #17.71

tau = (temperature-tb)/(tm-tb)
expon = beta2-beta3*tau
rt2 = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))

rft = 1/(1+exp(bf*(temperature-cf)))

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")

pdf('Test_R_x.pdf',height = 10, width = 11)
par(mfrow=c(2,1))
plot(rt2~temperature,type='l',lwd=3,xlab='Temperature (°C)',ylab='R(x)',main = 'Insect',cex.lab=1.2)
maxi = max(res$Emergence)
mini = min(res$Emergence)
avg = mean(res$Emergence)
#abline(h = maxi,col='blue',lwd = 3)
#abline(h = mini,col='blue',lwd = 3)
abline(h = avg,col='red',lwd = 3)
text(-8,0.03,'Average R(x(t*))',cex=1.2)

plot(rft~temperature,type='l',lwd=3,xlab='Temperature (°C)',ylab='R(x)',main = 'Tree')
maxi = max(res$Budburst)
mini = min(res$Budburst)
avg = mean(res$Budburst)
#abline(h = maxi,col='blue',lwd = 3)
#abline(h = mini,col='blue',lwd = 3)
abline(h = avg,col='red',lwd = 3,cex.lab=1.2)
text(-8,0.2,'Average R(x(t*))',cex=1.2)
dev.off()


#### test linear temperatures ####
# bf provincial
t0 = 87.36 #84
b = -1.32 #-0.12
c = 7.14 #12.5
fstar = 18.6 #17.71

# temperature = seq(-20,35,length.out = 1000)
# te = seq(0,364)
# A = 15.5
# phi = 200
# M = 6.9
# 
# temperature = M+A*cos(2*pi*(te-phi)/365)
# 

x0 = -8
x1vec = seq(0.05,0.3,length.out = 100)
tsarvec = rep(0,length(x1vec))

for (i in 1:length(x1vec)){
  x1 = x1vec[i]
  tstar = -1/(b*x1)*log(exp(-b*x1*(fstar+t0))+exp(b*(x0-c-x1*fstar))-exp(b*(x0-c)))
  tsarvec[i] = tstar
}

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
png('tstar_linear.png',width = 800,height = 480)
par(mar=c(5,5,4,1))
plot(tsarvec~x1vec,type='l',lwd=3,xlab=expression(paste('x'[1]*' (°C)')),ylab='t* (days)',cex.lab=1.5)
text(0.2,275,expression(paste('x'[0]*' = -8 °C (Fredericton)')),cex = 1.2)
dev.off()


#### warm spell test ####
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
t1sbw = 31+28

#xm1 = seq(-30,35,length.out = 1000);

# cosine
tmean = 6.9
phi = 200
amp = 15
dt = 1/24
tim = seq(1,365,by = dt)

xmean1 = tmean + amp*cos((2*pi*(tim-200))/365)
xm1 = xmean1[1:(phi/dt)];

# search ts1
index = which(xm1>=7.0 & xm1<=7.5)
ts1 = index[14]
# search ts2
index = which(xm1>=19 & xm1<=19.5)
ts2 = index[33]
ts2 = 2900
# warm spell 
deltax = 10
deltat = 5*24

xm1wp1 = xm1
xm1wp1[ts1:(ts1+deltat)] = xm1wp1[ts1:(ts1+deltat)]+deltax

xm1wp2 = xm1
xm1wp2[ts2:(ts2+deltat)] = xm1wp2[ts2:(ts2+deltat)]+deltax

results = matrix(nrow=3,ncol=3)
results = as.data.frame(results)
names(results)=c('Scenario','Emergence','Budburst')
scenario = c('Base','Treespell','Wormspell')

for (j in 1:3){
  
  if (j==1){
    temp = xm1
  }else{
    if (j==2){
      temp = xm1wp1
    }else{
      temp = xm1wp2
    }
  }
  # larvae
  rt2 = rep(0,length(temp))
  for (i in 1:length(temp)){
    if (temp[i] >= tb && temp[i] <= tm){
      tau = (temp[i]-tb)/(tm-tb)
      expon = beta2-beta3*tau
      rt2[i] = (beta1*(1/(1+exp(expon))-exp((tau-1)/beta4)))/24
    }else{
      rt2[i] = 0
    }
  }
  
  rt2[1:(t1sbw*24)] = 0
  rt2tot = cumsum(rt2)
  index = which(rt2tot>=1)
  emergence = index[1]/24
  #emergence = moulting + 59
  
  # from tree
  
  rft = 1/(1+exp(bf*(temp-cf)))/24
  rft[1:(t1*24)] = 0
  
  ft = cumsum(rft)
  
  index = which(ft>=Fb)
  budburst = index[1]/24
  
  results[j,1] = scenario[j]
  results[j,2] = emergence
  results[j,3] = budburst
}

# delta x = 10°C, delta t = 5 days
# base: sbw = 131.7, tree = 127.3
# treespell: sbw = 124.29, tree = 125.75
# wormspell: sbw = 124.75, tree = 127.29




