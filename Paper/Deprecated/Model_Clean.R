# Model clean file

library(foreach)
library(doParallel)

#### Past data ####
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
t1 = 84
bf = -0.1936
cf = 10.99
fstar = 13.63

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

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
write.csv(res,'4_hourly_Past_Insect_2.csv',quote = F,row.names = F)


#### Future data ####
# L2o 
# 1st March
beta1 = 0.194
beta2 = 3.0
beta3 = 5.84
beta4 = 0.034
tb = 2.5
tm = 35

# bf provincial
t1 = 84
bf = -0.1936
cf = 10.99
fstar = 13.63

setwd("~/Master_Uottawa/SBW_models/BIOSim_Temperatures/Temperatures")
# data
# temp1 = read.csv('2001_2030_RCM_4_RCP_85.csv')
# temperature1 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2011_2040_RCM_4_RCP_85.csv')
# temperature2 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2021_2050_RCM_4_RCP_85.csv')
# temperature3 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2031_2060_RCM_4_RCP_85.csv')
# temperature4 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2041_2070_RCM_4_RCP_85.csv')
# temperature5 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2051_2080_RCM_4_RCP_85.csv')
# temperature6 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2061_2090_RCM_4_RCP_85.csv')
# temperature7 = temp1[,c(2,3,7,9)]
# rm(temp1)
# temp1 = read.csv('2071_2100_RCM_4_RCP_85.csv')
# temperature8 = temp1[,c(2,3,7,9)]
# rm(temp1)

temp1 = read.csv('2001_2030_Hadley_RCP_26.csv')
temperature1 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2011_2040_Hadley_RCP_26.csv')
temperature2 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2021_2050_Hadley_RCP_26.csv')
temperature3 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2031_2060_Hadley_RCP_26.csv')
temperature4 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2041_2070_Hadley_RCP_26.csv')
temperature5 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2051_2080_Hadley_RCP_26.csv')
temperature6 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2061_2090_Hadley_RCP_26.csv')
temperature7 = temp1[,c(2,3,7,9)]
rm(temp1)
temp1 = read.csv('2071_2100_Hadley_RCP_26.csv')
temperature8 = temp1[,c(2,3,7,9)]
rm(temp1)

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

genetemp = function(x){
  Lat = x[1,"Latitude"]
  t1 = x[,c("Year","Month","Day","Tmax","Tmin")]
  weather = chillR::fix_weather(t1)
  hourtemps = chillR::stack_hourly_temps(weather, latitude=Lat)$hourtemps
  res = data.frame(KeyID=x[1,"KeyID"],hourtemps)
  res = res[seq(1,nrow(res),4),]
  return(res)
}

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

setwd("~/Master_Uottawa/SBW_models/Manuscripts/Paper_1")
write.csv(res,'4_hourly_Predicted_RCP_26_Insect_2.csv',quote = F,row.names = F)


