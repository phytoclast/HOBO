library(dplyr)
library(lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#
#
#Make Case wts proportional to the number of records in each of the three sets....
#
#
#
#
files <- list.files('data/loggers/clean')

loggers <- read.csv('data/loggers/loggers.csv')
for(i in 1:length(files)){#i=1
logdata0 <- read.csv(paste0('data/loggers/clean/',files[i]))
logdata0$date <- as.Date(logdata0$time)
logdata0$depth <- ifelse(grepl('50_cm',files[i]), 50,10)
logdata0$air <- 0
logdata0$t <-  (logdata0$t.F-32)/1.8
logdata0$station = ifelse(grepl('Newaygo',files[i]), ifelse(grepl('frigid',files[i]), 'GRR5', 'GRR6'),
                       ifelse(grepl('frigid',files[i]), 'GRR1', 'GRR2'))
logdata0 <- logdata0[,c('station', 'date', 'air','depth', 't')]
if(i==1){logdata=logdata0}else{logdata=rbind(logdata,logdata0)}
}
logdata <- logdata |> group_by(station,date,depth,air) |> summarise(t=mean(t))
logdata <- loggers |> mutate(GRR1 = ifelse(station %in% 'GRR1',1,0),GRR2 = ifelse(station %in% 'GRR2',1,0),GRR5 = ifelse(station %in% 'GRR5',1,0),GRR6 = ifelse(station %in% 'GRR6',1,0)) |> right_join(logdata) 

#other stations ---- 
enviro <- read.csv('data/envriroweather/stations.csv')
files <- list.files('data/envriroweather/clean')
stations <- stringr::str_split_fixed(files, '\\.csv',2)[,1]
envirodata <- NULL
for(i in 1:length(files)){#i=29
  envirodata0 <- read.csv(paste0('data/envriroweather/clean/',files[i]))
  envirodata0$date1 <- envirodata0$date
  envirodata0$date <- as.Date(envirodata0$date1, tryFormats = "%m/%d/%Y")
  envirodata0$station <- stations[i]
  if(!FALSE %in% (c("soil1_max","soil1_min") %in% colnames(envirodata0))){
    envirodata0$t=(envirodata0$soil1_max+envirodata0$soil1_min)/2
    envirodata0$air = 0
    envirodata0$depth = 10
    e0 <- subset(envirodata0, !is.na(t), select=c('station', 'date', 'air','depth', 't'))
    if(is.null(envirodata)){envirodata=e0}else{envirodata=rbind(envirodata,e0)}
  }
  if(!FALSE %in% (c("stmp_50cm_max","stmp_50cm_min") %in% colnames(envirodata0))){
    envirodata0$t=(envirodata0$stmp_50cm_max+envirodata0$stmp_50cm_min)/2
    envirodata0$air = 0
    envirodata0$depth = 50
    e0 <- subset(envirodata0, !is.na(t), select=c('station', 'date', 'air','depth', 't'))
    if(is.null(envirodata)){envirodata=e0}else{envirodata=rbind(envirodata,e0)}
  }
  if(!FALSE %in% (c("stmp_10cm_max","stmp_10cm_min") %in% colnames(envirodata0))){
    envirodata0$t=(envirodata0$stmp_10cm_max+envirodata0$stmp_10cm_min)/2
    envirodata0$air = 0
    envirodata0$depth = 10
    e0 <- subset(envirodata0, !is.na(t), select=c('station', 'date', 'air','depth', 't'))
    if(is.null(envirodata)){envirodata=e0}else{envirodata=rbind(envirodata,e0)}
  }
  if(!FALSE %in% (c("atmp_max","atmp_min") %in% colnames(envirodata0))){
    envirodata0$t=(envirodata0$atmp_max+envirodata0$atmp_min)/2
    envirodata0$air = 1
    envirodata0$depth = 0
    e0 <- subset(envirodata0, !is.na(t), select=c('station', 'date', 'air','depth', 't'))
    if(is.null(envirodata)){envirodata=e0}else{envirodata=rbind(envirodata,e0)}
  }
}
envirodata <- enviro |> mutate(GRR1 = 0,GRR2 = 0,GRR5 = 0,GRR6 = 0) |> right_join(envirodata) 

noaa.rec <- readRDS('data/noaa/NOAA_records.RDS')
noaa.sta <- readRDS('data/noaa/NOAA_stations.RDS')

noaa.sta <- noaa.sta |> mutate(station = id, lat=latitude, lon=longitude, elev=elevation) |> subset(select = c(station, lat,lon,elev))

noaa.rec <- noaa.rec |> mutate(station=id, air=1, depth=0, t=(tmax+tmin)/20) |> subset(!is.na(t), select=c(station, date, air, depth, t))

noaa.rec <- noaa.sta |> mutate(GRR1 = 0,GRR2 = 0,GRR5 = 0,GRR6 = 0) |> right_join(noaa.rec)

alldata <- rbind(logdata, envirodata, noaa.rec) 
                             


#build models
library(lubridate)

alldata <- alldata |> mutate(decdate = decimal_date(date), 
                             doy = (decdate - floor(decdate)), 
                             mon = month(date),
                             d = day(date), 
                             sin0 = cos((doy)*2*3.141592+2.817934867), #optimal phase
                             # sin15day = cos((doy+15/365)*2*3.141592+2.817934867), #15 day lag
                             sin1mon = cos((doy-1/12)*2*3.141592+2.817934867), #1 month lag
                             sinsqr = (sin0+1)^2/2, soil50 = ifelse(depth %in% 50,1,0)
                             # sin3mon = cos((doy-3/12)*2*3.141592+2.817934867) #3 month lag
) |> subset(decdate >= 1960)
#use optimized stations as training for timeline
# library(missRanger)
# liststations <- c('USW00014840', 'USW00094849', 'USW00014817', 'USC00200779')
# 
# newdat.dates <- subset(alldata, select = c(date)) |> unique() |> arrange(date)
# for(i in 1:length(liststations)){#i=1
# newdat.station0 <- subset(noaa.train, station %in%  liststations[i],
#                          select = c(date,t)) |> unique()
# colnames(newdat.station0)[colnames(newdat.station0) == 't'] <- liststations[i]
# newdat.dates <- left_join(newdat.dates, newdat.station0)}
# newdat.dates <- newdat.dates |> mutate(decdate = decimal_date(date), 
#           doy = (decdate - floor(decdate)),
#           sin0 = cos((doy)*2*3.141592+2.817934867))
# 
# msrf <- missRanger(newdat.dates, USW00014840+USW00094849+USW00014817+USC00200779 ~ decdate+sin0+USW00014840+USW00094849+USW00014817+USC00200779)
# msrf <- subset(msrf, select= -c(decdate,doy,sin0))
# saveRDS(msrf, 'data/noaa/imputed.RDS')
imputed <- readRDS('data/noaa/normmodel.RDS') |> subset(select= c(date,t,pred))
colnames(imputed) <- c('date','nt','ns') #normal temperature, normal soil
alldata <- left_join(alldata,imputed)
# #test for optimal phase shift
# sumt <- alldata |> subset(decdate > 1960 & decdate <2010 & station %in% "USW00094860") 
# library(eegkit)
# df2 = eegfft(sumt2$t, 1) |> mutate(p = 1/frequency)
# totaldats <-  sumt |> group_by(mon,d)  |>  summarize(doy = mean(doy), t = mean(t), sin0=mean(sin0))
# ggplot(totaldats, aes(x=doy, y=t))+
#   geom_point()+
#   geom_smooth()+
#   geom_line(aes(x=doy, y=sin0*15+10))+
#   geom_line(aes(x=doy, y=(sin0+1)^2/2*10+5))

mod <- lm(t ~ air*nt+air*ns+soil50*nt+soil50*ns+
            air*sin0+soil50*sin0+
            sin0*lat+sin0*lon+sin0*elev+
            station, data=alldata)

summary(mod)
alldata <- alldata |> mutate(t.lm = predict(mod,alldata), res = t-t.lm) |> subset(!is.na(nt))
mean((alldata$res)^2)^0.5
library(ranger)
rf.lm <- ranger(res ~ nt+ns+station+
               air+soil50+sin0+
               lat+lon+elev+decdate, data=alldata, num.trees=200, sample.fraction = 0.25)
rf.lm$prediction.error

#test models
alldata <- alldata |> mutate(pred = predictions(predict(rf.lm, data=alldata)), t.rflm = pred+t.lm)

mean((alldata$res - alldata$pred)^2)^0.5
mean((alldata$t - alldata$t.rflm)^2)^0.5

rf <- ranger(t ~ nt+ns+station+
               air+soil50+sin0+
               lat+lon+elev+decdate
               , data=alldata, num.trees=200, sample.fraction = 0.25)
rf$prediction.error
alldata <- alldata |> mutate(t.rf = predictions(predict(rf, data=alldata)))
mean((alldata$t - alldata$t.rf)^2)^0.5


#apply models to new data
newdat.dates<- subset(alldata, select = c(date)) |> unique() |> arrange(date)
newdat.station <- subset(alldata, #station %in% c('USW00014817', 'GRR1','GRR2','GRR5','GRR6'),
                         select = c(station,lat,lon,elev,air,depth,GRR1,GRR2,GRR5,GRR6)) |> unique()

virtual1 <- newdat.station |> mutate(depth=0,air=1)
virtual2 <- newdat.station |> mutate(depth=10,air=0)
virtual3 <- newdat.station |> mutate(depth=50,air=0)
newdat.station <- rbind(virtual1,virtual2,virtual3) |> unique()
newdat <- merge(newdat.station, newdat.dates)

newdat <- newdat |> mutate(decdate = decimal_date(date), 
                                       doy = (decdate - floor(decdate)), 
                                       mon = month(date),
                                       d = day(date), 
                                       sin0 = cos((doy)*2*3.141592+2.817934867), #optimal phase
                                       sin1mon = cos((doy-1/12)*2*3.141592+2.817934867), #1 month lag
                                       sinsqr = (sin0+1)^2/2, soil50 = ifelse(depth %in% 50,1,0)
                                      ) |> left_join(imputed)
newdat <- newdat |> mutate(t.lm = predict(mod,newdat), t.rflm = predictions(predict(rf.lm, data=newdat))+t.lm, t.rf = predictions(predict(rf, data=newdat)))
# newdat <- newdat |> mutate(pred = predict(mod,newdat), pred2 = predictions(predict(rf, data=newdat)), pred3 = pred+pred2)
saveRDS(newdat,'output/newdat.RDS')
saveRDS(alldata,'output/alldata.RDS')


alldata.50 <- subset(alldata, depth >=50, select=c(station, lat, lon, elev)) |> unique()
write.csv(alldata.50, 'alldata.50.csv', row.names = F)
alldata.10 <- subset(alldata, depth ==10, select=c(station, lat, lon, elev)) |> unique()
write.csv(alldata.10, 'alldata.10.csv', row.names = F)

newdat.annual <- newdat |> subset(decdate >= 2010 & decdate < 2023) |> group_by(station, lat,lon,elev, depth) |> summarise(t.lm =mean(t.lm), t.rf =mean(t.rf), t.rflm =mean(t.rflm))
write.csv(newdat.annual, 'annual_2010_2022.csv', row.names = F)

newdat.annual <- newdat |> subset(decdate >= 1981 & decdate < 2011) |> group_by(station, lat,lon,elev, depth) |> summarise(t.lm =mean(t.lm), t.rf =mean(t.rf), t.rflm =mean(t.rflm))
write.csv(newdat.annual, 'annual_1981_2010.csv', row.names = F)

newdat.annual <- newdat |> subset(decdate >= 1961 & decdate < 1991) |> group_by(station, lat,lon,elev, depth) |> summarise(t.lm =mean(t.lm), t.rf =mean(t.rf), t.rflm =mean(t.rflm))
write.csv(newdat.annual, 'annual_1961_1990.csv', row.names = F)


###start over#### 
library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
newdat <- readRDS('output/newdat.RDS')
alldata <- readRDS('output/alldata.RDS')

original <- subset(alldata, station %in% c('GRR1', 'USW00014817','USW00094860') & depth %in% c(0,50) & decdate >= 1960)
original <- subset(alldata, station %in% c('GRR2', 'USW00014817') & depth %in% c(0,50) & decdate >= 2019)
original <- subset(alldata, station %in% c('GRR2', 'nwmhrs', 'elkrapids') & depth ==50 & decdate >= 2010)
original <- subset(alldata, station %in% c('GRR6','aetna') & depth %in% c(50) & decdate >= 2015)
original <- subset(alldata, station %in% c('aetna') & depth %in% c(0,10,50) & decdate >= 2015)
original <- subset(alldata, station %in% c('ncmc') & depth %in% c(0,10,50) & decdate >= 2015)
original <- subset(alldata, station %in% c('GRR1', 'kalkaska') & depth ==10 & decdate >= 2015)
original <- subset(alldata, station %in% c('GRR1','USW00014817') & depth %in% c(0,50) & decdate >= 2015)
original <- subset(alldata, station %in% c('GRR1','arlene') & depth %in% c(50) & decdate >= 2015)

modeled  <- subset(newdat, station %in% unique(original$station)  & depth %in% unique(original$depth) & decdate >= min(original$decdate) & decdate <= max(original$decdate) )

dtg = 7
annual = F

if(annual){
  original2 <- original |> mutate(y = floor(decdate), grp = floor(decdate*365.25/dtg), station.depth = paste(station, depth))
  original2 <- original2 |> group_by(station.depth, y, mon) |> summarise(decdate = mean(decdate, na.rm=T), t=mean(t, na.rm=T))
  original2 <- original2 |> group_by(station.depth, y) |> summarise(decdate = mean(decdate, na.rm=T), t=mean(t, na.rm=T))
  modeled2 <- modeled |> mutate(y = floor(decdate), grp = floor(decdate*365.25/dtg), station.depth = paste(station, depth))
  modeled2 <- modeled2 |> group_by(station.depth,  y, mon) |> summarise(decdate = mean(decdate, na.rm=T), t=mean(t.rf, na.rm=T))
  modeled2 <- modeled2 |> group_by(station.depth,  y) |> summarise(decdate = mean(decdate, na.rm=T), t=mean(t.rf, na.rm=T))
}else{
  original2 <- original |> mutate(y = floor(decdate), grp = floor(decdate*365.25/dtg), station.depth = paste(station, depth))
  original2 <- original2 |> group_by(station.depth, grp) |> summarise(decdate = mean(decdate, na.rm=T), t=mean(t, na.rm=T))
  modeled2 <- modeled |> mutate(y = floor(decdate), grp = floor(decdate*365.25/dtg), station.depth = paste(station, depth))
  modeled2 <- modeled2 |> group_by(station.depth, grp) |> summarise(decdate = mean(decdate, na.rm=T), t=mean(t.rf, na.rm=T))
}

if(annual){
  brks <- original |> mutate(y = floor(decdate)) |> group_by(y) |> summarise(brk = min(decdate))
  brky <- original |> mutate(y = floor(decdate/10)*10) |> group_by(y) |> summarise(brk = min(decdate))
}else{
  brks <- original |> mutate(y = floor(decdate)) |> group_by(y,mon) |> summarise(brk = min(decdate))
  brky <- original |> mutate(y = floor(decdate)) |> group_by(y) |> summarise(brk = min(decdate))
}

ggplot()+
  geom_point(data=original2, aes(x=decdate, y=t, color=station.depth), alpha=0.2)+
  geom_line(data=modeled2, aes(x=decdate, y=t, color=station.depth), alpha=1)+
  scale_x_continuous(name='date', breaks = brky$brk, labels = brky$y, minor_breaks = brks$brk)+
  scale_y_continuous(name='temperature (C)')+
  labs(title = paste(dtg, 'day average'))

#####mean year---
modeled2 <- modeled |> mutate(d = floor((decdate-floor(decdate))*365.25), station.depth = paste(station, depth)) |> group_by(station.depth, d) |> summarise(t=mean(t.rf))

ggplot()+
  geom_line(data=modeled2, aes(x=d, y=t, color=station.depth), alpha=1)+
  scale_x_continuous(name='date', breaks = brky$brk, labels = brky$y, minor_breaks = brks$brk)+
  scale_y_continuous(name='temperature (C)')+
  labs(title = paste(dtg, 'day average'))

#test models
#Traverse City/Cadillac/Big Rapids


alldata <- mutate(alldata, station.depth = paste0(station,".",depth))

alldata.GRR1 <- subset(alldata, station %in% 'GRR1' & depth %in% 50)
alldata.elkrapids <- subset(alldata, station %in% 'elkrapids' & depth %in% 50)
alldata.USW00014850 <- subset(alldata, station %in% 'USW00014850')#Traverse City
alldata.USW00014817 <- subset(alldata, station %in% 'USW00014817')#Cadillac
alldata.USC00200779 <- subset(alldata, station %in% 'USC00200779')#Big Rapids	
alldata.GRR6 <- subset(alldata, station %in% 'GRR6' & depth %in% 50)
alldata.aetna <- subset(alldata, station %in% 'aetna' & depth %in% 50)


alldata.aetna.GRR6 <- subset(alldata, station %in% c('GRR6', 'aetna') & depth %in% 50 &
                               date %in% alldata.GRR6$date & date %in% alldata.aetna$date) |> mutate(pair = "aetna X GRR6")

alldata.aetna.bigrapids <- subset(alldata, station %in% c('USC00200779', 'aetna') & depth %in% 0 &
                                    date %in% alldata.USC00200779$date & date %in% alldata.aetna$date) |> mutate(pair = "aetna X bigrapids")


alldata.elkrapids.GRR1 <- subset(alldata, station %in% c('GRR1', 'elkrapids') & depth %in% 50 &
                                   date %in% alldata.GRR1$date & date %in% alldata.elkrapids$date) |> mutate(pair = "elkrapids X GRR1")

alldata.elkrapids.traverse <- subset(alldata, station %in% c('USW00014850', 'elkrapids') & depth %in% 0 &
                                          date %in% alldata.USW00014850$date & date %in% alldata.elkrapids$date) |> mutate(pair = "elkrapids X traverse")

alldata.GRR1.cadillac <- subset(alldata, station %in% c('USW00014817', 'GRR1') &
                                          date %in% alldata.USW00014850$date & date %in% alldata.GRR1$date) |> mutate(pair = "GRR1 X cadillac")


alldata.noaa.1990 <- subset(alldata, station %in% c('USW00014850','USW00014817','USC00200779') & decdate >= 1961 & decdate < 1991) |>  mutate(pair = "1961-1990")

alldata.Cadillac <- rbind(alldata.aetna.bigrapids, alldata.aetna.GRR6, alldata.elkrapids.GRR1,alldata.elkrapids.traverse,alldata.GRR1.cadillac,alldata.noaa.1990)

alldata.Cadillac <- alldata.Cadillac |> group_by(pair, station.depth, mon) |>  summarise(t=mean(t), t.rf = mean(t.rf), t.lm = mean(t.lm)) |> group_by(pair, station.depth)|>  summarise(t=mean(t), t.rf = mean(t.rf), t.lm = mean(t.lm)) 

write.csv(alldata.Cadillac, 'alldata.Cadillac.csv', row.names = F)
