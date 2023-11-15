library(dplyr)
library(lubridate)
library(ranger)
library(ggplot2)
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

bigrapidscadillac <- nrow(subset(noaa.rec, station %in% c('USW00014817', 'USC00200779')))
trecs <- (1/nrow(logdata)+1/nrow(envirodata)+1/nrow(noaa.rec)+1/bigrapidscadillac)
logdata <-  mutate(logdata, wts = 1/nrow(logdata)/trecs)
envirodata <-  mutate(envirodata, wts = 1/nrow(envirodata)/trecs)
noaa.rec <-   mutate(noaa.rec, wts = ifelse(station %in% c('USW00014817', 'USC00200779'), 1/bigrapidscadillac/trecs,1/nrow(noaa.rec)/trecs))

alldata.50 <- rbind(logdata, envirodata)

alldata.0 <- rbind(noaa.rec) 
alldata <- rbind(alldata.50, alldata.0) 
  
  
#build models
library(lubridate)

alldata.50 <- alldata.50 |> mutate(decdate = decimal_date(date),
                                   forest = ifelse(station %in% c('GRR1','GRR2','GRR5','GRR6'),'forest','open'),
                                   doy = (decdate - floor(decdate)), 
                                   mon = month(date),
                                   d = day(date), 
                                   sin0 = cos((doy)*2*3.141592+2.817934867), #optimal phase
                                   # sin15day = cos((doy+15/365)*2*3.141592+2.817934867), #15 day lag
                                   sin1mon = cos((doy-1/12)*2*3.141592+2.817934867), #1 month lag
                                   sinsqr = (sin0+1)^2/2, soil50 = ifelse(depth %in% 50,1,0)
                                   # sin3mon = cos((doy-3/12)*2*3.141592+2.817934867) #3 month lag
) |> subset(decdate >= 1960)

alldata.50 <- mutate(alldata.50, station.depth = paste0(station,'.x', depth,"cm"))
imputed <- readRDS('data/noaa/normmodel.RDS') |> subset(select= c(date,t,pred))
colnames(imputed) <- c('date','nt','ns') #normal temperature, normal soil
alldata.50 <- left_join(alldata.50,imputed) |> subset(!is.na(nt))


summary(lm(t ~ nt+ns+forest+
     air+soil50+sin0+
     lat+lon+elev+decdate
   , data=alldata.50))


rf <- ranger(t ~ nt+ns+forest+
               air+soil50+sin0+
               lat+lon+elev+decdate
             , data=alldata.50, num.trees=200, sample.fraction = 0.5)
rf$prediction.error
alldata.50 <- alldata.50 |> mutate(t.rf = predictions(predict(rf, data=alldata.50)))
mean((alldata.50$t - alldata.50$t.rf)^2)^0.5



newdat.dates<- subset(alldata.50, select = c(date)) |> unique() |> arrange(date)
newdat.station <- subset(alldata.50, 
                         select = c(station,lat,lon,elev,air,depth)) |> unique()

virtual1 <- newdat.station |> mutate(depth=0,air=1, forest = 'open')
virtual2 <- newdat.station |> mutate(depth=10,air=0, forest = 'open')
virtual3 <- newdat.station |> mutate(depth=50,air=0, forest = 'open')
virtual4 <- newdat.station |> mutate(depth=50,air=0, forest = 'forest')
virtual5 <- newdat.station |> mutate(depth=0,air=1, forest = 'forest')
newdat.station <- rbind(virtual1,virtual2,virtual3,virtual4,virtual5) |> unique()
newdat <- merge(newdat.station, newdat.dates)
original <- alldata.50 |> subset(select=c(date, station, depth, air, forest, t))
newdat <- newdat |> left_join(original)

newdat <- newdat |> mutate(station.depth = paste0(station,'.', depth,"cm",".",forest),
                           decdate = decimal_date(date), 
                           doy = (decdate - floor(decdate)), 
                           mon = month(date),
                           d = day(date), 
                           sin0 = cos((doy)*2*3.141592+2.817934867), #optimal phase
                           sin1mon = cos((doy-1/12)*2*3.141592+2.817934867), #1 month lag
                           sinsqr = (sin0+1)^2/2, soil50 = ifelse(depth %in% 50,1,0)
) |> left_join(imputed)
newdat.50 <- newdat |> mutate(t.rf = predictions(predict(rf, data=newdat)))

#test 
original <- subset(newdat.50, station %in% c('GRR2', 'nwmhrs') & depth ==50 & decdate >= 2019)
original <- subset(newdat.50, station %in% c('GRR1', 'nwmhrs') & depth ==50 & decdate >= 2019)

dtg = 7

original2 <- original |> mutate(y = floor(decdate), grp = floor(decdate*365.25/dtg), station.depth = paste(station, depth,  forest))
original2 <- original2 |> group_by(station.depth, grp) |> summarise(decdate = mean(decdate, na.rm=T), t=mean(t, na.rm=T), t.rf=mean(t.rf, na.rm=T))


brks <- original |> mutate(y = floor(decdate)) |> group_by(y,mon) |> summarise(brk = min(decdate))
brky <- original |> mutate(y = floor(decdate)) |> group_by(y) |> summarise(brk = min(decdate))

library(RColorBrewer)
groupnames = unique(original2$station.depth)
groupcolors = brewer.pal(n = length(groupnames), name = 'Set1')

  
ggplot()+
  geom_point(data=original2, aes(x=decdate, y=t, color=station.depth), alpha=0.3)+
  geom_line(data=original2, aes(x=decdate, y=t.rf, color=station.depth), alpha=1, linewidth=0.8)+
  scale_x_continuous(name='date', breaks = brky$brk, labels = brky$y, minor_breaks = brks$brk)+
  scale_y_continuous(name='temperature (C)')+
  scale_color_manual(name='station/depth/cover', label=groupnames, values =groupcolors)+
  labs(title = paste(dtg, 'day average'))







#NOAA ----

alldata.0 <- alldata.0 |> mutate(decdate = decimal_date(date), 
                                   doy = (decdate - floor(decdate)), 
                                   mon = month(date),
                                   d = day(date), 
                                   sin0 = cos((doy)*2*3.141592+2.817934867), #optimal phase
                                   # sin15day = cos((doy+15/365)*2*3.141592+2.817934867), #15 day lag
                                   sin1mon = cos((doy-1/12)*2*3.141592+2.817934867), #1 month lag
                                   sinsqr = (sin0+1)^2/2, soil50 = ifelse(depth %in% 50,1,0)
                                   # sin3mon = cos((doy-3/12)*2*3.141592+2.817934867) #3 month lag
) |> subset(decdate >= 1960)

alldata.0 <- mutate(alldata.0, station.depth = paste0(station,'.x', depth,"cm"))
imputed <- readRDS('data/noaa/normmodel.RDS') |> subset(select= c(date,t,pred))
colnames(imputed) <- c('date','nt','ns') #normal temperature, normal soil
alldata.0 <- left_join(alldata.0,imputed) |> subset(!is.na(nt))

rf <- ranger(t ~ nt+ns+station+
               air+soil50+sin0+
               lat+lon+elev+decdate
             , data=alldata.0, num.trees=200, sample.fraction = 0.25)
rf$prediction.error
alldata.0 <- alldata.0 |> mutate(t.rf = predictions(predict(rf, data=alldata.0)))
mean((alldata.0$t - alldata.0$t.rf)^2)^0.5

#new data ----
newdat.dates<- subset(alldata.0, select = c(date)) |> unique() |> arrange(date)
newdat.station <- subset(alldata.0, 
                         select = c(station,lat,lon,elev,air,depth)) |> unique()

newdat.station <- newdat.station |> mutate(depth=0,air=1) |> unique()

newdat <- merge(newdat.station, newdat.dates)
original <- alldata.0 |> subset(select=c(date, station, depth, air, t))
newdat <- newdat |> left_join(original)


newdat <- newdat |> mutate(station.depth = paste0(station,'.x', depth,"cm"),
                           decdate = decimal_date(date), 
                           doy = (decdate - floor(decdate)), 
                           mon = month(date),
                           d = day(date), 
                           sin0 = cos((doy)*2*3.141592+2.817934867), #optimal phase
                           sin1mon = cos((doy-1/12)*2*3.141592+2.817934867), #1 month lag
                           sinsqr = (sin0+1)^2/2, soil50 = ifelse(depth %in% 50,1,0)
) |> left_join(imputed)
newdat.0 <- newdat |> mutate(t.rf = predictions(predict(rf, data=newdat)))


#get annual temperatures
newdat.50 <- newdat.50 |> mutate(t.best = ifelse(is.na(t),t.rf,t))
newdat.0 <- newdat.0 |> mutate(t.best = ifelse(is.na(t),t.rf,t))


newdat.50.20192022 <- newdat.50 |> subset(decdate >= 2019.485 & decdate < 2023.485) |> group_by(station, lat, lon, elev, depth, forest) |> summarise(t.rf = mean(t.rf), t.best = mean(t.best),t = mean(t, na.rm=T))|> as.data.frame()

newdat.20192022.air <- newdat.50.20192022 |> mutate(t.air = t.best) |> 
  subset(depth %in% 0 & !forest %in% 'forest', select = c(station, lat, lon, elev, t.air))
newdat.20192022.open <- newdat.50.20192022|> mutate(t.open = t.best) |> 
  subset(depth %in% 50 & !forest %in% 'forest', select = c(station, lat, lon, elev, t.open))
newdat.20192022.forest <- newdat.50.20192022 |> mutate(t.forest = t.best)|> 
  subset(depth %in% 50 & forest %in% 'forest', select = c(station, lat, lon, elev, t.forest))


newdat.20192022 <- newdat.20192022.air |> left_join(newdat.20192022.open)|> left_join(newdat.20192022.forest) |> unique()

newdat.20192022.GRR <- newdat.20192022 |> mutate(dt.open = t.open - t.air, dt.forest = t.forest - t.air, dt.forestopen = t.forest - t.open)

newdat.20192022 <- subset(newdat.20192022.GRR)

#predict temperature differential ----
rf.forest <- ranger(dt.forest ~ 
               lat+lon+elev
             , data=newdat.20192022, num.trees=500, sample.fraction = 0.5)
rf$prediction.error
newdat.20192022 <- newdat.20192022 |> mutate(dt.forest.rf = predictions(predict(rf.forest, data=newdat.20192022)))
mean((newdat.20192022$dt.forest - newdat.20192022$dt.forest.rf)^2)^0.5

rf.open <- ranger(dt.open ~ 
               lat+lon+elev
             , data=newdat.20192022, num.trees=500, sample.fraction = 0.5)
rf$prediction.error
newdat.20192022 <- newdat.20192022 |> mutate(dt.open.rf = predictions(predict(rf.open, data=newdat.20192022)))
mean((newdat.20192022$dt.open - newdat.20192022$dt.open.rf)^2)^0.5

#predict NOAA

newdat.0.19611990 <- newdat.0 |> subset(decdate >= 1961 & decdate < 1991) |> group_by(station, lat, lon, elev) |> summarise(t1990 = mean(t.best)) |> subset(select = c(station, lat, lon, elev, t1990))
newdat.0.19812010 <- newdat.0 |> subset(decdate >= 1981 & decdate < 2011) |> group_by(station, lat, lon, elev) |> summarise(t2010 = mean(t.best)) |> subset(select = c(station, lat, lon, elev, t2010))

newdat.1990 <-  newdat.0.19611990 |> left_join(newdat.0.19812010) |> as.data.frame()

rf.1990 <- ranger(t1990 ~ 
                    lat+lon+elev
                  , data=newdat.1990, num.trees=500, sample.fraction = 0.5)
rf$prediction.error
newdat.1990 <- newdat.1990 |> mutate(t1990.rf = predictions(predict(rf.1990, data=newdat.1990)))
mean((newdat.1990$t1990 - newdat.1990$t1990.rf)^2)^0.5

rf.2010 <- ranger(t2010 ~ 
                    lat+lon+elev
                  , data=newdat.1990, num.trees=500, sample.fraction = 0.5)
rf$prediction.error
newdat.1990 <- newdat.1990 |> mutate(t2010.rf = predictions(predict(rf.2010, data=newdat.1990)))
mean((newdat.1990$t2010 - newdat.1990$t2010.rf)^2)^0.5

#apply to full data ----
newdat.station <- subset(alldata.0, 
                         select = c(station,lat,lon,elev)) |> unique() |> 
  rbind(subset(alldata.50,select = c(station,lat,lon,elev)) |> unique())

newdat.station <- newdat.station |> mutate(t2010.rf = predictions(predict(rf.2010, data=newdat.station)),
                                           t1990.rf = predictions(predict(rf.1990, data=newdat.station)),
                                           dt.open.rf = predictions(predict(rf.open, data=newdat.station)),
                                           dt.forest.rf = predictions(predict(rf.forest, data=newdat.station)))


original <- newdat.1990 |> subset(select=c(station, t1990, t2010))
newdat.station <- newdat.station |> left_join(original)
original <- newdat.20192022 |> subset(select=c(station, dt.open, dt.forest))
newdat.station <- newdat.station |> left_join(original)

newdat.station <- newdat.station |> mutate(
  soilopen1990 = ifelse(is.na(t1990), t1990.rf,t1990)+ifelse(is.na(dt.open), dt.open.rf,dt.open),
  soilopen2010 = ifelse(is.na(t2010), t2010.rf,t2010)+ifelse(is.na(dt.open), dt.open.rf,dt.open),
  soilforest1990 = ifelse(is.na(t1990), t1990.rf,t1990)+ifelse(is.na(dt.forest), dt.forest.rf,dt.forest),
  soilforest2010 = ifelse(is.na(t2010), t2010.rf,t2010)+ifelse(is.na(dt.forest), dt.forest.rf,dt.forest))

write.csv(newdat.station, 'soiltemperature.csv', row.names = F)



#misc............-----
newdat.50.20192023monthly <- newdat.50 |> subset(decdate >= 2019) |> group_by(station, depth, mon) |> summarise(t.rf = mean(t.rf), t.best = mean(t.best),t = mean(t, na.rm=T)) |> group_by(station, depth) |> summarise(t.rf = mean(t.rf), t.best = mean(t.best),t = mean(t)) |> group_by(station, depth) |> as.data.frame()

newdat.50.20192022 <- newdat.50 |> subset(decdate >= 2019.485 & decdate < 2023.485) |> group_by(station, lat, lon, elev, depth) |> summarise(t.rf = mean(t.rf), t.best = mean(t.best),t = mean(t, na.rm=T))|> as.data.frame()

newdat.2022.50 <- newdat.50 |> subset(decdate >= 2019.485 & decdate < 2023.485 & depth %in% 50) |> group_by(station, lat, lon, elev, depth) |> summarise(t.50 = mean(t.best))|> as.data.frame()|> subset(select=-depth)

newdat.2022.0 <- newdat.50 |> subset(decdate >= 2019.485 & decdate < 2023.485 & depth %in% 0) |> group_by(station, lat, lon, elev, depth) |> summarise(t.0 = mean(t.best))|> as.data.frame() |> subset(select=-depth)

newdat.2022 <- newdat.2022.0 |> left_join(newdat.2022.50) |> mutate(dif = t.50-t.0)



newdat.0.20192022 <- newdat.0 |> subset(decdate >= 2019.485 & decdate < 2023.485) |> group_by(station, lat, lon, elev) |> summarise(t2022 = mean(t.best))

newdat.0.19611990 <- newdat.0 |> subset(decdate >= 1961 & decdate < 1991) |> group_by(station, lat, lon, elev) |> summarise(t1990 = mean(t.best))
newdat.0.19812010 <- newdat.0 |> subset(decdate >= 1981 & decdate < 2011) |> group_by(station, lat, lon, elev) |> summarise(t2010 = mean(t.best))

newdat.0.annual <- newdat.0.20192022 |> left_join(newdat.0.19611990)  |> left_join(newdat.0.19812010)
newdat.0.annual <- newdat.0.annual |> mutate(dt1990 = t1990-t2022, dt2010 = t2010-t2022) |> as.data.frame()

lm1990 <- lm(dt1990 ~ lat*lon*elev
                 , data=newdat.0.annual)
summary(lm1990)
newdat.0.annual <- newdat.0.annual |> mutate(wts=ifelse(station %in% c('USC00200779', 'USW00014817'), 25,1))

rf1990 <- ranger(dt1990 ~ lat+lon+elev
                 , data=newdat.0.annual, num.trees=500, sample.fraction = 0.25, case.weights = 'wts')
rf1990$prediction.error

newdat.0.annual <- newdat.0.annual |> mutate(dt1990.rf = predictions(predict(rf1990, data=newdat.0.annual)))
mean((newdat.0.annual$dt1990 - newdat.0.annual$dt1990.rf)^2)^0.5

rf2010 <- ranger(dt2010 ~ lat+lon+elev
             , data=newdat.0.annual, num.trees=500, sample.fraction = 0.25, case.weights = 'wts')
rf2010$prediction.error

newdat.0.annual <- newdat.0.annual |> mutate(dt2010.rf = predictions(predict(rf2010, data=newdat.0.annual)))
mean((newdat.0.annual$dt1990 - newdat.0.annual$dt1990.rf)^2)^0.5

write.csv(newdat.0.annual,'newdat.0.annual.csv', row.names = F)

newdat.50.20192022 <- newdat.50.20192022 |> mutate(dt1990.rf = predictions(predict(rf1990, data=newdat.50.20192022)),
                          dt2010.rf = predictions(predict(rf2010, data=newdat.50.20192022)),
                          predt1990 = t.best+dt1990.rf, predt2010 = t.best+dt2010.rf)
write.csv(newdat.50.20192022,'newdat.50.20192022.csv', row.names = F)
newdat.20192022.50 <- subset(newdat.50.20192022, depth %in% 50)
write.csv(newdat.20192022.50,'newdat.20192022.50.csv', row.names = F)