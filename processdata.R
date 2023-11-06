library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
                             sinsqr = (sin0+1)^2/2
                             # sin3mon = cos((doy-3/12)*2*3.141592+2.817934867) #3 month lag
) |> subset(decdate >= 1960)

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

mod <- lm(t ~ air*sin0+depth*sin0+
            air*sin1mon+depth*sin1mon+
            air*sinsqr+depth*sinsqr+
            lat+lon+elev+decdate+I(decdate^2)+
            sin0*lat+sin0*lon+sin0*elev+
          GRR1+GRR2+GRR5+GRR6, data=alldata)

summary(mod)
alldata <- alldata |> mutate(pred = predict(mod,alldata), res = t-pred)
library(ranger)
rf <- ranger(res ~ air+depth+sin0+
         sin1mon+sinsqr+
         lat+lon+elev+decdate+
         GRR1+GRR2+GRR5+GRR6, data=alldata, num.trees=200, sample.fraction = 0.25)
rf$prediction.error

#test models
alldata <- alldata |> mutate(pred2 = predictions(predict(rf, data=alldata)), pred3 = pred+pred2)

newdat <- subset(alldata, station %in% 'USW00014817' & decdate >= 1961 & decdate < 1991) 
newdat <- subset(alldata, station %in% c('GRR1','GRR2','GRR5','GRR6')) 
newdat <- subset(alldata, station %in% 'nwmhrs' & decdate >= 2019.655 & decdate <= 2023.836) 

newdat <- newdat |> group_by(station, mon, depth) |> summarize(t=mean(t), pred3=mean(pred3)) |> group_by(station, depth) |> summarize(t=mean(t), pred3=mean(pred3))



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
                                       sinsqr = (sin0+1)^2/2,
                                      )

newdat <- newdat |> mutate(pred = predict(mod,newdat), pred2 = predictions(predict(rf, data=newdat)), pred3 = pred+pred2)

newdat.monthly <- newdat |> subset(decdate >= 1961 & decdate < 1991) |> group_by(station, mon, depth) |> summarise(t=mean(pred3))

newdat.annual <- newdat |> subset(decdate >= 2010 & decdate < 2023) |> group_by(station, lat,lon,elev, depth) |> summarise(t=mean(pred3))
write.csv(newdat.annual, 'annual_2010_2022.csv', row.names = F)

newdat.annual <- newdat |> subset(decdate >= 1981 & decdate < 2011) |> group_by(station, lat,lon,elev, depth) |> summarise(t=mean(pred3))
write.csv(newdat.annual, 'annual_1981_2010.csv', row.names = F)

newdat.annual <- newdat |> subset(decdate >= 1961 & decdate < 1991) |> group_by(station, lat,lon,elev, depth) |> summarise(t=mean(pred3))
write.csv(newdat.annual, 'annual_1961_1990.csv', row.names = F)

