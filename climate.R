library(rnoaa)
library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stations = ghcnd_stations()
stations1 <- subset(stations, latitude >= 43 & latitude <= 45 & longitude >= -87 & longitude <= -84.5 & first_year <1970 & last_year > 2020 & element %in% 'TMAX')
for(i in 1:length(stations1$id)){#i=1
#data = ghcnd_search(stationid='USW00094860')
data = ghcnd_search(stationid=stations1$id[i], refresh = TRUE)
dat.tmax <- data$tmax
dat.tmin <- data$tmin
dat.prcp <- data$prcp
dat <- dat.prcp[,c('id', 'prcp', 'date')]  |> full_join(dat.tmax[,c('id', 'tmax', 'date')]) |> full_join(dat.tmin[,c('id', 'tmin', 'date')])

dat1 <- rbind(dat1,dat)
saveRDS(dat1,'NOAA_records.RDS')}
stations1 <- subset(stations, id %in% unique(dat1$id) & element %in% 'TMAX')
saveRDS(stations1,'NOAA_stations.RDS')

dat <- dat |> mutate(year = as.numeric(format.Date(date, '%Y')), month = as.numeric(format.Date(date, '%m')), day = as.numeric(format.Date(date, '%d')), dayserial = as.numeric(date))

dat <- dat |> mutate(ppt = prcp/10, temp=((tmax+tmin)/20))
dat <- dat |> mutate(rain = ifelse(ppt >= 5,1,0),trace = ifelse(ppt >= 1,1,0))


library(ggplot2)
dat <- dat |> mutate(decade = as.factor(floor(year/5)*5))

selected <-  dat |> group_by(decade, month) |> summarise(temp=mean(temp), ppt=sum(ppt), rain=sum(rain) , trace=sum(trace)) |> subset(month %in% c(7))

ggplot(selected, aes(x=ppt, y=trace, color=decade))+
  geom_point()+
  geom_smooth()


dat <- dat |> mutate(decade = as.factor(floor(year/3)*3))
selected <-  dat |> subset(!is.na(ppt)) |> group_by(decade, year, month) |> summarise(temp=mean(temp, na.rm=T), ppt=sum(ppt, na.rm=T), rain=sum(rain, na.rm=T) , trace=sum(trace, na.rm=T), count=length(day)) |> subset(count>=28)
selected <-  selected |> group_by(decade, month) |> 
  summarise(temp.mean=mean(temp, na.rm=T), temp.sd=sd(temp, na.rm=T), ppt.mean=mean(ppt, na.rm=T), ppt.sd = sd(ppt, na.rm = TRUE)
, rain=mean(rain, na.rm=T) , trace=mean(trace, na.rm=T), count=length(month)) |> subset(count>=3) |> 
  subset(month %in% c(6,7,8))

ggplot(selected, aes(x=decade, y=rain))+
  geom_point()+
  geom_smooth()

ggplot(selected, aes(x=decade, y=ppt.sd))+
  geom_point()+
  geom_smooth()
ggplot(selected, aes(x=decade, y=ppt.mean))+
  geom_point()+
  geom_smooth()

ggplot(selected, aes(x=decade, y=temp.sd))+
  geom_point()+
  geom_smooth()
ggplot(selected, aes(x=decade, y=temp.mean))+
  geom_point()+
  geom_smooth()
























serialdates <- dat$dayserial

dat$trang=NA
dat$tmean=NA
dat$psum=NA

switch = 0
for(k in 2:60){
  Nday=k
for (i in 1:length(serialdates)){#i=2000
  dat.filter <- subset(dat, dayserial <= serialdates[i] & dayserial >= serialdates[i]-Nday)
  trang <- max(dat.filter$temp) - min(dat.filter$temp)
  tmean <- mean(dat.filter$temp)
  psum <- sum(dat.filter$ppt) 
  dat[dat$dayserial %in% serialdates[i],]$trang = trang
  dat[dat$dayserial %in% serialdates[i],]$tmean = tmean
  dat[dat$dayserial %in% serialdates[i],]$psum = psum
}

sclim0 <- dat |> group_by(year,month) |> summarise(NDays = Nday, meanTrang = mean(trang), meanTmean = mean(tmean), meanPsum = mean(psum))
if(switch == 0){
  sclim = sclim0
  switch = 1
}else{sclim = rbind(sclim, sclim0)}}

write.csv(sclim, 'sclim.csv', row.names = F)

library(ggplot2)
selected <-  subset(sclim, month %in% 6 & NDays %in% 7)
ggplot(selected, aes(x=year, y=meanTrang))+
  geom_point()+
  geom_smooth()
sclim <- sclim |> mutate(decade = floor(year/10)*10)
selected <-  sclim |> group_by(decade, month, NDays) |> summarise(meanTrang=mean(meanTrang), meanTmean=mean(meanTmean), meanPsum=mean(meanPsum)/NDays) |> subset(month %in% c(6))

ggplot(selected, aes(x=NDays, y=meanPsum, color=as.factor(decade)))+
  geom_point()+
  geom_smooth()


