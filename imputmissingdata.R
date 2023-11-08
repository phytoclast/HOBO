library(dplyr)
library(missRanger)
library(lubridate)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
alldata <- readRDS('output/alldata.RDS')

#get all 50 cm data
alldata.50 <- subset(alldata, depth == 50) |>  group_by(station, lat, lon) |> summarise(nrecs = length(station), decdate = min(decdate)) |> subset(nrecs >= 1000)
alldata.air <- subset(alldata, depth == 0)
alldata.50 <- subset(alldata, station %in% alldata.50$station & station %in% alldata.air$station & decdate >= min(alldata.50$decdate))
alldata.50 <- mutate(alldata.50, station.depth = paste0(station,'.x', depth,"cm"))
liststations <- unique(alldata.50$station.depth)


newdat.dates <- subset(alldata.50, decdate >= 2019.405, select = c(date)) |> unique() |> arrange(date)
for(i in 1:length(liststations)){#i=1
  newdat0 <- subset(alldata.50, station.depth %in%  liststations[i],
                    select = c(date, t)) |> unique()
  colnames(newdat0)[colnames(newdat0) == 't'] <- liststations[i]
  newdat.dates <- left_join(newdat.dates, newdat0)}
newdat.dates <- newdat.dates |> mutate(decdate = decimal_date(date),
                                       doy = (decdate - floor(decdate)),
                                       sin0 = cos((doy)*2*3.141592+2.817934867),
                                       doy = NULL)

newdat.50 <- missRanger(newdat.dates)

cols50 <- colnames(newdat.50)[grepl('x50cm',colnames(newdat.50))]
mean50 = apply(newdat.50[,cols50], MARGIN = 1, FUN='mean')
cols10 <- colnames(newdat.50)[grepl('x10cm',colnames(newdat.50))]
mean10 = apply(newdat.50[,cols10], MARGIN = 1, FUN='mean')
cols0 <- colnames(newdat.50)[grepl('x0cm',colnames(newdat.50))]
mean0 = apply(newdat.50[,cols0], MARGIN = 1, FUN='mean')
meanall <- data.frame(date = newdat.50$date, 
                      decdate = newdat.50$decdate,
                      doy = round((newdat.50$decdate - floor(newdat.50$decdate))*365.25,0),
                      sin0 = newdat.50$sin0, 
                      mean0 = mean0, 
                      mean10 = mean10, 
                      mean50 = mean50)

ggplot()+
  geom_point(data=meanall, aes(x=decdate, y=mean0), alpha=0.2, color='blue')+
  geom_point(data=meanall, aes(x=decdate, y=mean10), alpha=0.2, color='green')+
  geom_point(data=meanall, aes(x=decdate, y=mean50), alpha=0.2, color='red')

meanyear <- meanall |> group_by(doy) |> summarise(mean0=mean(mean0),mean10=mean(mean10),mean50=mean(mean50))

ggplot()+
  geom_line(data=meanyear, aes(x=doy, y=mean0), alpha=1, color='blue', size=1)+
  geom_line(data=meanyear, aes(x=doy, y=mean10), alpha=1, color='green', size=1)+
  geom_line(data=meanyear, aes(x=doy, y=mean50), alpha=1, color='red', size=1)
rownames(meanall) <-  NULL

meanall <- mutate(meanall, bt=ifelse(mean0 > 0,mean0,0),cold=ifelse(mean0 < 5,5-mean0,0), run02 = NA, run03 = NA, run05 = NA, run07 = NA, run15 = NA, run22 = NA,run30 = NA, run45 = NA, bun02 = NA, bun03 = NA, bun05 = NA, bun07 = NA, bun15 = NA, bun22 = NA, bun30 = NA,bun45 = NA, cold22=NA)
for(i in 1:nrow(meanall)){#i=14
  x45 = (i:(i-44))
  x30 = (i:(i-29))
  x22 = (i:(i-21))
  x15 = (i:(i-14))
x7 = (i:(i-6))
x5 = (i:(i-4))
x3 = (i:(i-2))
x2 = (i:(i-1))
meanall[i,]$cold22 <- mean(meanall[x45[x45>0],]$cold)
meanall[i,]$run45 <- mean(meanall[x45[x45>0],]$mean0)
meanall[i,]$run30 <- mean(meanall[x30[x30>0],]$mean0)
meanall[i,]$run22 <- mean(meanall[x22[x22>0],]$mean0)
meanall[i,]$run15 <- mean(meanall[x15[x15>0],]$mean0)
meanall[i,]$run07 <- mean(meanall[x7[x7>0],]$mean0)
meanall[i,]$run05 <- mean(meanall[x5[x5>0],]$mean0)
meanall[i,]$run03 <- mean(meanall[x3[x3>0],]$mean0)
meanall[i,]$run02 <- mean(meanall[x2[x2>0],]$mean0)
meanall[i,]$bun45 <- mean(meanall[x45[x45>0],]$bt)
meanall[i,]$bun30 <- mean(meanall[x30[x30>0],]$bt)
meanall[i,]$bun22 <- mean(meanall[x22[x22>0],]$bt)
meanall[i,]$bun15 <- mean(meanall[x15[x15>0],]$bt)
meanall[i,]$bun07 <- mean(meanall[x7[x7>0],]$bt)
meanall[i,]$bun05 <- mean(meanall[x5[x5>0],]$bt)
meanall[i,]$bun03 <- mean(meanall[x3[x3>0],]$bt)
meanall[i,]$bun02 <- mean(meanall[x2[x2>0],]$bt)
}


#determine optimal formulas ----
intercept_only <- lm(mean50 ~ 1, data=meanall)

#define model with all predictors
all <- lm(mean50 ~ mean0+bt+run02+run03+run05+run07+run15+run22+run30+run45+bun02+bun03+bun05+bun07+bun15+bun22+bun30+bun45+bun05*bun22*run45, data=meanall)

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)
forward$anova

mod <- lm(mean50 ~ bun22 +bun05 +run45 , data=meanall)
summary(mod)

meanall <- meanall |> mutate(pred = predict(mod,meanall))
ggplot()+
  geom_line(data=meanall, aes(x=decdate, y=pred), alpha=1, color='blue', size=1)+
  geom_line(data=meanall, aes(x=decdate, y=run22), alpha=1, color='green', size=1)+
  geom_line(data=meanall, aes(x=decdate, y=mean50), alpha=1, color='red', size=1)

#get norms ----

alldata.norms <- subset(alldata, depth == 0) |>  group_by(station, lat, lon) |> summarise(nrecs = length(station), decdate = min(decdate)) |> subset(nrecs >= 60*365.25)

liststations <- unique(alldata.norms$station)
alldata.norms <- subset(alldata, station %in% liststations & depth == 0)

newdat.dates <- subset(alldata.norms, select = c(date)) |> unique() |> arrange(date)
for(i in 1:length(liststations)){#i=1
  newdat0 <- subset(alldata.norms, station %in%  liststations[i],
                    select = c(date, t)) |> unique()
  colnames(newdat0)[colnames(newdat0) == 't'] <- liststations[i]
  newdat.dates <- left_join(newdat.dates, newdat0)}
newdat.dates <- newdat.dates |> mutate(decdate = decimal_date(date),
                                       doy = (decdate - floor(decdate)),
                                       sin0 = cos((doy)*2*3.141592+2.817934867),
                                       doy = NULL)

newdat.norms <- missRanger(newdat.dates)


mean.norm = apply(newdat.norms[,c(2:(ncol(newdat.norms)-2))], MARGIN = 1, FUN='mean')
norms <-  data.frame(date=newdat.norms$date,
                     decdate = newdat.norms$decdate,
                     doy = round((newdat.norms$decdate - floor(newdat.norms$decdate))*365.25,0),
                     sin0 = newdat.norms$sin0, 
                     t = mean.norm) 

norms <- mutate(norms, bt=ifelse(t > 0,t,0),run45 = NA, bun05 = NA, bun22 = NA)
for(i in 1:nrow(norms)){#i=14
  x45 = (i:(i-44))
  x22 = (i:(i-21))
  x5 = (i:(i-4))
  norms[i,]$run45 <- mean(norms[x45[x45>0],]$t)
  norms[i,]$bun22 <- mean(norms[x22[x22>0],]$bt)
  norms[i,]$bun05 <- mean(norms[x5[x5>0],]$bt)
}

norms <- norms |> mutate(pred = predict(mod,norms))


norm.mon <- subset(norms, decdate < 2023) |> group_by(doy) |> summarize(t=mean(t), s=mean(pred))

ggplot()+
  geom_line(data=norm.mon, aes(x=doy, y=t), alpha=1, color='blue', size=1)+
  geom_line(data=norm.mon, aes(x=doy, y=s), alpha=1, color='red', size=1)

saveRDS(norms, 'data/noaa/normmodel.RDS')

