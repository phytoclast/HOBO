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
logdata <- loggers |> right_join(logdata) 

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
envirodata <- enviro |> right_join(envirodata) 
