### Packages and functions ----
library(stringr)

dyear <- function(year, month, day){
  leap <- ifelse(year/400 == floor(year/400), 1,
                 ifelse(year/100 == floor(year/100), 0,
                        ifelse(year/4 == floor(year/4), 1,0)))
  
  nday <-ifelse(month == 1, 0+day,
                ifelse(month == 2, 31+day,
                       ifelse(month == 3, 59+day+leap,
                              ifelse(month == 4, 90+day+leap,
                                     ifelse(month == 5, 120+day+leap,
                                            ifelse(month == 6, 151+day+leap,
                                                   ifelse(month == 7, 181+day+leap,
                                                          ifelse(month == 8, 212+day+leap,
                                                                 ifelse(month == 9, 243+day+leap,
                                                                        ifelse(month == 10, 273+day+leap,
                                                                               ifelse(month == 11, 304+day+leap,334+day+leap)))))))))))
  
  decimalyear <- year + nday/(365+leap) - 1/(365+leap)
  return(decimalyear)
}                                          
### Load and process data frames ----
## Presumes a HOBO cvs expo0rt file with yy/mm/dd date format, a separate time column, and no plot title in the header.

hobo <- read.csv('data/Wexford_frigid_surface.csv')
colnames(hobo) <- c('id', 'date', 'time', 't')
hobo$station <- 'Wexford_frigid_10'

hobo1 <- read.csv('data/Wexford_frigid_50_cm.csv')
colnames(hobo1) <- c('id', 'date', 'time', 't')
hobo1$station <- 'Wexford_frigid_50'
hobo <- rbind(hobo, hobo1)

hobo1 <- read.csv('data/Wexford_mesic_50_cm.csv')
colnames(hobo1) <- c('id', 'date', 'time', 't')
hobo1$station <- 'Wexford_mesic_50'
hobo <- rbind(hobo, hobo1)

hobo1 <- read.csv('data/Newaygo_mesic_50_cm.csv')
colnames(hobo1) <- c('id', 'date', 'time', 't')
hobo1$station <- 'Newaygo_mesic_50'
hobo <- rbind(hobo, hobo1)

hobo1 <- read.csv('data/Newaygo_frigid_50_cm.csv')
colnames(hobo1) <- c('id', 'date', 'time', 't')
hobo1$station <- 'Newaygo_frigid_50'
hobo <- rbind(hobo, hobo1)
rm(hobo1)

hobo$year <- as.numeric(str_split_fixed(hobo$date, '/', 3)[,1]) +2000
hobo$month <- as.numeric(str_split_fixed(hobo$date, '/', 3)[,2])
hobo$day <- as.numeric(str_split_fixed(hobo$date, '/', 3)[,3])
hobo$hour <- as.numeric(str_split_fixed(hobo$time, ':', 3)[,1]) + as.numeric(str_split_fixed(hobo$time, ':', 3)[,2])/60
hobo$dyear <- dyear(hobo$year,hobo$month,hobo$day)
hobo$hyear <- hobo$dyear+hobo$hour/24/365.25

ground <-  read.csv('data/2228937.csv')
ground <- subset(ground, select = c('STATION','NAME','LATITUDE','LONGITUDE','ELEVATION','DATE','PRCP','SNOW','TMAX','TMIN'))
ground$year <- as.numeric(str_split_fixed(ground$DATE, '-', 3)[,1])
ground$month <- as.numeric(str_split_fixed(ground$DATE, '-', 3)[,2])
ground$day <- as.numeric(str_split_fixed(ground$DATE, '-', 3)[,3])
ground$dyear <- dyear(ground$year,ground$month,ground$day)
ground$t <- (ground$TMAX + ground$TMIN)/2


## compare two stations ----
hobo1 <- subset(hobo, station %in% 'Wexford_frigid_50', select = c(date, dyear, hour, hyear, t, station))
hobo2 <- subset(hobo, station %in% 'Wexford_mesic_50', select = c(date, dyear, hour, hyear, t, station))
hobo2$t2 <- hobo2$t
hobo2$station2 <- hobo2$station
hobo1 <- merge(hobo1, hobo2[,c('hyear', 't2', 'station2')], by='hyear')
rm(hobo2)


hobo1$tdif <- hobo1$t - hobo1$t2

plot(hobo1$tdif ~ hobo1$hyear)
boxplot(hobo1$t ~ hobo1$hour)

## get means by station
hobo1.mean <- aggregate(hobo1[,c('t','t2')], by = list(hour = hobo1$hour), FUN = 'mean')

hobo.mean <- aggregate(list(t = hobo$t), by = list(station = hobo$station, month = hobo$month,  day = hobo$day), FUN = 'mean')
hobo.mean2 <- aggregate(list(t = hobo.mean$t),by = list(station = hobo.mean$station), FUN = 'mean')

hobo.summer <- subset(hobo, month %in% c(6,7,8))
hobo.summer.mean <- aggregate(list(t = hobo.summer$t), by = list(station = hobo.summer$station, month = hobo.summer$month,  day = hobo.summer$day), FUN = 'mean')
hobo.summer.mean2 <- aggregate(list(t.summer = hobo.summer.mean$t),by = list(station = hobo.summer.mean$station), FUN = 'mean')

hobo.winter <- subset(hobo, month %in% c(1,2,12))
hobo.winter.mean <- aggregate(list(t = hobo.winter$t), by = list(station = hobo.winter$station, month = hobo.winter$month,  day = hobo.winter$day), FUN = 'mean')
hobo.winter.mean2 <- aggregate(list(t.winter = hobo.winter.mean$t),by = list(station = hobo.winter.mean$station), FUN = 'mean')

hobo.merge <- merge(hobo.mean2, hobo.summer.mean2, by='station')
hobo.merge <- merge(hobo.merge, hobo.winter.mean2, by='station')
hobo.merge$range <- hobo.merge$t.summer - hobo.merge$t.winter

## Determine relationship between layers 1: simple running average ----
## compare two stations
i <- 10
hobo.d <- aggregate(list(t = hobo$t), by = list(station = hobo$station, year = hobo$year, month = hobo$month,  day = hobo$day, dyear = hobo$dyear), FUN = 'mean')
hobo1 <- subset(hobo.d, station %in% 'Wexford_frigid_10', select = c(dyear, t, station))
hobo2 <- subset(hobo.d, station %in% 'Wexford_frigid_50', select = c(dyear, t, station))
hobo2$t50cm <- hobo2$t
hobo1 <- merge(hobo1, hobo2[,c('dyear','t50cm')], by='dyear')
rm(hobo2)

hobo1$t02 <- NA
for (i in 1:nrow(hobo1)){
  hobo1[i,]$t02 <-  mean(subset(hobo1, dyear <= hobo1[i,]$dyear & dyear >= hobo1[i,]$dyear-2/365.25, select = t)[,1])
}

hobo1$t03 <- NA
for (i in 1:nrow(hobo1)){
  hobo1[i,]$t03 <-  mean(subset(hobo1, dyear <= hobo1[i,]$dyear & dyear >= hobo1[i,]$dyear-3/365.25, select = t)[,1])
}

hobo1$t05 <- NA
for (i in 1:nrow(hobo1)){
  hobo1[i,]$t05 <-  mean(subset(hobo1, dyear <= hobo1[i,]$dyear & dyear >= hobo1[i,]$dyear-5/365.25, select = t)[,1])
}

hobo1$t16 <- NA
for (i in 1:nrow(hobo1)){
  hobo1[i,]$t16 <-  mean(subset(hobo1, dyear <= hobo1[i,]$dyear & dyear >= hobo1[i,]$dyear-16/365.25, select = t)[,1])
}

hobo1$t17 <- NA
for (i in 1:nrow(hobo1)){
  hobo1[i,]$t17 <-  mean(subset(hobo1, dyear <= hobo1[i,]$dyear & dyear >= hobo1[i,]$dyear-17/365.25, select = t)[,1])
}

hobo1$t18 <- NA
for (i in 1:nrow(hobo1)){
  hobo1[i,]$t18 <-  mean(subset(hobo1, dyear <= hobo1[i,]$dyear & dyear >= hobo1[i,]$dyear-18/365.25, select = t)[,1])
}
hobo1$t19 <- NA
for (i in 1:nrow(hobo1)){
  hobo1[i,]$t19 <-  mean(subset(hobo1, dyear <= hobo1[i,]$dyear & dyear >= hobo1[i,]$dyear-19/365.25, select = t)[,1])
}
hobo1$t20 <- NA
for (i in 1:nrow(hobo1)){
  hobo1[i,]$t20 <-  mean(subset(hobo1, dyear <= hobo1[i,]$dyear & dyear >= hobo1[i,]$dyear-20/365.25, select = t)[,1])
}

hobo1$t28 <- NA
for (i in 1:nrow(hobo1)){
  hobo1[i,]$t28 <-  mean(subset(hobo1, dyear <= hobo1[i,]$dyear & dyear >= hobo1[i,]$dyear-28/365.25, select = t)[,1])
}

hobo1$t35 <- NA
for (i in 1:nrow(hobo1)){
  hobo1[i,]$t35 <-  mean(subset(hobo1, dyear <= hobo1[i,]$dyear & dyear >= hobo1[i,]$dyear-25/365.25, select = t)[,1])
}
cols <- c(2, 4:14) 
cor(hobo1[,cols])

model <- lm(t50cm ~ t + t02 + t03 + t05 + t07 + t14 + t18 + t21 + t24 + t28 + t35, data = hobo1)
summary(model)

for (i in 1:length(cols)){
  model <- lm(t50cm ~ hobo1[,cols[i]], data = hobo1)
  print(colnames(hobo1)[cols[i]])
  print(AIC(model))}

model <- lm(t50cm ~ t18 , data = hobo1)
summary(model)
AIC(model)


## Determine relationship between layers 2: decay day influence ----
## compare two stations
i <- 10
hobo.d <- aggregate(list(t = hobo$t), by = list(station = hobo$station, year = hobo$year, month = hobo$month,  day = hobo$day, dyear = hobo$dyear), FUN = 'mean')
hobo1 <- subset(hobo.d, station %in% 'Wexford_frigid_10', select = c(dyear, t, station))
hobo2 <- subset(hobo.d, station %in% 'Wexford_frigid_50', select = c(dyear, t, station))
hobo2$t50cm <- hobo2$t
hobo1 <- merge(hobo1, hobo2[,c('dyear','t50cm')], by='dyear')
rm(hobo2)

hobo1$t02 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 2
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((hobo1[i,]$dyear - tab$dyear)*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t02 <- sum(tab$twt)/sum(tab$wt)
}

hobo1$t05 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 5
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((hobo1[i,]$dyear - tab$dyear)*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t05 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t06 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 6
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((hobo1[i,]$dyear - tab$dyear)*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t06 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t07 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 7
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((hobo1[i,]$dyear - tab$dyear)*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t07 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t08 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 8
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((hobo1[i,]$dyear - tab$dyear)*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t08 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t09 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 09
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((hobo1[i,]$dyear - tab$dyear)*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t09 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t10 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 10
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((hobo1[i,]$dyear - tab$dyear)*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t10 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t11 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 11
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((hobo1[i,]$dyear - tab$dyear)*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t11 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t12 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 12
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((hobo1[i,]$dyear - tab$dyear)*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t12 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t13 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 13
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((hobo1[i,]$dyear - tab$dyear)*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t13 <- sum(tab$twt)/sum(tab$wt) }

cols <- c(2, 4:14) 
cor(hobo1[,cols])
# determined that 7 day half life is the optimum

for (i in 1:length(cols)){
  model <- lm(t50cm ~ hobo1[,cols[i]], data = hobo1)
  print(colnames(hobo1)[cols[i]])
  print(AIC(model))}
hobo1.x <- hobo1[-c(1:15),]
model <- lm(t50cm ~ t07 , data = hobo1.x)
summary(model)
AIC(model)

## Determine relationship between layers 3: decay day influence shift ----
## compare two stations
i <- 10
hobo.d <- aggregate(list(t = hobo$t), by = list(station = hobo$station, year = hobo$year, month = hobo$month,  day = hobo$day, dyear = hobo$dyear), FUN = 'mean')
hobo1 <- subset(hobo.d, station %in% 'Wexford_frigid_10', select = c(dyear, t, station))
hobo2 <- subset(hobo.d, station %in% 'Wexford_frigid_50', select = c(dyear, t, station))
hobo2$t50cm <- hobo2$t
hobo1 <- merge(hobo1, hobo2[,c('dyear','t50cm')], by='dyear')
rm(hobo2)
shift <- 6 # determined that a shift of zero is the optimum
hobo1$t02 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 2
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((abs((hobo1[i,]$dyear - shift/365.25) - tab$dyear))*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t02 <- sum(tab$twt)/sum(tab$wt)
}

hobo1$t03 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 3
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((abs((hobo1[i,]$dyear - shift/365.25) - tab$dyear))*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t03 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t05 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 5
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((abs((hobo1[i,]$dyear - shift/365.25) - tab$dyear))*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t05 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t07 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 7
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((abs((hobo1[i,]$dyear - shift/365.25) - tab$dyear))*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t07 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t14 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 14
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((abs((hobo1[i,]$dyear - shift/365.25) - tab$dyear))*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t14 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t18 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 18
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((abs((hobo1[i,]$dyear - shift/365.25) - tab$dyear))*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t18 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t21 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 21
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((abs((hobo1[i,]$dyear - shift/365.25) - tab$dyear))*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t21 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t24 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 24
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((abs((hobo1[i,]$dyear - shift/365.25) - tab$dyear))*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t24 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t28 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 28
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((abs((hobo1[i,]$dyear - shift/365.25) - tab$dyear))*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t28 <- sum(tab$twt)/sum(tab$wt) }

hobo1$t35 <- NA
for (i in 1:nrow(hobo1)){
  halflife <- 35
  tab <- subset(hobo1, dyear <= hobo1[i,]$dyear)
  tab$wt <- 2^-((abs((hobo1[i,]$dyear - shift/365.25) - tab$dyear))*365.25/halflife)
  tab$twt <- tab$wt * tab$t
  hobo1[i,]$t35 <- sum(tab$twt)/sum(tab$wt) }

cols <- c(2, 4:14) 
cor(hobo1[,cols])

model <- lm(t50cm ~ t + t02 + t03 + t05 + t07 + t14 + t18 + t21 + t24 + t28 + t35, data = hobo1)
summary(model)

for (i in 1:length(cols)){
  model <- lm(t50cm ~ hobo1[,cols[i]], data = hobo1)
  print(colnames(hobo1)[cols[i]])
  print(AIC(model))}

model <- lm(t50cm ~ t07 , data = hobo1)
summary(model)
AIC(model)








## Relationship to ground stations ----

hobo.d <- aggregate(list(t = hobo$t), by = list(station = hobo$station, year = hobo$year, month = hobo$month,  day = hobo$day, dyear = hobo$dyear), FUN = 'mean')
hobo1 <- subset(hobo.d, station %in% 'Wexford_frigid_10', select = c(dyear, t, station))
sts <- unique(ground$STATION)
i=1
for (i in 1:length(sts)){

ground1 <- subset(ground, STATION %in% sts[i], select = c(dyear, t, STATION))
ground1$x <- ground1$t
hobo1 <- merge(hobo1, ground1[,c('dyear','x')], by='dyear')
colnames(hobo1)[names(hobo1) == 'x'] <- as.character(sts[i])
}


hobo1 <- subset(hobo1, !is.na(USC00200779) & !is.na(USC00201178) , select = -c(USC00208772))
cor(hobo1[,c(2,4:7)])

