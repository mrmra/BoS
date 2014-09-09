## BoS analysis -- plotting the number of saints over time.

## For this analysis we make use of the following packages:

library(NLP)
library(openNLP)
library(openNLPmodels.en)

library(sp)
library(rgdal)

library(rCharts)
 ##options(
 ##   rcharts.mode = 'iframesrc', 
 ##   rcharts.cdn = TRUE,
 ##   RCHART_WIDTH = 600,
 ##   RCHART_HEIGHT = 400
## )

library(googleVis)


## load the saints table and geo location info

saints <- read.csv("saints.csv", stringsAsFactors = FALSE)
saints$DECEASEDNEAR <- as.numeric(saints$DECEASEDNEAR)
geoLoc <- read.csv("googleGeo.csv", stringsAsFactors = FALSE)
shape <- readOGR("TM_WORLD_BORDERS_SIMPL-0.3.shp", layer="TM_WORLD_BORDERS_SIMPL-0.3")

## bin the saints based on date deceased, using subset. I had this time series
## in mind when I created the geoLocation data, so the date deceased is already
## in that table, simplifying things a little.

## Well, what I've discovered is that most of the saints in my datas
## are from late antiquity Rome. I'll need to break down the Roman
## time period in more detail. Done!

## Since early Christianity is dominated by the overbearing presence of the 
## not-so-nice Roman empire, we're counting based on Roman ages in early AD.
## http://en.wikipedia.org/wiki/Timeline_of_Rome_history

timeData <- data.frame('START'=NA, 'END'=NA, 'NAME'=NA, 'NUMBER'=NA, 
                       'DATAOBJ'=NA,
                       row.names=NULL)
timeData <- timeData[-1,]
## ^^ Has to be a better way of initializing!
## colnames(timeData) <- c('START', 'END', 'NAME', 'NUMBER', 'DATAOBJ')

ageOfGospel <- subset(saints, DECEASEDNEAR >= 0 & DECEASEDNEAR <= 100)
## timeageOfGospel <- c("0","100","Age of Gospel", nrow(ageOfGospel), 'ageOfGospel')
## timeData <- rbind(timeData,c(0,100,"Age of Gospel", nrow(ageOfGospel), 'ageOfGospel'))
timeData[1,] <- c(0,100,"Age of Gospel", nrow(ageOfGospel), 'ageOfGospel')

## lateAntiquityRome <- subset(saints, DECEASEDNEAR > 100 & DECEASEDNEAR <= 476)

## Hadrian builds wall, ends in universal citizenship
hadrianPeriod <- subset(saints, DECEASEDNEAR > 100 & DECEASEDNEAR <= 200)
timeData <- rbind(timeData,c(101,200,"Hadrian Period", nrow(hadrianPeriod), 'hadrianPeriod'))

## Roman empire splits
beginningOfByzantium <- subset(saints, DECEASEDNEAR > 200 & DECEASEDNEAR <= 300)
timeData <- rbind(timeData,c(201,300,"Beginning of Byzantium", nrow(beginningOfByzantium),'beginningOfByzantium'))

## Traditional start of medieval period of European history
constantineEmpire <- subset(saints, DECEASEDNEAR > 300 & DECEASEDNEAR <= 376)
timeData <- rbind(timeData,c(301,376,"Emperor Constantine", nrow(constantineEmpire), 'constantineEmpire'))

## Christianity becomes the official religion of Rome, dissidents slaughtered 
## Goths sack it a couple times, Emperor deposed, Popes take over. The 
## "Dark Ages" begin.
christianRomanEmpire <- subset(saints, DECEASEDNEAR > 376 & DECEASEDNEAR <= 551)
timeData <- rbind(timeData,c(377,551,"Christian Rome Forms", nrow(christianRomanEmpire),'christianRomanEmpire'))

## Under Pope Gregory, the Christian Roman Empire gains strength. Pantheon
## becomes a Christian church.
popeGregory <- subset(saints, DECEASEDNEAR > 551 & DECEASEDNEAR <= 725)
timeData <- rbind(timeData,c(552,725,"Powerful Christian Rome", nrow(popeGregory), 'popeGregory'))

## Charlemagne's rule and Arab sack of Rome
charlemagneArab <- subset(saints, DECEASEDNEAR > 725 & DECEASEDNEAR <= 860)
timeData <- rbind(timeData,c(726,860,"Charlemagne and Arab Sacking", nrow(charlemagneArab), 'charlemagneArab'))

## The city of Rome has women rulers.
womenRule <- subset(saints, DECEASEDNEAR > 860 & DECEASEDNEAR <= 950)
timeData <- rbind(timeData,c(861,950,"Women Rule Rome", nrow(womenRule), 'womenRule'))

## darkAges <- subset(saints, DECEASEDNEAR > 476 & DECEASEDNEAR <= 800)

middleAges <- subset(saints, DECEASEDNEAR > 950 & DECEASEDNEAR <= 1300)
timeData <- rbind(timeData,c(951,1300,"Middle Ages", nrow(middleAges),'middleAges'))


## renaissance <- subset(saints, DECEASEDNEAR > 1300 & DECEASEDNEAR <= 1550)
## protestantReformation <- subset(saints, DECEASEDNEAR > 1550 & DECEASEDNEAR <= 1650)
## classicist <- subset(saints, DECEASEDNEAR > 1650 & DECEASEDNEAR <= 1750)
## romantic <- subset(saints, DECEASEDNEAR > 1750 & DECEASEDNEAR <= 1836)
## victorian <- subset(saints, DECEASEDNEAR > 1836 & DECEASEDNEAR <= 1901)
## earlyContemporary <- subset(saints, DECEASEDNEAR > 1901 & DECEASEDNEAR <= 1980)
## digital <- subset(saints, DECEASEDNEAR > 1980 & DECEASEDNEAR <= 2014)

## simple bar chart with rCharts and NVD3.
## TODO: On mouseover load the names of saints in other window/frame. !!

n1 <- nPlot(NUMBER ~ NAME, group = "NAME", data = timeData, 
            type = 'multiBarChart')

## now individual chloropleth maps with leaflet or Googlevis. On mouse click,
## will load the map of appropriate era.


## now I'd like to do a map with a slider to show changes over time ...

