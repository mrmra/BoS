## BoS analysis -- plotting the number of saints over time.

## For this analysis we make use of the following packages:

library(NLP)
library(openNLP)
library(openNLPmodels.en)

library(sp)
library(rgdal)

library(rCharts)
library(googleVis)


## load the saints table and geo location info

saints <- read.csv("saints.csv")
saints$DECEASEDNEAR <- as.numeric(saints$DECEASEDNEAR)
geoLoc <- read.csv("googleGeo.csv")
shape <- readOGR("TM_WORLD_BORDERS_SIMPL-0.3.shp", layer="TM_WORLD_BORDERS_SIMPL-0.3")

## bin the saints based on date deceased, using subset. I had this time series
## in mind when I created the geoLocation data, so the date deceased is already
## in that table, simplifying things a little.

## Well, what I've discovered is that most of the saints in my datas
## are from late antiquity Rome. I'll need to break down the Roman
## time period in more detail. Done!

## Since early Christianity is dominated by the overbearing presence of the 
## not-so-nice Roman empire, we're counting based on Roman ages in early AD.

ageOfGospel <- subset(saints, DECEASEDNEAR >= 0 & DECEASEDNEAR <= 100)
## lateAntiquityRome <- subset(saints, DECEASEDNEAR > 100 & DECEASEDNEAR <= 476)

## Hadrian builds wall, ends in universal citizenship
hadrianPeriod <- subset(saints, DECEASEDNEAR > 100 & DECEASEDNEAR <= 200)

## Roman empire splits
beginningOfByzantium <- subset(saints, DECEASEDNEAR > 200 & DECEASEDNEAR <= 300)

## Traditional start of medieval period of European history
constantineEmpire <- subset(saints, DECEASEDNEAR > 300 & DECEASEDNEAR <= 376)

## Christianity becomes the official religion of Rome, dissidents slaughtered 
## Goths sack it a couple times, Emperor deposed, Popes take over. The 
## "Dark Ages" begin.
christianRomanEmpire <- subset(saints, DECEASEDNEAR > 376 & DECEASEDNEAR <= 551)

## Under Pope Gregory, the Christian Roman Empire gains strength. Pantheon
## becomes a Christian church.
popeGregory <- subset(saints, DECEASEDNEAR > 551 & DECEASEDNEAR <= 725)

## Charlemagne's rule and Arab sack of Rome
charlemagneArab <- subset(saints, DECEASEDNEAR > 725 & DECEASEDNEAR <= 860)

## The city of Rome has women rulers.
womenRule <- subset(saints, DECEASEDNEAR > 860 & DECEASEDNEAR <= 950)

## darkAges <- subset(saints, DECEASEDNEAR > 476 & DECEASEDNEAR <= 800)
middleAges <- subset(saints, DECEASEDNEAR > 950 & DECEASEDNEAR <= 1300)
renaissance <- subset(saints, DECEASEDNEAR > 1300 & DECEASEDNEAR <= 1550)
protestantReformation <- subset(saints, DECEASEDNEAR > 1550 & DECEASEDNEAR <= 1650)
classicist <- subset(saints, DECEASEDNEAR > 1650 & DECEASEDNEAR <= 1750)
romantic <- subset(saints, DECEASEDNEAR > 1750 & DECEASEDNEAR <= 1836)
victorian <- subset(saints, DECEASEDNEAR > 1836 & DECEASEDNEAR <= 1901)
earlyContemporary <- subset(saints, DECEASEDNEAR > 1901 & DECEASEDNEAR <= 1980)
digital <- subset(saints, DECEASEDNEAR > 1980 & DECEASEDNEAR <= 2014)

## simple chart time series with rCharts and NVD3.

## now I'd like to do a map with a slider to show changes over time ...

