## BoS analysis

## For this analysis we make use of the following packages:
## OpenNLP, rCharts, knitr, Slidify

library(NLP)
library(openNLP)
library(openNLPmodels.en)

library(sp)
library(rgdal)

library(rCharts)
library(googleVis)


## load the saints table

saints <- read.csv("saints.csv")

## initialize the map we'll use, from the Leafletjs library

## var map = L.map('map').setView([51.505, -0.09], 13);
##L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {
##    attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
##}).addTo(map);
##L.marker([51.5, -0.09]).addTo(map)
##.bindPopup('A pretty CSS3 popup. <br> Easily customizable.')
##.openPopup();

## Map from googleVis
shape=readOGR("TM_WORLD_BORDERS_SIMPL-0.3.shp", layer="TM_WORLD_BORDERS_SIMPL-0.3")
googleGeo <- data.frame()


## Using natural language processing (OpenNLP) to extract place names from
## description passages.
## (also known as NERC -- Named Entity Recognition and Classification).

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
entity_annotator <- Maxent_Entity_Annotator(language="en", kind="location")

## FIX BRITAIN!!

for(i in 1:nrow(saints)){
rSDesc <- as.String(saints[i,'DESCRIPTION'])
rSN <- as.String(saints[i, 'NAME'])
rSDate <- as.numeric(saints[i, 'DECEASEDNEAR'])

a2 <- annotate(rSDesc,list(sent_token_annotator,word_token_annotator))
tryCatch({placeNames <- rSDesc[entity_annotator(rSDesc,a2)]}, 
          error=function(e){
            print(e)
            placeNames <- 0
          }
        )


if(length(placeNames) >= 1){
    saints[i,'LOCATIONS'] <- as.character(as.String(placeNames))
    for(f in 1:length(placeNames)){
        if(length(placeNames) > 1){
           tryCatch({
               if(placeNames[f] == 'Britain'){
                placeNames[f] <- 'United Kingdom'
            }
            if(placeNames[f] == 'England'){
                placeNames[f] <- 'United Kingdom'
            }
           },
           error=function(e){
               print(e)
               placeNames <- 0
           }
           )
        }
        else {
            if(placeNames == 'Britain'){
                placeNames <- 'United Kingdom'
            }
            if(placeNames == 'England'){
                placeNames <- 'United Kingdom'
            }
        }
    }
    tempSelection <- match(placeNames, shape$NAME)
    shapeSelections <- na.omit(tempSelection)
    }
    
if(length(shapeSelections) > 0) {
    shapeDF <- as.data.frame(shape[shapeSelections,])
    shapeDF['GDESC'] <- rSN 
    shapeDF['GVALUE'] <- 1
    shapeDF['GYEAR'] <- rSDate
    googleGeo <- rbind(googleGeo,shapeDF)
    rm(shapeDF)
    }
rm(shapeSelections)
}

googleGeo<-unique(googleGeo)

for(i in 1:nrow(as.data.frame(shape))){
    name <- as.data.frame(shape[i,'NAME'])
    subName <- subset(googleGeo, NAME == as.String(name$NAME))
    num <- nrow(subName)
    
    if(num > 0){
        googleGeo[row.names(subName),'GVALUE'] <- num    
    }
}

G1 <- gvisGeoMap(googleGeo, locationvar="NAME", numvar="GVALUE", hovervar="NAME",
        options=list())
plot(G1)

## Question 1: Where are saints most common?

## -- How about over time?
