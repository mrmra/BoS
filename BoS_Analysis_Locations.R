## BoS analysis -- natural language processing to pull out location refs from
##                 body of description text.

## For this analysis we make use of the following packages:

library(NLP)
library(openNLP)
library(openNLPmodels.en)

library(sp)
library(rgdal)

library(rCharts)
library(googleVis)


## load the saints table

saints <- read.csv("saints.csv")

## initialize the map we'll use, from the Leafletjs library -- Ultimately just
## used googleVis for quicker development cycle on location data, would like to
## go back and learn Leaflet later.

## var map = L.map('map').setView([51.505, -0.09], 13);
##L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {
##    attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
##}).addTo(map);
##L.marker([51.5, -0.09]).addTo(map)
##.bindPopup('A pretty CSS3 popup. <br> Easily customizable.')
##.openPopup();

## Map from googleVis

## First load shape file with country names, geoJSON, lat/long, etc...
## then prep the Google data object for later writing.

shape <- readOGR("TM_WORLD_BORDERS_SIMPL-0.3.shp", layer="TM_WORLD_BORDERS_SIMPL-0.3")
googleGeo <- data.frame()


## Using natural language processing (OpenNLP) to extract place names from
## description passages.
## (also known as NERC -- Named Entity Recognition and Classification).

## setup the initial token analyzers so we don't repeat it each time in loop
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
entity_annotator <- Maxent_Entity_Annotator(language="en", kind="location")

## loop through all the saints to pull out location data, and a couple other 
## things it might be useful to have in a geo data table for later analysis

for(i in 1:nrow(saints)){
rSDesc <- as.String(saints[i,'DESCRIPTION'])
rSN <- as.String(saints[i, 'NAME'])
rSDate <- as.numeric(saints[i, 'DECEASEDNEAR'])

a2 <- annotate(rSDesc,list(sent_token_annotator,word_token_annotator))

## some of the tokenizing (chopping the description passage into words) 
## generated errors, probably because of special characters and the size of
## some words. Rather than stop the show, we'll simply catch the error
## and allow the loop to continue.

tryCatch({placeNames <- rSDesc[entity_annotator(rSDesc,a2)]}, 
          error=function(e){
            print(e)
            placeNames <- 0
          }
        )

## if there isn't a place name, no need to waste processing cycles, so we'll
## test for that.
## Now an embarrassing point -- I coded this section after midnight and
## after many hours of coding. I was very tired. I kept have a string error
## stating that there was a missing value -- it was because placeNames sometimes
## has one element and sometimes many.
## It took me 30 minutes to figure out the error and I ultimately picked apart
## the code way more than I needed to -- if statements do not make use of the 
## logical OR (||) and there's an unnecessary tryCatch.

if(length(placeNames) >= 1){
    ## might as well take this opportunity to improve the main saints table
    saints[i,'LOCATIONS'] <- as.character(as.String(placeNames))
    
    ## My post-midnight crusade to correctly ID 'Britain' and 'England' 
    ## references as the United Kingdom so we can geo them correctly.
    
    for(f in 1:length(placeNames)){
        if(length(placeNames) > 1){
               if(placeNames[f] == 'Britain'){
                placeNames[f] <- 'United Kingdom'
            }
            if(placeNames[f] == 'England'){
                placeNames[f] <- 'United Kingdom'
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
    
    ## compare the place names to names in the shape file to pull out just
    ## the countries for mapping.
    
    tempSelection <- match(placeNames, shape$NAME)
    shapeSelections <- na.omit(tempSelection)
    }
    
    ## next we make a temporary data frame and append the data to the geo 
    ## dataframe we'll use for the Google map visualization.
    
if(length(shapeSelections) > 0) {
    shapeDF <- as.data.frame(shape[shapeSelections,])
    shapeDF['GDESC'] <- rSN 
    shapeDF['GVALUE'] <- 1
    shapeDF['GYEAR'] <- rSDate
    googleGeo <- rbind(googleGeo,shapeDF)
    
    ## a little cleanup because the underlying Java library is freaking out 
    ## about all the garbage collection.
    rm(shapeDF)
    }
rm(shapeSelections)
}

## some saints had multiple references to places, but while saints may 
## reference multiple places, we don't want the same saint to reference Ireland
## 4 times because it'll throw off our count. So we'll remove duplicates.

googleGeo<-unique(googleGeo)

for(i in 1:nrow(as.data.frame(shape))){
    name <- as.data.frame(shape[i,'NAME'])
    subName <- subset(googleGeo, NAME == as.String(name$NAME))
    num <- nrow(subName)
    
    if(num > 0){
        googleGeo[row.names(subName),'GVALUE'] <- num    
    }
}

## call to googleVis lib to make our map object for plotting.

G1 <- gvisGeoMap(googleGeo, locationvar="NAME", numvar="GVALUE", hovervar="NAME",
        options=list())

## and finally a quick visual comes together and we write our hard work,
## because this analysis takes a good 10 minutes to run on my laptop!

plot(G1)
write.csv(googleGeo, "googleGeo.csv")


## this was a fun evening (and early morning!) of data hacking. Not too pretty, 
## but we've got a good locations table to work with, and a framework for future 
## natural language data chunking.