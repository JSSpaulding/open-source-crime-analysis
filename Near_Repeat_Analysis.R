## NOTE: Run Database Combination Script Prior to Use

## Set the Working Directory ----
maindir <- "*DIRECTORY PATH HERE*" #enter directory folder location here 
filepath <- "*DIRECTORY PATH*/near-repeats-out/cases"
netpath <- "*DIRECTORY PATH*/near-repeats-out/linkage-figs"
setwd(maindir)


## Specify Thresholds for Near Repeat Analysis ----
DistThresh <- 1000 #specify distance threshold for near crimes (in m)
TimeThresh <- 2 #specify time threshold for near crimes (in days)


## Install/Load Required Packages ----
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", "leaflet", "data.table", "sp", "KernSmooth", "lubridate", "igraph")
# See "R_package_descriptions.txt" for Package Descriptions 

#install.packages(x) #installs packages in the list x
lapply(x, library, character.only = TRUE) #loads required packages in list x


## Load Chicago Data ----
load("chicago_crimes_database.rData") #loads Spaulding Chicago Crime Data 2001-2017
full <- na.omit(full) #removes incomplete cases (rows with NA's)


## Convert Date and Time Columns into a new Date-Time Object ----
full$date <- as.Date(full$date, "%m/%d/%Y") #ensure date column is in Date format, convert to date if not
full$time <- as.POSIXct(full$time, tz = "America/Chicago", "%H:%M:%S") #ensure time column is in time format, convert to date-time object if not
full$time <- format(full$time, format="%H:%M:%S")
full$datetime <- as.POSIXct(paste(full$date, full$time), tz = "America/Chicago", format="%Y-%m-%d %H:%M:%S") #create new column with a combined date-time class


## Notes About Capacity and Subsetting (if necessary) ----
# NOTE: Will only work with datasets of 25000 incidents or less 
# Tested with Windows 64-Bit, 32GB RAM  

#If necessary to shrink dataset, subset by a time interval and make multiple iterations
interval <- subset(full, full$datetime >= as.POSIXct('2015-01-01 00:01:00') &
                     full$datetime <= as.POSIXct('2015-12-31 23:59:00'))

#If looking for a singular crime type
#crime <- subset(full, full$primary_type == "BURGLARY") #use without interval subset for all cases; if possible
crime <- subset(interval, interval$primary_type == "BURGLARY") #for use with interval subset. Recommended to reduce memory strain


## Converting Latitude and Longitude to UMT ----
cord.dec = SpatialPoints(cbind(crime$longitude, crime$latitude), proj4string = CRS("+proj=longlat")) #forms an object of spatial points class

# Transforming coordinate to UTM using EPSG=32616 for WGS=84, UTM Zone=16N, Northern Hemisphere: central US)
# For US UTM zones see: 
# https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system#/media/File:Utm-zones-USA.svg
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32616")) #change to appropriate EPSG. convert (lat,lon) to coordinate system in EPSG 
coordsout <- as.data.frame(cord.UTM@coords) #makes df of coordinates
crime$x1 <- coordsout$coords.x1 #bind coordinate 1 to crime data
crime$x2 <- coordsout$coords.x2 #bind coordinate 2 to crime data
crime <- na.omit(crime) #omit any crimes with N/A from calculations


## Near Repeat Analysis using Threshold Parameters ----
SpatDist <- as.matrix(dist(crime[,c('x1','x2')])) < DistThresh  #return 1's if incident under distance
TimeDist <- as.matrix(dist(crime[,'date'])) < TimeThresh #return 1's if incident under time
AdjMat <- SpatDist * TimeDist #check for incidents under both distance and under time
row.names(AdjMat) <- crime[,'case_number'] #get case numbers for labels in igraph
colnames(AdjMat) <- crime[,'case_number'] #get case numbers for labels in igraph
G <- graph_from_adjacency_matrix(AdjMat, mode="undirected", diag = FALSE) #create igraph network from adjacency matrix
CompInfo <- components(G) #assigning the connected components
out <- data.frame(CompId=CompInfo$membership, CompNum=CompInfo$csize[CompInfo$membership]) #get near repeat series
out <- out[out$CompNum!=1, ] #remove any series consisting of 1 incident
#NOTES for `out'
#The CompId field is a unique Id for every string of events. 
#The CompNum field states how many events are within the string. 


## Create Table of Incidents for Each Near Repeat Series ----
datalist <- split(out , f = out$CompId) #create list of each identified series
rm(list=setdiff(ls(), c("datalist","crime","maindir","filepath", "netpath", "DistThresh","TimeThresh"))) #free up system memory by removing objects

name <- 1
for (i in datalist) {
  cases <- rownames(i) #get case numbers of series
  a <- crime[crime$case_number %in% cases,] #get incident information of case numbers
  write.table(a, paste(filepath, "/series", name, ".txt", sep = ""),
              col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE) #save table of incidents to filepath directory
  SpatDist <- as.matrix(dist(a[,c('x1','x2')])) < DistThresh #compute adjacency matrix for each series
  TimeDist <- as.matrix(dist(a[,'date'])) < TimeThresh
  AdjMat <- SpatDist * TimeDist
  row.names(AdjMat) <- a[,'case_number']
  colnames(AdjMat) <- a[,'case_number']
  G <- graph_from_adjacency_matrix(AdjMat, mode="undirected", diag = FALSE) #create network of cases from each series
  png(file = paste(netpath, "/series", name, ".png",sep = "")) #save image of igraph network to netpath directory
  plot(G)
  dev.off()
  name <- name + 1
}


rm(list=setdiff(ls(), c("maindir","filepath", "netpath"))) #free up system memory by removing objects


## Make Leaflet Map for Each Series ----
file_list <- list.files(path=filepath, pattern="*.txt") #lists all files in directory
file_list <- paste(filepath, "/", file_list, sep="") #names of all files in directory

# Watch out for unix sort/numbering conventions! Numbers inside file names are not numerically sortable. ls (and so the vector here) will count them 1, 10, 11, 12 ... 2, 20, 21, 22, ... etc. Reorder them to get the numbers right.
# Extract the numbers and reorder the names properly.
ind <- order(as.numeric(gsub("[^[:digit:]]", "", file_list)))
file_list <- file_list[ind]


setwd(filepath)
library(htmlwidgets)

#Create Leaflet map and save to netpath directory
name <- 1
for (file in file_list){
  a <- read.table(file, header=TRUE, sep="\t")
  map <- leaflet(a) %>% addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addScaleBar(position = "bottomright") %>%
  addMarkers(~longitude, ~latitude, label = a$case_number, labelOptions = labelOptions(noHide = T, textsize = "15px"), 
             popup =paste("Case Number:", a$case_number, "<br/>"
                          ,"Description:", a$description, "<br/>"
                          ,"Location Description:", a$location_description, "<br/>"
                          ,"Date:", a$date, "<br/>"
                          ,"Time:", a$time))
  saveWidget(map, file = paste(netpath, "/series", name, ".html",sep = ""))
  name <- name + 1

  
}




#### END ----