## NOTE: Run Database Combination Script Prior to Use

## Set the Working Directory ----
maindir <- "*DIRECTORY PATH HERE*" #enter directory folder location here 
setwd(maindir) 

## Load Chicago Data ----
load("chicago_crimes_database.rData") #loads Spaulding Chicago Crime Data 2001-2017
district <- readOGR(dsn = "data/Boundaries - Police Districts (current)/geo_export_035df149-f04a-49a4-b531-8242fe399f25.shp") # used to load a shapefile and assign it to a new spatial object. From Chicago Data Portal.
beat <- readOGR(dsn = "data/Boundaries - Police Beats (current)/geo_export_94ecb88a-22c5-446f-80e1-e4a250f859fe.shp") # used to load a shapefile and assign it to a new spatial object. From Chicago Data Portal.


## Install/Load Required Packages ----
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", "leaflet", "data.table", "sp", "maptools", "KernSmooth")
# See "R_package_descriptions.txt" for Package Descriptions 

#install.packages(x) #installs packages in the list x
lapply(x, library, character.only = TRUE) #loads required packages in list x


## Select a Crime to Map ----
uniquecrime <- unique(full$primary_type) #choose a random crime to observe
x <- floor(runif(1, min = 0, max = 35))
x
randomcrime <- uniquecrime[x]
randomcrime
randomcrime <- "NARCOTICS" ### or set by crime type name
crime <- subset(full, primary_type==randomcrime) # subset to only see that crime type
crime <- as.data.frame(na.omit(crime)) #removes incomplete cases


## Make Contour Lines Using Kernel Density Estimator ----
bwlat <- bw.nrd0(crime$latitude) #calculate bandwidth (lat) for KDE function
bwlon <- bw.nrd0(crime$longitude) #calculate bandwidth (lon) for KDE function
crime$latitude <- as.numeric(crime$latitude)
crime$longitude <- as.numeric(crime$longitude)
kde <- bkde2D(crime[,c("longitude", "latitude")], # the order of longitude and latitude is important. Calculates the KDE using calculated bandwidths
              bandwidth=c(bwlon, bwlat), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat) #uses KDE to create contour lines


## Extract Contour Line Levels ----
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS)) 


## Convert Contour Lines To Polygons ----
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)


## Loads htmltools package for popup labels ----
library(htmltools)


## Sample Dataset for Plotting ----
crime <- crime[sample(nrow(crime), 500), ] #Randomly samples crimes by n. Eases computation load


## Create Leaflet Map ----
leaflet(crime) %>% addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addPolygons(data = beat, stroke = TRUE, color = "black", weight = 2, fillColor = "blue") %>%
  addPolygons(data = district, stroke = TRUE, color = "blue", fillColor = "transparent", 
              highlightOptions = highlightOptions(color = "red", weight = 5,
                                                 bringToFront = FALSE)) %>%
  addScaleBar(position = "bottomright") %>%
  addPolygons(data=spgons, color = heat.colors(NLEV, NULL)[LEVS]) %>% 
  addCircles(~longitude, ~latitude, popup =paste("Case Number:", crime$case_number, "<br/>"
                                                 ,"Description:", crime$description, "<br/>"
                                                 ,"District:", crime$district, "<br/>"
                                                 ,"Beat:", crime$beat, "<br/>"
                                                 ,"Date:", crime$date), color ="purple")




#### END ----