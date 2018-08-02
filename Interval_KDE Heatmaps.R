## NOTE: Run Database Combination Script Prior to Use

## Set the Working Directory ----
maindir <- "*DIRECTORY PATH HERE*" #enter directory folder location here 
setwd(maindir) 


## Install/Load Required Packages ----
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", "leaflet", "data.table", "sp", "maptools", "KernSmooth", "lubridate")
# See "R_package_descriptions.txt" for Package Descriptions 

lapply(x, library, character.only = TRUE) #loads required packages in list x


## Load Chicago Data ----
load("chicago_crimes_database.rData") #loads Spaulding Chicago Crime Data 2001-2017
district <- readOGR(dsn = "data/Boundaries - Police Districts (current)/geo_export_035df149-f04a-49a4-b531-8242fe399f25.shp") # used to load a shapefile and assign it to a new spatial object. From Chicago Data Portal.
beat <- readOGR(dsn = "data/Boundaries - Police Beats (current)/geo_export_94ecb88a-22c5-446f-80e1-e4a250f859fe.shp") # used to load a shapefile and assign it to a new spatial object. From Chicago Data Portal.


## Convert Date and Time Columns into a new Date-Time Object ----
full$date <- as.Date(full$date, "%m/%d/%Y") #ensure date column is in Date format, convert to date if not
full$time <- as.POSIXct(full$time, tz = "America/Chicago", "%H:%M:%S") #ensure time column is in time format, convert to date-time object if not
full$time <- format(full$time, format="%H:%M:%S")
full$datetime <- as.POSIXct(paste(full$date, full$time), tz = "America/Chicago", format="%Y-%m-%d %H:%M:%S") #create new column with a combined date-time class


## Calculate and Subset Data Over Recent Intervals ----
today <- Sys.Date() #gets todays date
lastw <- today %m+% weeks(-1) #gets date 1 week ago
last2w <- today %m+% weeks(-2) #gets date 2 weeks ago
lastm <- today %m+% months(-1) #gets date 1 month ago
last2m <- today %m+% months(-2) #gets date 2 months ago

#NOTE in the Chicago DB crimes are through 2017 so last week/month will return 0.
lastweek <- subset(full, full$datetime >= lastw & full$datetime <= today) #gets crimes from last week
last2weeks <- subset(full, full$datetime >= last2w & full$datetime <= today) #gets crimes from last 2 weeks
lastmonth <- subset(full, full$datetime >= lastm & full$datetime <= today) #gets crimes from last month
last2months <- subset(full, full$datetime >= last2m & full$datetime <= today) #gets crimes from last 2 months


## Calculate and Subset Data Over Specified Intervals ----
interval1 <- subset(full, full$datetime >= as.POSIXct('2016-01-01 00:01:00') &
                      full$datetime <= as.POSIXct('2016-12-31 23:59:00'))

interval2 <- subset(full, full$datetime >= as.POSIXct('2015-01-01 00:01:00') &
                      full$datetime <= as.POSIXct('2015-12-31 23:59:00'))


## Choose Crime Type to Evaluate ----
uniquecrime <- unique(full$primary_type) #random crime type chooser
x <- floor(runif(1, min = 0, max = 35))
x
randomcrime <- uniquecrime[x]
randomcrime
# Or specify particular type below:
# randomcrime <- "NARCOTICS"


## INTERVAL 1 KDE HEATMAP ----
interval <- interval1 #replace with object name of first interval of interest
crime <- subset(interval, primary_type==randomcrime)
#crime <- crime[complete.cases(crime),] # if dataset has incomplete columns run this line to remove them

# MAKE CONTOUR LINES
bwlat <- bw.nrd0(crime$latitude) #calculate bandwidth (lat) for KDE function
bwlon <- bw.nrd0(crime$longitude) #calculate bandwidth (lon) for KDE function
crime$latitude <- as.numeric(crime$latitude)
crime$longitude <- as.numeric(crime$longitude)
kde <- bkde2D(crime[,c("longitude", "latitude")], # the order of longitude and latitude is important. Calculates the KDE using calculated bandwidths
              bandwidth=c(bwlon, bwlat), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat) #uses KDE to create contour lines

# Extract Contour Line Levels
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS)) 

# Convert Contour Lines To Polygons
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)


## INTERVAL 2 KDE HEATMAP ----
interval2 <- interval2 #replace with object name of second interval of interest for comparison
crime2 <- subset(interval2, primary_type==randomcrime)
#crime2 <- crime2[complete.cases(crime2),]
crime2 <- head(crime2)
# MAKE CONTOUR LINES
bwlat2 <- bw.nrd0(crime2$latitude) #calculate bandwidth (lat) for KDE function
bwlon2 <- bw.nrd0(crime2$longitude) #calculate bandwidth (lon) for KDE function
crime2$latitude <- as.numeric(crime2$latitude)
crime2$longitude <- as.numeric(crime2$longitude)
kde2 <- bkde2D(crime2[,c("longitude", "latitude")], # the order of longitude and latitude is important. Calculates the KDE using calculated bandwidths
              bandwidth=c(bwlon2, bwlat2), gridsize = c(100,100))
CL2 <- contourLines(kde2$x1 , kde2$x2 , kde2$fhat) #uses KDE to create contour lines

# Extract Contour Line Levels
LEVS2 <- as.factor(sapply(CL2, `[[`, "level"))
NLEV2 <- length(levels(LEVS2)) 

# Convert Contour Lines To Polygons
pgons2 <- lapply(1:length(CL2), function(i)
  Polygons(list(Polygon(cbind(CL2[[i]]$x, CL2[[i]]$y))), ID=i))
spgons2 = SpatialPolygons(pgons2)


## Create KDE Heatmaps ----
# Heatmap of Interval 1
leaflet(crime) %>% addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addScaleBar(position = "bottomright") %>%
  addPolygons(data=spgons, stroke = TRUE, color = heat.colors(NLEV, NULL)[LEVS],
              opacity = 0.7, fillOpacity = 0.2)

# Heatmap of Interval 2
leaflet(crime) %>% addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addScaleBar(position = "bottomright") %>%
  addPolygons(data=spgons2, stroke = TRUE, color = topo.colors(NLEV2, NULL)[LEVS2], 
              opacity = 0.3, fillOpacity = 0.1) 

# Overlay of Heatmaps 
leaflet(crime) %>% addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addScaleBar(position = "bottomright") %>%
  addPolygons(data=spgons, stroke = TRUE, color = heat.colors(NLEV, NULL)[LEVS],
              opacity = 0.7, fillOpacity = 0.2) %>%
  addPolygons(data=spgons2, stroke = TRUE, color = topo.colors(NLEV2, NULL)[LEVS2], 
              opacity = 0.3, fillOpacity = 0.1) 


## Overlay to see which Interval has Larger Contours ----
# It is important to note which heatmap has larger contours for comparison
plot(spgons)
plot(spgons2, col = "transparent", border= "red", add=TRUE)

# IF THE RED CONTOURS (INTERVAL 2) ARE CUT OFF USE THE FOLLOWING 
# plot(spgons2)
# plot(spgons, col = "transparent", border= "red", add=TRUE)

# IF YOU ARE UNABLE TO TELL COMPARE THE BOUNDING BOX SIZES
bbox1 <- bbox(spgons)
size <- rbind(abs(bbox1[1,1]-bbox1[1,2]), abs(bbox1[2,1]-bbox1[2,2]))
bbox2 <- bbox(spgons2)
size2 <- rbind(abs(bbox2[1,1]-bbox2[1,2]), abs(bbox2[2,1]-bbox2[2,2]))
out <- size-size2
out # If out is positive, the contours of INTERVAL 1 are larger, if negative, then the contours of INTERVAL 2 are larger 
# NOTE WHICH CONTOURS ARE LARGER IN SIZE


## Create Raster of Each Heatmap Interval ---- 
# NOTE: The following works when Interval 1 has larger contours than Interval 2. Inverting the values will accommodate the contrary.
length(unique(LEVS)) #note the number of levels for each interval
length(unique(LEVS2))
# R has 100 different shades of grey to use here, divide number of layers evenly
#For example, say both LEVS and LEVS 2 each have 6 levels:
grad <- c("gray75", "gray60", "gray45", "gray30", "gray15", "gray0") # creates a color for each level

#Create and Save Plot of Interval 1 Grayscale Heatmap
png('int1.png')
plot(spgons, stroke = TRUE, col = grad[LEVS], border = grad[LEVS]) #plots interval 1 with specified grayscale gradient
dev.off()

# Create Plot of Interval 2 (smaller contours) at same size
png('int2.png')
plot(spgons, stroke = TRUE, col = "transparent", border= "transparent") #create a blank plot area of interval 1 (larger contours)
plot(spgons2, stroke = TRUE, col = grad[LEVS2], border = grad[LEVS2], add = TRUE)  #plots interval 2 (smaller) within the plot area of interval 1. Plots now have the same bounds = same geospatial size
dev.off()


## Find Net Difference Between Intervals 1 and 2 ----
p1 <- raster("int1.png") #read in the output images as a raster file
p2 <- raster("int2.png")
diff <- p1 - p2 # raster math
plot(diff, axes = FALSE, col=grey(1:100/100)) #plot difference between rasters
hist(diff) #histogram of plot level differences

# Save Net Difference Plot
png('interval_differene.png')
plot(diff, axes = FALSE, col=grey(1:100/100))
dev.off()




#### END ----