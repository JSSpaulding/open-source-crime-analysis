## Set the Working Directory ----
maindir <- "*DIRECTORY PATH HERE*" #enter directory folder location here 
setwd(maindir) 


## Install/Load Required Packages ----
x <- c("ggmap", "ggplot2", "Rmisc")
# See "R_package_descriptions.txt" for Package Descriptions 

#install.packages(x) #installs packages in the list x
lapply(x, library, character.only = TRUE) #loads required packages in list x

## Google Credentials ----
# https://developers.google.com/maps/documentation/javascript/get-api-key
KEY = "*CREDENTIAL KEY HERE*" #enter your Google Maps API Key here
register_google(key = KEY, account_type = "standard")
ggmap_credentials()

## Designate TO and FROM ----
# If using addresses
from <- 'Oglebay Hall, Morgantown, WV'
to <- 'Morgantown High School, Morgantown, WV'

#If using latitude and longitude
from <- '39.636104, -79.953697'
to <- '39.624478, -79.957232'


## Create df of the top 3 routes ----
route_df <- route(from, to, structure = 'route', mode = 'driving', alternatives = TRUE)
route_df #view result
#subset to create each independent route
routeA <- subset(route_df, route=="A")
routeB <- subset(route_df, route=="B")
routeC <- subset(route_df, route=="C")


## Plot Overlay of All Routes on Map ----
map <- qmap('Morgantown', zoom = 15) +
  geom_path(
    aes(x = lon, y = lat),  colour = 'green', size = 1.5,
    data = routeA, lineend = 'round') +
  geom_path(
    aes(x = lon, y = lat),  colour = 'red', size = 1.5,
    data = routeB, lineend = 'round') +
  geom_path(
    aes(x = lon, y = lat),  colour = 'blue', size = 1.5,
    data = routeC, lineend = 'round')
map #view result

#To Save the Map
png(filename = paste(maindir, "/route_map.png", sep = ""))
plot(map)
dev.off()


## Create Multiplot of Top 3 Routes ----
route_a <- qmap('Morgantown', zoom = 15, maptype = "hybrid") +
  geom_path(
    aes(x = lon, y = lat),  colour = 'green', size = 1.5,
    data = routeA, lineend = 'round') +
  ggtitle("Route A")
route_a
route_b <- qmap('Morgantown', zoom = 15, maptype = "hybrid") +
  geom_path(
    aes(x = lon, y = lat),  colour = 'red', size = 1.5,
    data = routeB, lineend = 'round') +
  ggtitle("Route B")

route_c <- qmap('Morgantown', zoom = 15, maptype = "hybrid") +
  geom_path(
    aes(x = lon, y = lat),  colour = 'blue', size = 1.5,
    data = routeC, lineend = 'round') +
  ggtitle("Route C")

multiplot(route_a, route_b, route_c, cols = 3) #view result

#To Save the multiplot
png(filename = paste(maindir, "/route_multiplot.png", sep = ""))
multiplot(route_a, route_b, route_c, cols = 3) #result
dev.off()


## Route Function from a Centroid to List of Locations ----
from <- paste('39.636104, -79.953697') #start point
data <- read.table("route_ex.txt", header = TRUE) #destination data
tox <- data$tolat[!is.na(data$tolat)] #extract latitudes
toy <- data$tolong[!is.na(data$tolong)] #extract longitudes
coord <- cbind(paste(tox, toy, sep = ", ")) #create destination in proper format

# Loop for Route Calculation 
for (i in 1:nrow(coord)) {
  a <- route(from[i], coord[i], structure = 'route', mode = 'driving', alternatives = TRUE)
  a$route_name <- paste("route", i, sep = "")
  write.table(a, paste(maindir, "/routes_out/route", i, ".txt", sep = ""),
              col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)  
  }


## Route Function Between a List of Origin and Destination Locations ----
#uses route_ex.txt
fromx <- data$fromlat[!is.na(data$fromlat)] #extract latitudes
fromy <- data$fromlong[!is.na(data$fromlong)] #extract longitudes
start <- cbind(paste(fromx, fromy, sep = ", ")) #create destination in proper format
tox <- data$tolat[!is.na(data$tolat)] #extract latitudes
toy <- data$tolong[!is.na(data$tolong)] #extract longitudes
end <- cbind(paste(tox, toy, sep = ", ")) #create destination in proper format

# Loop for Route Calculation 
for (i in 1:nrow(coord)) {
  a <- route(start[i], end[i], structure = 'route', mode = 'driving', alternatives = TRUE)
  a$route_name <- paste("route", i, sep = "")
  write.table(a, paste(maindir, "/routes_out/sfroute", i, ".txt", sep = ""),
              col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE)  
}




#### END ----