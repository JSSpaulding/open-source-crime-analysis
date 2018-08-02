## Set the Working Directory ----
maindir <- "-DIRECTORY PATH HERE-" #enter directory folder location here 
setwd(maindir) 


## Install/Load Required Packages ----
library(ggmap)
# See "R_package_descriptions.txt" for Package Descriptions 
# Note the daily request limits for free usage
# https://cloud.google.com/maps-platform/pricing/sheet/


## Google Credentials ----
# https://developers.google.com/maps/documentation/javascript/get-api-key
KEY = "*CREDENTIAL KEY HERE*" #enter your Google Maps API Key here
register_google(key = KEY, account_type = "standard")
ggmap_credentials()


## OPTION 1: Specify Data for Single Request ----
address <- "1600 University Ave., Morgantown, WV" #specify the address in the quotes for geocoding
latitude <- 39.6361 #specify the latitude for reverse geocoding
longitude <- -79.95372 #specify the longitude for reverse geocoding

# Geocode
out <- geocode(address, output = "latlona", source = "google", override_limit = FALSE)
out #result

# Reverse Geocode
coord <- cbind(longitude, latitude)
out1 <- revgeocode(coord, "more")
out1 #result


## OPTION 2: Specify Data for List of Requests ----
data <- read.csv("geocode_data.csv") #read in data

#Geocode Addresses or Building Names
data1 <- head(data,n=5) #takes first 5 from example data (addresses)
addresses <- as.character(data1$address) #makes address character object

geocodes <- geocode(addresses, output = "latlona", source = "google", override_limit = FALSE)
geocodes #result
write.csv(geocodes, "geocodes_out.csv") #write results to a .csv file

#Reverse Geocode Latitude Longitude Pairs 
data2 <- tail(data,n=5) #takes last 5 from example data (coords)
data2$address <- NULL #removes blank address column
lon <- data2$longitude
lat <- data2$latitude

rgeocodes <- mapply(FUN = function(lon, lat) {
  revgeocode(c(lon, lat), output = "more") 
  }, 
  data2$longitude, data2$latitude
)
result <- cbind(data2, data.table::rbindlist(rgeocodes, fill = TRUE)) #result
write.csv(result, "reverse_geocode_out.csv") #write results to a .csv file




#### END ----