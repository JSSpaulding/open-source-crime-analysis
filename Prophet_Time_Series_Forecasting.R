## NOTE: Run Database Combination Script Prior to Use

## Set the Working Directory ----
maindir <- "*DIRECTORY PATH HERE*" #enter directory folder location here 
setwd(maindir) 


## Install/Load Required Packages ----
x <- c("dplyr", "lubridate", "prophet")
# See "R_package_descriptions.txt" for Package Descriptions 

#install.packages(x) #installs packages in the list x
lapply(x, library, character.only = TRUE) #loads required packages in list x


## Load Chicago Data ----
load("chicago_crimes_database.rData") #loads Spaulding Chicago Crime Data 2001-2017


##  Select Crime Type ----
unique(full$primary_type)
crimetype <- "THEFT" #specify the crime type you wish to analyze
crime <- subset(full, full$primary_type == crimetype)  #subset dataset by that crime type


## Transform Data for Prophet fxn ----
crime$date <- as.Date(crime$date,"%m/%d/%Y")
crime$mod <- format(as.Date(crime$date), "%Y-%m-%d") #ADD Month/Year Column
dates <- unique(crime$date) #get unique dates
z <- as.data.frame(table(crime$mod)) #frequency per day
cols <- c("ds", "y") #rename columns
colnames(z) <- cols
str(z) #display the internal structure of z


## Quick Start ----
m <- prophet(z)   ##performs fitting and returns a model object

future <- make_future_dataframe(m, periods = 365) # function takes the model object and a number of periods to forecast and produces a suitable dataframe. By default it will also include the historical dates so we can evaluate in-sample fit.
tail(future) #see end days of future prediction
forecast <- predict(m, future)  # function to get forecast. has additional columns for uncertainty intervals and seasonal components
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast, xlabel = "Date", ylabel = "Number of Incidents") #plot the forecast
# Save forecast plot from viewer 
dev.copy(png,'PLOT_NAME_HERE.png')
dev.off()

prophet_plot_components(m, forecast) #forecast broken down into trend, weekly seasonality, and yearly seasonality
#save Decomposition plot from viewer
dev.copy(png,'DECOMPOSITION_PLOT_NAME_HERE.png')
dev.off()


## TREND CHANGEPOINTS ----
# Automatic changepoint detection in Prophet. Potentially useful for policy evaluation
m <- prophet(z, changepoint.prior.scale = 0.5) #strength of the sparse prior using the input argument
forecast <- predict(m, future)
plot(m, forecast)

m <- prophet(df, changepoint.prior.scale = 0.001) #trend less flexible
forecast <- predict(m, future)
plot(m, forecast)

# Specifying the locations of the changepoints Manually
m <- prophet(df, changepoints = c('2014-01-01'))
forecast <- predict(m, future)
plot(m, forecast)




#### END ----