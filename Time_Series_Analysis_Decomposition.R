## NOTE: Run Database Combination Script Prior to Use

## Set the Working Directory ----
maindir <- "*DIRECTORY PATH HERE*" #enter directory folder location here 
setwd(maindir) 


## Install/Load Required Packages ----
x <- c("dplyr", "lubridate", "TTR")
# See "R_package_descriptions.txt" for Package Descriptions 

#install.packages(x) #installs packages in the list x
lapply(x, library, character.only = TRUE) #loads required packages in list x


## Load Chicago Data ----
load("chicago_crimes_database.rData") #loads Spaulding Chicago Crime Data 2001-2017


## Select Crime Type ----
crimetype <- "HOMICIDE" #specify the crime type you wish to analyze
crime <- subset(full, full$primary_type == crimetype) #subset dataset by that crime type


## Transform Data into Time Series ----
crime$date <- as.Date(crime$date,"%m/%d/%Y")
crime$my <- format(as.Date(crime$date), "%m/%Y") #Add Month/Year Column
dates <- unique(crime$my) #get unique dates
z <- as.data.frame(table(crime$my))
str(z) #display the internal structure of z
z$Var1 <- parse_date_time(z$Var1, orders = "%m/%Y") #parses an z into POSIXct date-time object
z$Var1 <- z[order(as.Date(z$Var1)),] #order by date
data <- z[,1]
out <- data$Freq #Output incident frequencies


## Time Series Conversion and Plot ----
crimeseries <- ts(out, frequency=12, start=c(2001,1)) #frequency=12 months, specify start date and that data is monthly (2001,1)
crimeseries #view df of incident frequencies
plot.ts(crimeseries) #plot time series


## Moving Averages and Decomposing Time Series ----
crimeseriesSMA3 <- SMA(crimeseries,n=3) #calculate moving average using 3 months
plot.ts(crimeseriesSMA3)
crimeseriesSMA8 <- SMA(crimeseries,n=8) #calculate moving average using 3 months
plot.ts(crimeseriesSMA8)
crimeseriescomponents <- decompose(crimeseries) #decomposition of time series
crimeseriescomponents$seasonal #get the estimated values of the seasonal component
plot(crimeseriescomponents, xlab="Date") #plot decomposed times series

#Save decomposition plot
png(filename = paste(maindir, "/", crimetype, "_ts_decomposition.png", sep = ""))
plot(crimeseriescomponents, xlab="Date") #plot decomposed times series
dev.off()

#View Time Series without Seasonality
crimeseriesseasonallyadjusted <- crimeseries - crimeseriescomponents$seasonal #remove seasonal component
plot(crimeseriesseasonallyadjusted)

## Forecasts using Holt Winters Exponential Smoothing ----
#Forecast over data time period
plot.ts(crimeseries)
crimeseriesforecasts <- HoltWinters(crimeseries, beta=FALSE, gamma=FALSE) #forecasts for the time period covered by the data
crimeseriesforecasts #view exponential smoothing parameters
crimeseriesforecasts$fitted #forecasted time seties
plot(crimeseriesforecasts) #plot of HW forecast over time series
crimeseriesforecasts$SSE #sum of squared errors for the in-sample forecast errors

# Recalculate using set initial value
start <- z$Freq[1]
HoltWinters(crimeseries, beta=FALSE, gamma=FALSE, l.start=start) #use the first value in the time series as the initial value for the level

#Forecast of future incident counts
#install.packages("forecast")
library("forecast")
crimeseriesforecasts2 <- forecast(crimeseriesforecasts, h=12) #create forecast
crimeseriesforecasts2 #view calculated results
plot(crimeseriesforecasts2) #view plot of calculated results with 80 and 95 percent confidence intervals

crimeseriesforecasts2$residuals <- na.omit(crimeseriesforecasts2$residuals)
acf(crimeseriesforecasts2$residuals, lag.max=20, na.action=na.exclude) #calculate a correlogram of the in-sample forecast errors
Box.test(crimeseriesforecasts2$residuals, lag=20, type="Ljung-Box") #test whether there is significant evidence for non-zero correlations at lags 1-20
plot.ts(crimeseriesforecasts2$residuals) #check whether the forecast errors have constant variance. In this case the fluctuations appear to have roughly constant variance over time

#Function to plot forecast errors
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


plotForecastErrors(crimeseriesforecasts2$residuals) #result. In this case the plot shows that the distribution of forecast errors is roughly centred on zero, and is more or less normally distributed





#### END ----