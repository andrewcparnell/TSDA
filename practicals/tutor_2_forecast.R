## Tutorial lab 2 - The Forecast Package

#---- First section is on formatting datasets which involve dates

# R generally does not handle dates and times well...
# We'll use an R package call lubricate

install.packages("lubridate")
library(lubridate)

# ---------- Manipulate single date
# Tell R order of year, month and day
ymd("20110604")    
mdy("06-04-2011")
dmy("04/06/2011")

# Can edit a date
ymd("20110101") + dyears(1) # add a year
ymd("20110101") + ddays(2) # add two days
ymd("20110101") + dmonths(4) # error
?ddays # no dmonths

# Check for leap year
leap_year(1985)
leap_year(1884)

# Set up a meeting time
meeting <- ymd_hms("2017-06-25 08:37:00", tz = "Europe/London") # can handle time zones
meeting
with_tz(meeting, "Canada/Newfoundland")

# Set the meeting time to weekly for 6 weeks 
meetings <- meeting + weeks(0:5)
with_tz(meetings, "Canada/Newfoundland") #convert vector of times

# Compensating for month lengths
jan31 <- ymd("2017-01-31")
jan31 + months(0:11) # why does this give NA values? 

# What happens with the following code?
floor_date(jan31, "month") 
ceiling_date(jan31, "month") 
floor_date(jan31, "month") + months(0:11) + days(31)
floor_date(jan31, "month") + months(0:11) - days(1)

#-------- Comparing time intervals
# set a time 
trappings_start <- ymd_hms("2017-04-25 10:00:00", 
                           tz = "Canada/Newfoundland")
trappings_start # whoops wrong hour
hour(trappings_start) <- 16 
trappings_start

# Set time for leaving
trappings_finish <- ymd_hms("2017-07-28 11:37:00", 
                            tz = "Canada/Newfoundland")
trappings_finish

# Define period for bear trappings and breeding 
lynx_trappings <- interval(trappings_start, trappings_finish) 
lynx_trappings

breeding_period <- interval(ymd(20170205, tz = "Canada/Newfoundland"), 
                            ymd(20170427, tz = "Canada/Newfoundland"))
breeding_period

# Does the breeding and trapping period overlap?
int_overlaps(lynx_trappings, breeding_period) 

# When is the overlap?
setdiff(lynx_trappings, breeding_period)

# shift the interval for tarppings
lynx_trappings <- int_shift(bear_trappings, duration(days = 10))
int_overlaps(lynx_trappings, breeding_period)

# How long is the trappings period?
lynx_trappings/ddays(1) # in days
breeding_period/dseconds(1) # in seconds
as.period(lynx_trappings) #per time unit

#------ Manipulating time vectors for time series

date1 <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
date1
date2 <- as.Date(date1, "%d%b%Y") 
date2
is.Date(as.Date("2009-08-03"))

# Create time dataframe to play with
dates1 <- seq(as.Date("2017-01-01"), as.Date("2017-04-29"), by="days")
dates1
weekdays(dates1) # what days to these dates relate to?

# Creating random time dataframe
days <- rep(1:31, each=6)
months <- rep(1:6, times=31)
dates2 <- data.frame(days,months,2017)
head(dates2)

# Need to combine dates2 into a single variable
paste(dates2$days, dates2$months, dates2$X2017, sep= "/")
dates3 <- as.Date(paste(dates2$days, dates2$months, dates2$X2017, 
                        sep="/")," %d/%m/%Y")
dates3
dates3 <-sort(na.omit(dates3)) #remove NA and sort
dates3

# Add in a time variable
time1 <- data.frame(rep(1:30, length=181), rep(1:12, length=181), 
                    rep(1:58, length=181))
colnames(time1) <- c("sec","hour","min")
head(time1)
times1<-(paste(time1$hour, time1$min, time1$sec, sep= ":"))
head(times1)

#combining dates and time
as.POSIXct(paste(dates3, times1), format="%Y-%m-%d %H:%M:%S" , tz="EST")

#Quicker way
dates4 <- ymd(dates3) + hms(times1)
dates4

# check date period
range(dates3)

# Changing format
#as.Date(dates3, format = "%m/%d/%Y")
format(dates3, "%d/%m/%Y")

# Calling the current time
Sys.time()
now()

# create a time
make_datetime(year = 1999, month = 12, day = 22, sec = 10)

as.POSIXlt(Sys.time(), "America/New_York")

# moving a date forward by 'z' seconds
z <- 1472562988 #let z = number of seconds
as.POSIXct(z, origin = "1960-01-01")  
as.POSIXct(z, origin = dates4)
as.Date(as.POSIXct(1*60*60*24, origin = "2017-06-27", tz = "GMT"))


#------------ Second Section Forecast package -----------

library(forecast)
library(ggplot2) #  Not essential - will point out where this occurs

# Dealing with outliers in a dataset
CO2<-read.csv("data/CO2.csv")
tsdisplay(CO2$CO2_ppm)
# Replaces outliers and missing values using linear interpolation
clean_CO2<-tsclean(CO2$CO2_ppm) 
tsdisplay(clean_CO2)

# "Best" ARIMA answer
tide<-read.csv("data/tide_gauge.csv")
auto.arima(tide$sea_level_m) # based on aic
auto.arima(tide$sea_level_m, ic = "bic") # different model for bic
auto.arima(tide$sea_level_m, xreg = tide$year_AD)

# Check which models were considered for auto.arima
auto.arima(tide$sea_level_m, ic="aic", trace=TRUE) 
auto.arima(tide$sea_level_m, ic="bic", trace=TRUE) 

# Note auto.arima is sensitive to outliers

# Time series plots
Acf(tide$sea_level_m)
Acf(tide$sea_level_m, lag.max = 50) 
tsdisplay(tide$sea_level_m) # see acf, pacf and time series plot together
ggtsdisplay(tide$sea_level_m, theme = theme_bw()) #ggplot equivalent

# Fitting an ARIMA(p,d,q)
model1<-Arima(tide$sea_level_m, order = c(1,0,2), xreg = tide$year_AD)

# Random functions
ndiffs(tide$sea_level_m) #recommendation for differences
arimaorder(model1) #return order (p,d,q,P,D,Q and m)

# ------------- Forecasting ahead
tide_model<-Arima(tide$sea_level_m, order=c(0,1,1)) # best bic model
forecast(tide_model, h=10) # Why are all the forecasts the same?
plot(forecast(tide_model, h=10))

# Forecast a linear model
y <- ts(rnorm(120,0,3) + 1:120 + 20*sin(2*pi*(1:120)/12), frequency=12)
plot(y, xlab = "Years")
fit <- tslm(y ~ trend + season) # fit linear models to time series including trend and seasonality components
plot(forecast(fit, h = 20))

##seasonal adjusted data is returned
y_seasonal<-seasadj(decompose(y,"additive"))  #removing seasonal component
plot(y_seasonal)

#------ Use a transformation: Box Cox
#Transformations
#Lambda value (??)	  Transformed data (Y')
#-2	                Y^-2 = 1/Y2
#-1	                Y^-1 = 1/Y1
#-0.5	              Y^-0.5 = 1/(???(Y))
#0	                log(Y)**
#0.5	              Y0.5 = ???(Y)
#1	                Y^1 = Y
#2	                Y^2

# The "optimal value - best approximation of a normal distribution curve.

fires<-read.csv("data/forest_fires.csv")  
ggtsdisplay(fires$acres) #asymmetric data

lambda1 <- BoxCox.lambda(fires$acres, lower = -3) #calculate appropriate transformation
lambda1

plot(BoxCox(fires$acres, lambda1), type= "l")
plot(BoxCox(fires$acres, lambda = 2), type= "l") #example of other transformations
plot(BoxCox(fires$acres, lambda = -2), type= "l")

#best approx. to normal dist.
hist(fires$acres)
hist(BoxCox(fires$acres, lambda1))
hist(BoxCox(fires$acres, lambda = 3))


fires.fit <- Arima(fires$acres, order = c(1,0,0), 
                   lambda = lambda1)
forecast(fires.fit, h = 3)

# Breakdown dataset into components
fit <- stl(CO2$CO2_ppm, s.window="periodic") # not seasonal

data("nottem")
fit <- stl(nottem, s.window="periodic")
lines(trendcycle(fit),col="red")
autoplot(cbind(
  Data = nottem,
  Seasonal = seasonal(fit),
  Trend = trendcycle(fit),
  Remainder = remainder(fit)),
  facets = TRUE) +
  xlab("Year") + theme_bw()

# ----- Other models apart from ARIMA
#Neural Networks - idea: based on the human brain: lots of linear eqtns
fit_n1 <- nnetar(tide$sea_level_m)
fit_n1
fcast <- forecast(fit_n1)
plot(fcast)

fit_n2 <- nnetar(tide$sea_level_m, repeats = 30, maxit = 150, 
                 size=3, decay = 5e-4)
fit_n2
fcast <- forecast(fit_n2)
plot(fcast)

# Exponential Smoothing - idea: based on how well we did in past forecasts
# Good for forecasts with no trend or seasonal components

# Single Exponential Smoothing (SES) - ARIMA(0,1,1)
# Now we observe yt and wish to make a forecast Ft. 
# We do this by taking our old forecast Ft, choose a (0 < a < 1) and
# adjusting it using the error in forecasting yt as follows:
# Ft+1 = Ft + a*(yt-Ft)

# Double Exponential Smoothing (DES) - Holt's Linear Model - ARIMA(0,2,2)
# Adjust level and slope

#Table 4.2: Holt-Winters Algorithms:
# Time series patterns:
#     trend      no     yes   no/yes
#     seasonal   no     no    yes
#     noise      yes    yes   yes
#     Algorithms SES    DES
#     parameters a      (a,b)

# Seasonal HoltWinter's Additive Model Algorithm (noted SHW+)
# ARIMA(0,1,s+1)(0,1,0)_s
# estimates level, trend, seasonal (3 parameters to estimate)

# HoltWinter's Multiplicative Model Algorithm - no ARIMA equivalent

?ets
ets(tide$sea_level_m)  #ETS- (error type, trend type, season type)
ets(fires$acres)
ets(clean_CO2, allow.multiplicative.trend = TRUE)
ets(nottem, allow.multiplicative.trend = TRUE)
ets(AirPassengers, allow.multiplicative.trend = TRUE)

#splinef - The cubic smoothing spline model is equivalent to an 
# ARIMA(0,2,2) model but with a restricted parameter space. 
# The advantage of the spline model over the full ARIMA model 
# is that it provides a smooth historical trend as well as a 
# linear forecast function.

forecast_spline <- splinef(tide$sea_level_m)
forecast_spline

fcast <- splinef(tide$sea_level_m, method = "mle", 
                 h = 15, level = c(70, 90))
summary(fcast)
plot(fcast)

# Tomorrow work through series using all forecast steps including 
# investigating residuals and accuracy