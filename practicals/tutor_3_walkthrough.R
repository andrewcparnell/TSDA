## Tutorial lab 3 - Walkthrough examples of time series analysis

rm(list=ls())
library(forecast)
library(R2jags)
library(rjags)
library(ggplot2)


##############################
#class_3_ARMA
##############################

#1. Plot the data and the ACF/PACF
#2. Decide if the data look stationary or not. If not, perform a
#   suitable transformation and return to 1. If the data has a
#   strong trend or there is a high degree of autocorrelation
#   try 1 or 2 differences
#3. Guess at a suitable p and q for an ARMA(p, q) model
#4. Fit the model
#5. Try a few models around it by increasing/decreasing p and q
#   and checking the AIC (or others)
#6. Check the residuals
#7. Forecast into the future

# ----- First section: Time Series 1: Basic Example: 
# ----- HadCRUT is the dataset of monthly instrumental temperature records

#1. Plot the data and the ACF/PACF
hadcrut<-read.csv("data/hadcrut.csv") # HadCRUT is the dataset of monthly instrumental temperature records
head(hadcrut)
ggtsdisplay(hadcrut$Anomaly) # Familarise with dataset

#2. Decide if the data look stationary or not.
# Test for trend-stationary
t = seq_along(hadcrut$Anomaly)
trendseries = lm(hadcrut$Anomaly ~ t)
detrended_series = hadcrut$Anomaly - trendseries$fitted.values
ggtsdisplay(detrended_series) # Not trend-stationary

# Try difference-stationary

ndiffs(hadcrut$Anomaly)
Anomaly_diff<-diff(hadcrut$Anomaly)

#3. Guess at a suitable p and q for an ARMA(p, q) model
ggtsdisplay(Anomaly_diff) # AR models look similar to ARMA 
# Try AR(2)?

#4. Fit the model
model1 <- Arima(hadcrut$Anomaly, order = c(2,1,0), include.drift = TRUE)  
model1 #AIC = -262.57

# require(lmtest) # Not a prerequisite but an extra two lines
# coeftest(model1) # Check whether terms are relevant

#Q5 Overfitting a model
# Overfit in AR direction (Add extra AR terms)
model2 =  Arima(hadcrut$Anomaly, order=c(3,1,0), include.drift = TRUE)
model2  # AIC = -272.34

# Overfit in AR direction (Add extra AR terms)
model3 =  Arima(hadcrut$Anomaly, order = c(4,1,0), include.drift = TRUE)
model3  # AIC = -270.62 

# Overfit in MA direction
model4 =  Arima(hadcrut$Anomaly, order = c(3,1,1), include.drift = TRUE)
model4 # AIC = -272.62 

model5 =  Arima(hadcrut$Anomaly, order = c(3,1,2), include.drift = TRUE)
model5 # AIC = -275.52

# Comparing our model to an ARIMAX model
auto.arima(hadcrut$Anomaly, trace = TRUE)  # AIC = -274.32
auto.arima(hadcrut$Anomaly, xreg=hadcrut$Year)  #AIC = -272.03 

# set auto.arima model as our model to use
model_new =  Arima(hadcrut$Anomaly, order = c(2,1,1), include.drift = TRUE) 
model_new #AIC = -274.32

#6. Check the residuals
residuals <- model_new$res 
fit <- fitted(model_new)

qqnorm(residuals)
qqline(residuals) # Want points along line
acf(residuals)  # Check residuals don't correlate with themselves
plot(fit, residuals) # Want random scatter
hist(residuals, breaks = 30) # Want normal distribution
tsdiag(model_new, gof.lag = 30) # Combines plots
checkresiduals(model_new) # Combines plots

#7. Forecast into the future
forecast(model_new, h = 10)
plot(forecast(model_new ,h = 10))

# Holt-Winters or splines?
forecast_spline <- splinef(hadcrut$Anomaly, h = 10)
summary(forecast_spline)
plot(forecast_spline)
holt <- ets(hadcrut$Anomaly) 
plot(forecast(holt, h = 10))

# ------- Second section: Lynx Dataset - Cyclic Pattern - Not fixed period

lynx<-read.csv("data/lynx.csv")

# Start by getting a feel of the data and checking it's format
head(lynx) #check format
with(lynx, plot(year , number, type ='l'))
ggtsdisplay(lynx$number) # from class_1_AR recognise repeating pattern in ACF
# have issue with increasing mean - investigate non-stationarity

# Log transformation makes the peaks and troughs appear in the same pattern
lynx_log <- log(lynx$number)
ggtsdisplay(lynx_log)

ggtsdisplay(diff(lynx$number)) # not great see PACF

lambda <- BoxCox.lambda(lynx$number)
lambda

# auto.arima(lynx$number) # see if our answer matches the packages suggestion
auto.arima(lynx$number, lambda = lambda, trace = TRUE) #AIC = 408.93 

# Ar fits am AR series based on AIC
lynx.fit <- ar(BoxCox(lynx$number, lambda))
plot(forecast(lynx.fit, h = 20, lambda = lambda))

# model used in literature White Tong (1977)
fit1 <- Arima(lynx$number, order = c(11,0,0), lambda = lambda) #Series is the original series
fit1  #AIC = 394.57

# Forecasting transformed data
plot(forecast(fit1, h = 15)) 

# Cross Fold Validation and forecast from neural network models
modelcv <- CVar(lynx$number, k = 5, lambda = lambda)  # Currently applies a neural network model
print(modelcv)
print(modelcv$fold1)

# ------ Third Section: Seasonality example and comparing models using accuracy functions 
data("nottem") # air temperatures around Nottingham castle

# Start by getting a feel of the data and checking it's format
head(nottem) #check format
ggtsdisplay(nottem)

# Breakdown time series to trend, seasonal and remainder
fit <- stl(nottem, s.window="periodic")
autoplot(cbind(
  Data = nottem,
  Seasonal = seasonal(fit),
  Trend = trendcycle(fit),
  Remainder = remainder(fit)),
  facets = TRUE) +
  xlab("Year") +theme_bw()

# Take some data for modelling - leave remainder for forecast comparison
nott <- window(nottem, end = c(1936,12))

fit1 <- auto.arima(nott, trace = TRUE)
fit1 <- auto.arima(nott)
fit1 # aic =1091.07

ggtsdisplay(diff(nott, lag=12)) #never use more than one seasonal diff

# Use AIC to compare within ARIMA models
fit2 <- Arima(nott, order = c(0,0,1), list(order = c(0,1,1), period = 12)) # Seasonal lag is neg so try SMA
fit2 # aic = 899.96

fit3 <- Arima(nott, order = c(0,0,2), list(order = c(0,1,1), period = 12))
fit3
# aic = 897.02

fit4 <- Arima(nott, order = c(0,0,1), list(order = c(0,1,2), period = 12))
fit4 # aic = 892.66

fit5 <- Arima(nott, order = c(1,0,0), list(order = c(1,1,0), period = 12))
fit5 # aic = 912.14

ggtsdiag(fit3) # Check residuals
ggtsdiag(fit4)
qqnorm(fit4$residuals)
qqline(fit4$residuals)
qqnorm(fit3$residuals)
qqline(fit3$residuals)

#Forecast ahead by 36 places and then compare using MAPE, MSE etc.
forecast_a <- forecast(fit3, h = 36) # arima forecast
forecast_m <- meanf(nott, h = 36) # mean of data
forecast_rw <- rwf(nott, h = 36)  # random walk
forecast_srw <- snaive(nott, h = 36) # Y[t]=Y[t-m] + Z(t) Z~Normal iid
plot(forecast_a)

nott_for <- window(nottem, start = c(1937,1))

accuracy(forecast_a, nott_for)
accuracy(forecast_m, nott_for)
accuracy(forecast_rw, nott_for)
accuracy(forecast_srw, nott_for)

# Exponential smoothing can be used to make short-term forecasts for time series data.
fit6 <- ets(nott, allow.multiplicative.trend = TRUE) #ETS - (error type, trend type, season type)
summary(fit6)
forecast_ets <- forecast(fit6, h = 36)

# Could have dealt with seasonality by setting it as a covariate.....
nott_ts <- ts(nott, frequency = 12,
              start = c(1920, 1))
fit_cov <- tslm(nott_ts ~ trend + season)
plot(forecast(fit_cov, h = 36))
forecast_cov <- forecast(fit_cov, h = 36)

fit6<-nnetar(nott, repeats = 20, maxit = 200)
forecast_nn<-forecast(fit6, h = 36)

# Which is best? 
accuracy(forecast_a, nott_for)
accuracy(forecast_ets, nott_for)
accuracy(forecast_nn, nott_for) 
accuracy(forecast_cov, nott_for)

plot(forecast_ets)
plot(forecast_cov)

# Beware of artificial seasonality owing to months having different lengths

# --------------- Fourth section: Quick Jags example 
# --------------- Further detail on Jags tomorrow


# Maths -------------------------------------------------------------------

# Description of the Bayesian model fitted in this file
# Notation:
# y(t) = response variable at time t, t = 1,...,T
# alpha = overall mean parameter
# beta = autocorrelation/autoregressive (AR) parameter
# beta_j = Some of the models below have multiple AR parameters, j = 1,..P
# sigma = residual standard deviation

# Likelihood
# For AR(1)
# y[t] ~ normal(alpha + beta * y[t-1], sigma^2) # See AR/ARMA/MA etc. course notes
# For AR(p)
# y[t] ~ normal(alpha + beta[1] * y[t-1] + ... + beta[p] * y[y-p], sigma^2)

# Priors
# alpha ~ dnorm(0,100)
# beta ~ dunif(-1,1) # If you want the process to be stable/stationary
# beta ~ dnorm(0,100) # If you're not fussed about stability
# sigma ~ dunif(0,100)

###############################
# From course slides
####################################


# 1. Write some Stan or JAGS code which contains the likelihood and get the prior(s)
# 2. Get your data into a list so that it matches the data names used in the Stan/JAGS code
# 3. Run your model through Stan/JAGS
# 4. Get the posterior output
# 5. Check convergence of the posterior probability distribution
# 6. Create the output that you want (forecasts, etc)

# Simulate data -----------------------------------------------------------

# Some R code to simulate data from the above model
# First an AR(1)

set.seed(123)
T = 100
t_seq = 1:T
sigma = 1
alpha = 1    
beta = 0.6    # Constrain beta to (-1,1) so the series doesn't explode
y = rep(NA,T)
y[1] = rnorm(1,0,sigma)
for(t in 2:T) y[t] = rnorm(1, alpha + beta * y[t-1], sigma)
# plot
plot(t_seq, y, type='l')

# Jags code ---------------------------------------------------------------

# Jags code to fit the model to the simulated data

model_code = '
model
{
  # Likelihood
  for (t in (p+1):T) {
    y[t] ~ dnorm(mu[t], sigma^-2)
    mu[t] <- alpha + inprod(beta, y[(t-p):(t-1)])
  }
  # Priors
  alpha ~ dnorm(0, 10^-2)
  for (i in 1:p) {
    beta[i] ~ dunif(-1, 1)
  }
  sigma ~ dunif(0, 10)
}
'

# Set up the data
model_data = list(T = T, y = y, p = 1)

# Choose the parameters to watch
model_parameters =  c("alpha","beta","sigma")

# Run the model
model_run = jags(data = model_data,
                 parameters.to.save = model_parameters,
                 model.file = textConnection(model_code),
                 n.chains = 4, # Number of different starting positions
                 n.iter = 1000, # Number of iterations
                 n.burnin = 200, # Number of iterations to remove at start
                 n.thin = 2) # Amount of thinning

# Check the output - are the true values inside the 95% CI?
# Also look at the R-hat values - they need to be close to 1 if convergence has been achieved
print(model_run)
plot(model_run)

post = model_run$BUGSoutput$sims.matrix
head(post)
cor(post[,'alpha'],post[,'beta'])
plot(post[,'alpha'], type="l")

#---------------------- Moving Average example 

# Description of the Bayesian model fitted in this file
# Notation:
# theta = MA parameters
# q = order of the moving average (fixed)
# Likelihood for an MA(q) model:
# y_t ~ N(alpha + theta_1 ept_{t-1} + ... + theta_q eps_{t-q}, sigma)
# Prior
# alpha ~ normal(0, 100) # Vague
# sigma ~ uniform(0, 10)
# theta[q] ~ normal(0, 100)

# Simulate data -----------------------------------------------------------

# Some R code to simulate data from the above model
q = 1 # Order
T = 100
sigma = 1
alpha = 0
set.seed(123)
theta = runif(q)
y = rep(NA,T)
y[1:q] = rnorm(q, 0, sigma)
eps = rep(NA, T)
eps[1:q] = y[1:q] - alpha
for(t in (q+1):T) {
  y[t] = rnorm(1, mean = alpha + sum(theta * eps[(t-q):(t-1)]), sd = sigma)
  eps[t] = y[t] - alpha - sum(theta * eps[(t-q):(t-1)])
}
plot(1:T, y, type = 'l')

# Jags code ---------------------------------------------------------------

# This code to fit a general MA(q) model
model_code = '
model
{
  # Set up residuals
  for(t in 1:q) {
    eps[t] <- y[t] - alpha
  }
  # Likelihood
  for (t in (q+1):T) {
    y[t] ~ dnorm(mean[t], sigma^-2)
    mean[t] <- alpha + inprod(theta, eps[(t-q):(t-1)])
    eps[t] <- y[t] - alpha - inprod(theta, eps[(t-q):(t-1)])
  }
  # Priors
  alpha ~ dnorm(0, 10^-2)
  for (i in 1:q) {
    theta[i] ~ dunif(-1, 1)
  }
  sigma ~ dunif(0 ,10)
}
'

# Set up the data
model_data = list(T = T, y = y, q = 1)

# Choose the parameters to watch
model_parameters =  c("alpha", "theta", "sigma")

# Run the model
model_run = jags(data = model_data,
                 parameters.to.save = model_parameters,
                 model.file = textConnection(model_code),
                 n.chains = 4, # Number of different starting positions
                 n.iter = 1000, # Number of iterations
                 n.burnin = 200, # Number of iterations to remove at start
                 n.thin = 2) # Amount of thinning

print(model_run) # Parameter theta should match the true value

# could you expand this code for an AR(2) and an MA(2)?