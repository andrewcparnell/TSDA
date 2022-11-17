# Answer script for self-guided practical 2 

## PLEASE DON"T READ THIS UNTIL YOU HAVE COMPLETED THE PRACTICAL QUESTIONS

# The tasks of this practical are to:
# Analyse the airquality data as a time series model
# Create some time series plots
# Run some ARIMA, nnetar, tslm, and ets models on the data
# Produce some forecasts
# Compare the accuracy of the different methods

# 1. Create a new data frame called `airquality2` from the airquality data set already loaded into R. Use the `as.Date` function to convert Month and Day into a proper time variable called `Date`.
airquality2 = airquality
airquality2$Date = as.Date(paste(airquality$Day, 
                                airquality$Month,
                                1973,
                                sep="/")," %d/%m/%Y")

# 2. Create some time series plots of Date vs Ozone and interpret them. Do you think it looks stationary? 
with(airquality2, plot(Date, Ozone, type = 'l')) # Hard to tell - too many missing values!

# 3. Yesterday we found that airquality was a bit better behaved when we log-transformed it. Forecast has a smarter method for doing transformations using the Box-Cox transformation. Use the function `BoxCox.lambda` to estimate the lambda transformation parameter for the Ozone variable. Re-plot the data with the BoxCox transformed series and see if it looks more stationary.
lambda = BoxCox.lambda(airquality2$Ozone)

# Check the difference
with(airquality2, plot(Date, BoxCox(Ozone, lambda = lambda), type = 'l')) # Looks a bit more stationary

# 4. Hopefully you noticed that there are lots of missing values. If you try an ACF plot here using the standard `acf` function it will fail, but if you use the `forecast` functions `Acf` and `Pacf` it will work. Create and interpret the ACF and PACF plots. (You could also try running them on the transformed data using the function `BoxCox`)
Acf(airquality2$Ozone)
Pacf(airquality2$Ozone) # Sudden drop off at lag ~ 1

Acf(BoxCox(airquality2$Ozone, lambda = lambda))
Pacf(BoxCox(airquality2$Ozone, lambda = lambda)) # Drop off at lag 1

# 5. It looks like some kind of AR model might work for these data. Use `Arima` to fit an AR(1) model and interpret the output. Don't forget to include the lambda argument
mod_1 = Arima(airquality2$Ozone, order = c(1, 0, 0), lambda = lambda)
summary(mod_1) # AIC = 378.45 

# 6. Try an `auto.arima` model and interpret your output
mod_2 = auto.arima(airquality2$Ozone, lambda = lambda) # Chooses 2,1, 0
summary(mod_2) # AIC = 382.31 - slightly worse!
# Read the help file for auto.arima. It actually uses approximations to fit models quickly and therefore missed the AR(1) model we found above

# 7. Use the `forecast` function to plot 10 steps into the future using the model you just created
plot(forecast(mod_2, h = 10))

# 8. Check the residuals of your `auto.arima` model using `hist` and QQ-plots (hint: see answers from yesterday for a reminder)
hist(mod_2$residuals, breaks = 30)
qqnorm(mod_2$residuals)
qqline(mod_2$residuals)

# 9. Unfortunately with missing values, many of the other time series methods won't work. However, we can impute (i.e. replace) the missing values using the `na.interp` function. Create a new variable `Ozone2` in your `airquality2` data frame which has no missing values. Plot this new series

airquality2$Ozone2 = na.interp(airquality2$Ozone)
with(airquality2, plot(Date, Ozone2, type = 'l')) # A bit odd in places
 
# 10. Let's now use this complete data set to try others types of model. Run the `ets``, `nnetar``, and `tslm`` functions used in the earlier lectures and tutorials today to create some different models. See if you can find ones that beat the ARIMA versions. Try and interpret the model output

mod_3 = ets(airquality2$Ozone2, lambda = lambda)
summary(mod_3)
plot(forecast(mod_3))

mod_4 = nnetar(airquality2$Ozone2, lambda = lambda)
mod_4
plot(forecast(mod_4))

mod_5 = tslm(ts(airquality2$Ozone2) ~ trend, lambda = lambda, data = )
summary(mod_5)
plot(forecast(mod_5))

# Note mod_4 has no AIC as it's not really a statistical model
AIC(mod_1, mod_2, mod_3, mod_5) # Can't beat the ARIMA!

# Next steps --------------------------------------------------------------

# Try picking another data set from the pile (not a seasonal one yet as we don't cover it till tomorrow!) and see if you can follow our standard steps:
#1. Plot the data and the ACF/PACF
#2. Decide if the data look stationary or not. If not, perform a
#suitable transformation and return to 1. If the data has a
#strong trend or there is a high degree of autocorrelation
#try 1 or 2 differences
#3. Guess at a suitable p and q for an ARMA(p, q) model
#4. Fit the model
#5. Try a few models around it by increasing/decreasing p and q
#and checking the AIC (or others)
#6. Check the residuals
#7. Forecast into the future

# In step 3 you can also try some of the other standard models in `forecast`, such as `naive`, `ets`, `nnetar`, and `tslm`. 

# If you're feeling really brave see if you can get your own data set into a format suitable for ARIMA modelling. Even if it involves throwing away or approximating some data it will be a useful start 
