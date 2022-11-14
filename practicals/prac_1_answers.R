# Answer script for self-guided practical 1 

## PLEASE DON'T READ THIS UNTIL YOU HAVE COMPLETED THE PRACTICAL QUESTIONS

# The tasks of this practical are to:
# Load in some data
# Create some plots
# Fits some models (linear regression, a binary logistic regression, a Poisson generalised linear model)


# Set-up ------------------------------------------------------------------

# Clear the workspace and set the working directory
rm(list = ls())
#setwd('path/to/your/files') # Change this to be the path to your data directory

# Load in any necessary packages
library(MASS)


# Data set 1: airquality --------------------------------------------------

# The airquality data set is included with R. You can get at it simply by typing airquality at the R command prompt

# Let's suppose we're interested in predicting the variable Ozone from the other variables

# 1. First create some plots of the variables and make sure you understand the relationship between them
# Hint: A good start can be found in the help(airquality) file (see the example at the bottom), though that plot isn't very helpful for visualisation of the month and day variables, nor for visualising the shape of individual variables (e.g. using hist)

pairs(airquality, panel = panel.smooth, main = "airquality data")

# Other plots:
boxplot(Ozone ~ Month, data = airquality)
boxplot(Ozone ~ Day, data = airquality)

# Histograms
hist(airquality$Ozone, breaks = 30) # Very skewed
hist(airquality$Solar.R, breaks = 30) # The rest look ok
hist(airquality$Wind, breaks = 30)
hist(airquality$Temp, breaks = 30)

# 2. Fit a linear regression model using lm with Ozone as the response and the other variables as covariates. Use the summary method to interpret your findings. (Note that R here is automatically removing rows with missing variables)
mod_1 = lm(Ozone ~ ., data = airquality)
summary(mod_1)
# Looks ok as a model. Only day seems unimportant. However, these are both included as linear covariates so not much sense in including them this way

# 3. Have a look at the residuals of the model (e.g. histograms and qq-plots). Does the model fit well? 
qqnorm(mod_1$residuals)
qqline(mod_1$residuals) # Really not good - very skewed
hist(mod_1$residuals, breaks = 30)

# 4. Try another model but this time using the log of Ozone instead. Does it fit better?
airquality$log_Ozone = log(airquality$Ozone)
mod_2 = lm(log_Ozone ~ Solar.R + Wind + Temp + Month + Day, data = airquality)
summary(mod_2) # Month no longer important. Slightly better model fit by R-sq

# Residuals
qqnorm(mod_2$residuals)
qqline(mod_2$residuals) # All good but one dodgy observation!
hist(mod_2$residuals, breaks = 30)

# 5. Identify the one strange observation and see if you can work out what happened that day
airquality[as.integer(names(which.min(mod_2$residuals))),]
# Compare with summary(airquality) - average Ozone value, but very high Solar.R, low temperature

# 6. You can get the AIC of this model with e.g. AIC(my_model). Try some more models and see if you can get a better fit. Some ideas might include: interactions between terms (e.g. include + Wind*Temp), quadratic functions (e.g. include + I(Wind^2)), and changing month and day to be factor rather than numerical variables. 
AIC(mod_2)

# I haven't done all the above but here's a model with month and day included as factors
airquality$Month_fac = as.factor(airquality$Month)
airquality$Day_fac = as.factor(airquality$Day)
mod_3 = lm(log_Ozone ~ Solar.R + Wind + Temp + Month_fac + Day_fac, data = airquality)
summary(mod_3) # Month seems almost completely unimportant, and day seems to be over-fitting
qqnorm(mod_3$residuals)
qqline(mod_3$residuals) # OK but still one odd obs
hist(mod_3$residuals, breaks = 30)
AIC(mod_3) # Much worse than mod_2


# Data set 2: Horseshoe ---------------------------------------------------

# 1. Load in the horseshoe.csv data set from the data directory and familiarise yourself with the data structure from the data_description.txt file

horseshoe = read.csv('horseshoe.csv')

# 2. Turn the color and spine variables into factors with their proper names
horseshoe$color = factor(horseshoe$color, labels = c('light medium', 'medium', 'dark medium', 'dark'))
horseshoe$spine = factor(horseshoe$spine, labels = c('both good', 'one worn or broken', 'both worn or broken'))

# 2. Familiarise yourself by plotting the data and exploring the structure between the variables
pairs(horseshoe, panel = panel.smooth, main = "horseshoe data")
plot(horseshoe$width, horseshoe$satell) # Looks like a slight increasing relaitonship with width
plot(horseshoe$weight, horseshoe$satell) # 2 strange observations
plot(horseshoe$weight, horseshoe$width) # A few odd obs here

# 3. Create a binary variable which contains only whether the satellite variable is >0 or not. We will use this as our response variable. Create a plot which shows the relationship of this variable with width.

horseshoe$satell_bin = as.integer(horseshoe$satell>0)
boxplot(horseshoe$width ~ horseshoe$satell_bin)

# 4. Fit a binomial glm (a logistic regression) with the binary variable as the repsonse and width as a covariate. Summarise your findings
mod_4 = glm(satell_bin ~ width, family = binomial(link = 'logit'), data = horseshoe)
summary(mod_4) # Highly significant fit AIC 198.45

# 5. Create plot of the fitted values on top of a scatter plot of the data (hint: width on x-axis, binary reponse variable on y-axis)
plot(horseshoe$width, horseshoe$satell_bin)
points(horseshoe$width, mod_4$fitted.values, col = 'red')

# 6. Try fitting some more models to the data with more variables (and perhaps interactions) to see if you can get the AIC lower. Compare your new models' fitted values to the first model

# Here's a model with width and weight and spine condition
mod_5 = glm(satell_bin ~ width + weight + spine, family = binomial(link = 'logit'), data = horseshoe)
summary(mod_5) # AIC a bit worse 203.97

# Data set 3: Horseshoe (again) -------------------------------------------

# 1. This time fit a Poisson GLM to the horseshoe data, using the original number of satellites rather than the binary version you fitted previously. Again use width as the sole covariate, and again plot the fitted values

mod_6 = glm(satell ~ width, family = poisson(link = 'log'), data = horseshoe)
summary(mod_6) # Width clearly important, AIC 827.18

plot(horseshoe$width, horseshoe$satell)
points(horseshoe$width, mod_6$fitted.values, col = 'red') # Non-linear increasing relationship with width

# 2. Now try a model with all of the covariates (make sure not to include the binary variable you created before). Summarise and see if there's any improvement. You might notice that more variables are important (compared to the previous binary logistic regression) because we're using more of the data
mod_7 = glm(satell ~ width + color + spine + weight, family = poisson(link = 'log'), data = horseshoe)
summary(mod_7) # Colour and weight seem to be important

# 3. A common occurrence is that the Poisson distribution is a poor fit to the data as the mean=variance relationship is rarely met. (You could check if the mean and the variance match for these data). A common alternative is to fit a Negative-Binomial GLM which has an extra parameter to measure excess variance (over-dispersion). The glm function doesn't have this distribution in itby default so you need to call in the MASS library with library(MASS). The family will now be called negative.binomial and you'll need to specify a value of the excess variance theta. The ratio of the variance to the mean of the response would be a good start. Fit a Negative Binomial GLM to these data, interpret your findings, see if the AIC improves, and plot your output.

mean(horseshoe$satell) # 2.9
var(horseshoe$satell) # 9.9 - much bigger
# Suggests using theta = 3

mod_8 = glm(satell ~ width, family = negative.binomial(theta = 3, link = 'log'), data = horseshoe)
summary(mod_8) # Huge decrease in AIC

points(horseshoe$width, mod_8$fitted.values, col = 'blue') # Slight change to the plot


# Extra questions ---------------------------------------------------------

# The other data sets which are worth fitting GLMs or linear models to in the data directory are:
# 1. The geese_isotopes data. You might like to see if one of the isotope values is affected by some of the other variables (sex, adult, etc)
# 2. The whitefly data set. This is binomial (as used in the lectures) but you might like to additionaly try some of the other variables and see which are important and why. The data set has particular issues with zero-inflation. See if you can predict which zero data points are poorly predicted by the model.

