# Reminder:
# 1. Write some Stan or JAGS code which contains the likelihood and get the prior(s)
# 2. Get your data into a list so that it matches the data names used in the Stan/JAGS code
# 3. Run your model through Stan/JAGS
# 4. Get the posterior output
# 5. Check convergence of the posterior probability distribution
# 6. Create the output that you want (forecasts, etc)

#####################################################################
# 1. Jags: AutoRegressive Integrated Moving Average (ARMA) models
#####################################################################

# ARIMA (AutoRegressive Integrated Moving Average) fit in JAGS

# Some code to clear the workspace and load in required packages
rm(list=ls()) # Clear the workspace
library(R2jags)

# Maths -------------------------------------------------------------------

# Description of the Bayesian model fitted in this file
# This is for a general ARIMA(p,d,q) model
# Notation:
# y(t) = response variable at time t, t=1,...,T
# alpha = mean parameter
# eps_t = residual at time t
# theta = MA parameters
# phi = AR parameters
# sigma = residual standard deviation
# d = number of first differences
# p and q = number of autoregressive and moving average components respecrively
# We do the differencing outside the model so let z[t] = diff(y, differnces = d)
# Likelihood:
# z[t] ~ N(alpha + phi[1] * z[t-1] + ... + phi[p] * z[y-p] + theta_1 ept_{t-1} + ... + theta_q eps_{t-q}, sigma^2)
# Priors
# alpha ~ N(0,100)
# phi ~ N(0,100) - need to be a bit careful with these if you want the process to remain stable
# theta ~ N(0,100)
# sigma ~ unif(0,10)

# Simulate data -----------------------------------------------------------

# Some R code to simulate data from the above model
p = 1 # Number of autoregressive terms
d = 0 # Number of differences
q = 1 # Numner of MA terms
T = 100
sigma = 1
alpha = 0
set.seed(123)
theta = runif(q)
phi = sort(runif(p),decreasing=TRUE)
y = rep(NA,T)
y[1:q] = rnorm(q,0,sigma)
eps = rep(NA,T)
eps[1:q] = y[1:q] - alpha
for(t in (q+1):T) {
  ar_mean = sum( phi * y[(t-1):(t-p)] )
  ma_mean = sum( theta * eps[(t-q):(t-1)] )
  y[t] = rnorm(1, mean = alpha + ar_mean + ma_mean, sd = sigma)
  eps[t] = y[t] - alpha - ma_mean - ar_mean
}
plot(1:T,y,type='l')

# Jags code ---------------------------------------------------------------

# Jags code to fit the model to the simulated data
model_code = '
model
{
  # Set up residuals
  for(t in 1:max(p,q)) {
    eps[t] <- z[t] - alpha
  }
  # Likelihood
  for (t in (max(p,q)+1):T) {
    z[t] ~ dnorm(alpha + ar_mean[t] + ma_mean[t], sigma^-2)
    ma_mean[t] <- inprod(theta, eps[(t-q):(t-1)])
    ar_mean[t] <- inprod(phi, z[(t-p):(t-1)])
    eps[t] <- z[t] - alpha - ar_mean[t] - ma_mean[t]
  }
  # Priors
  alpha ~ dnorm(0, 10^-2)
  for (i in 1:q) {
    theta[i] ~ dnorm(0, 10^-2)
  }
  for(i in 1:p) {
    phi[i] ~ dnorm(0, 10^-2)
  }
  sigma ~ dunif(0.0,10.0)
}
'

# Set up the data
model_data = list(T = T, z = y, q = 1, p = 1)

# Choose the parameters to watch
model_parameters =  c("alpha","theta","phi","sigma")

# Run the model
model_run = jags(data = model_data,
                 parameters.to.save = model_parameters,
                 model.file=textConnection(model_code),
                 n.chains=4, # Number of different starting positions
                 n.iter=1000, # Number of iterations
                 n.burnin=200, # Number of iterations to remove at start
                 n.thin=2) # Amount of thinning

# Simulated results -------------------------------------------------------

# Results and output of the simulated example,
# to include convergence checking, output plots, interpretation etc
print(model_run) # Parameters theta/phi/sigma should match the true value
plot(model_run)

# Extracting the posterior samples:
post = model_run$BUGSoutput$sims.matrix
head(post)

# Plotting the chains (traceplots):
post_alpha = post[,'alpha']
median_alpha = median(post_alpha)
plot(post_alpha, type="l")
abline(h=median_alpha, col="blue", lwd=2)
abline(h=alpha, col="red", lwd=2)

# Can repeat this for all variables of interest, e.g. sigma:
post_sigma = post[,'sigma']
median_sigma = median(post_sigma)
plot(post_sigma, type="l")
abline(h=median_sigma, col="blue", lwd=2)
abline(h=sigma, col="red", lwd=2)

# Can repeat this for all variables of interest, e.g. phi:
post_phi = post[,'phi']
median_phi = median(post_phi)
plot(post_phi, type="l")
abline(h=median_phi, col="blue", lwd=2)
abline(h=phi, col="red", lwd=2)

# Can repeat this for all variables of interest, e.g. theta:
post_theta = post[,'theta']
median_theta = median(post_theta)
plot(post_theta, type="l")
abline(h=median_theta, col="blue", lwd=2)
abline(h=theta, col="red", lwd=2)

# Real example ------------------------------------------------------------

# Data wrangling and jags code to run the model on a real data set in the data directory
hadcrut = read.csv('hadcrut.csv') #hadcrut = read.csv('https://raw.githubusercontent.com/andrewcparnell/tsme_course/master/data/hadcrut.csv')
head(hadcrut)
dim(hadcrut)

par(mfrow=c(1,2))
with(hadcrut,plot(Year,Anomaly,type='l'))
with(hadcrut,plot(Year[-1],diff(Anomaly),type='l'))
par(mfrow=c(1,1)) # Difference-stationary

# Save these differences:
hadcrut2 = diff(hadcrut$Anomaly)

# Look at the ACF/PACF
par(mfrow=c(1,2))
acf(hadcrut2)
pacf(hadcrut2)
par(mfrow=c(1,1))
# Try ARIMA(3,1,3)?

# Set up the data
d = 1
real_data = with(hadcrut,
                 list(T = nrow(hadcrut)-d,
                      z = diff(Anomaly, differences = d),
                      q = 3,
                      p = 3))

# Run the model
real_data_run = jags(data = real_data,
                     parameters.to.save = model_parameters,
                     model.file=textConnection(model_code),
                     n.chains=4,
                     n.iter=1000,
                     n.burnin=200,
                     n.thin=2)

# Plot output
print(real_data_run)
plot(real_data_run)

# Can also produce all plots as above (without the known values obviously)

# Plot some of the fitted values (also known as one-step-ahead predictions)
# (Or leave-none-out CV, as seen yesterday)
post = real_data_run$BUGSoutput$sims.list
alpha_mean = mean(post$alpha)
theta_mean = apply(post$theta,2,'mean')
phi_mean = apply(post$phi,2,'mean')

# Create fitted values
z = diff(hadcrut$Anomaly, differences = d)
eps_fit = z_fit = rep(NA,real_data$T) # Create holder
eps_fit[1:real_data$q] = z[1:real_data$q] - alpha_mean
z_fit[1:real_data$q] = alpha_mean
for (t in (real_data$q+1):real_data$T) {
  ar_mean = sum( phi_mean * z[(t-real_data$p):(t-1)] )
  ma_mean = sum( theta_mean * eps_fit[(t-real_data$q):(t-1)] )
  eps_fit[t] = z[t] - alpha_mean - ma_mean - ar_mean
  z_fit[t] = alpha_mean + ma_mean + ar_mean # No sigma term here - fitted value
}

# Create fitted lines - note that the z_fit values are one step ahead
# predicitons so they need to be added on
with(hadcrut, plot(Year, Anomaly, type='l'))
with(hadcrut, lines(Year, Anomaly+c(0,z_fit), col='blue'))

# Not a bad fit!

# Create some predictions off into the future - this time do it within jags
# A neat trick - just increase T and add on NAs into y!
T_future = 20 # Number of future data points
real_data_future = with(hadcrut,
                        list(T = nrow(hadcrut) + T_future - d,
                             z = c(diff(hadcrut$Anomaly,
                                        differences = d),
                                   rep(NA,T_future)),
                             q = 3,
                             p = 3))

# Just watch y now
model_parameters =  c("z")

# Run the model
real_data_run_future = jags(data = real_data_future,
                            parameters.to.save = model_parameters,
                            model.file=textConnection(model_code),
                            n.chains=4,
                            n.iter=1000,
                            n.burnin=200,
                            n.thin=2)

# Print out the above
print(real_data_run_future)

# Get the future values
z_all = real_data_run_future$BUGSoutput$sims.list$z
# If you look at the above object you'll see that the first columns are all
# identical because they're the data
z_all_mean = apply(z_all,2,'mean')
y_all_mean = cumsum(c(hadcrut$Anomaly[1],z_all_mean))
year_all = c(hadcrut$Year,(max(hadcrut$Year)+1):(max(hadcrut$Year)+T_future))

# Plot these all together
plot(year_all,
     y_all_mean,
     type='n')
lines(year_all,y_all_mean,col='red')
with(hadcrut,lines(Year,Anomaly))

#####################################################################
# 2. Jags: AutoRegressive Conditional Heteroskesticity (ARCH) models
#####################################################################

# An ARCH model is just like an AR model but with the AR component applied to the variance instead.
# This script just contains an ARCH(1) model

# Some code to clear the workspace, and load in required packages
rm(list=ls()) # Clear the workspace
library(R2jags)

# Maths -------------------------------------------------------------------

# Description of the Bayesian model fitted in this file
# Notation:
# y_t = response variable at time t=1,...,T
# alpha = overall mean
# sigma_t = residual standard deviation at time t
# gamma_0 = mean of variance term
# gamma_1 = AR component of variance
# Likelihood - two versions:
# y_t = alpha + epsilon_t
# epsilon_t ~ N(0, sigma_t^2)
# sigma_t^2 = gamma_0 + gamma_1 * epsilon_{t-1}^2
# or equivalently
# y_t ~ N(alpha, sigma_t^2)
# sigma_t^2 = gamma_0 + gamma_1 * (y_{t-1} - mu)^2
# Note that this works because epsilon_{t-1} = y_{t-1} - alpha in the first equation

# Priors
# gamma_0 ~ unif(0,10) - needs to be positive
# gamma_1 ~ unif(0,1) - ditto, and usually <1 too
# alpha ~ N(0,100) - vague

# Simulate data -----------------------------------------------------------

# Some R code to simulate data from the above model
T = 100
alpha = 1
gamma_0 = 1
gamma_1 = 0.4
sigma = y = rep(NA,length=T)
set.seed(123)
sigma[1] = runif(1)
y[1] = 0
for(t in 2:T) {
  sigma[t] = sqrt(gamma_0 + gamma_1 * (y[t-1] - alpha)^2)
  y[t] = rnorm(1, mean = alpha, sd = sigma[t])
}
plot(1:T,y,type='l')

# Jags code ---------------------------------------------------------------

# Jags code to fit the model to the simulated data
model_code = '
model
{
  # Likelihood
  for (t in 1:T) {
    y[t] ~ dnorm(alpha, sigma[t]^-2)
    tau[t] <- 1/pow(sigma[t], 2)
  }
  sigma[1] ~ dunif(0, 10)
  for(t in 2:T) {
  sigma[t] <- sqrt( gamma_0 + gamma_1 * pow(y[t-1] - alpha, 2) )
  }
  # Priors
  alpha ~ dnorm(0.0, 0.01)
  gamma_0 ~ dunif(0, 10)
  gamma_1 ~ dunif(0, 1)
}
'

# Set up the data
model_data = list(T = T, y = y)

# Choose the parameters to watch
model_parameters =  c("sigma", "gamma_0","gamma_1","alpha")

# Run the model
model_run = jags(data = model_data,
                 parameters.to.save = model_parameters,
                 model.file=textConnection(model_code),
                 n.chains=4, # Number of different starting positions
                 n.iter=1000, # Number of iterations
                 n.burnin=200, # Number of iterations to remove at start
                 n.thin=2) # Amount of thinning

# Simulated results -------------------------------------------------------

# Results and output of the simulated example, to include convergence checking, output plots, interpretation etc
print(model_run)
plot(model_run)

# Extracting the posterior samples:
post = model_run$BUGSoutput$sims.matrix
head(post)

# Plotting the chains (traceplots):
post_alpha = post[,'alpha']
median_alpha = median(post_alpha)
plot(post_alpha, type="l")
abline(h=median_alpha, col="blue", lwd=2)
abline(h=alpha, col="red", lwd=2)

# Plotting the chains (traceplots):
post_gamma0 = post[,'gamma_0']
median_gamma0 = median(post_gamma0)
plot(post_gamma0, type="l")
abline(h=median_gamma0, col="blue", lwd=2)
abline(h=gamma_0, col="red", lwd=2)

# Plotting the chains (traceplots):
post_gamma1 = post[,'gamma_1']
median_gamma1 = median(post_gamma1)
plot(post_gamma1, type="l")
abline(h=median_gamma1, col="blue", lwd=2)
abline(h=gamma_1, col="red", lwd=2)

# Real example ------------------------------------------------------------

# Run the ARCH(1) model on the ice core data set
ice = read.csv('GISP2_20yr.csv') #ice = read.csv('https://raw.githubusercontent.com/andrewcparnell/tsme_course/master/data/GISP2_20yr.csv')
head(ice)
dim(ice)

with(ice, plot(Age, Del.18O,type='l'))

# Try plots of differences
with(ice, plot(Age[-1],diff(Del.18O,differences=1), type='l')) # Still has heteroskedasticity
with(ice, plot(Age[-(1:2)],diff(Del.18O,differences=2), type='l'))

# Look at the last 30k years:
ice2 = subset(ice,Age>=10000 & Age<=25000)
table(diff(ice2$Age))
with(ice2,plot(Age[-1],diff(Del.18O),type='l'))

# Set up the data
real_data = with(ice2,
                 list(T = nrow(ice2) - 1, y = diff(Del.18O)))

# Save the sigma's - the most interesting part!
model_parameters = c('sigma','alpha','gamma_0','gamma_1')

# Run the model - requires longer to converge
real_data_run = jags(data = real_data,
                     parameters.to.save = model_parameters,
                     model.file=textConnection(model_code),
                     n.chains=4,
                     n.iter=1000,
                     n.burnin=200,
                     n.thin=2)

print(real_data_run)

# Have a look at the ARCH parameters;
par(mfrow=c(1,2))
hist(real_data_run$BUGSoutput$sims.list$gamma_0, breaks=30)
hist(real_data_run$BUGSoutput$sims.list$gamma_1, breaks=30) # Definitely looks different from 0
par(mfrow=c(1,1))

# Plot the sigma outputs
dim(real_data_run$BUGSoutput$sims.list$sigma) # 1600 accepted values for each time period (747 of these)
sigma_med = apply(real_data_run$BUGSoutput$sims.list$sigma,2,'quantile',0.5)
sigma_low = apply(real_data_run$BUGSoutput$sims.list$sigma,2,'quantile',0.025)
sigma_high = apply(real_data_run$BUGSoutput$sims.list$sigma,2,'quantile',0.975)

plot(ice2$Age[-1],sigma_med,type='l',ylim=range(c(sigma_low[-1],sigma_high[-1])))
lines(ice2$Age[-1],sigma_low,lty='dotted')
lines(ice2$Age[-1],sigma_high,lty='dotted')
# Some periods of high heteroskesdasticity

# Things to try at home: -------------------------------------------------------------

# 1) Try playing with the values of gamma_0 and gamma_1 in the simulated data above.
# See if you can create some really crazy patterns (e.g. try gamma_1>1)
# 2) (non-statistical) Do the periods of high
# heteroskedasticity match periods of known climate variability?
# 3) (harder) The above model is only an ARCH(1) model.
# See if you can simulate from and then fit an ARCH(2) version.

#####################################################################
#####################################################################
# Stan resources:
# An overview: http://mlss2014.hiit.fi/mlss_files/2-stan.pdf
# A very good tutorial: https://cran.r-project.org/web/packages/rstan/vignettes/rstan.html
# A comprehensive manual (most recent at the first link on this page):
# http://mc-stan.org/users/documentation/
# Overview of Stan...built to overcome deficiencies in Jags
# Python, command line, Rstan... MatlabStan, STATAStan, MathematicaStan (not supported)
# 624-page document - look at 22, 23... lots of examples through the manual
#####################################################################
#####################################################################

#####################################################################
# 3. Stan: AutoRegressive Integrated Moving Average (ARMA) models
#####################################################################

# ARIMA (AutoRegressive Integrated Moving Average) fit in Stan

# Some code to clear the workspace and load in required packages

rm(list=ls()) # Clear the workspace
library(rstan)

# Maths -------------------------------------------------------------------

# Description of the Bayesian model fitted in this file
# This is for a general ARIMA(p,d,q) model
# Notation:
# y(t) = response variable at time t, t=1,...,T
# alpha = mean parameter
# eps_t = residual at time t
# theta = MA parameters
# phi = AR parameters
# sigma = residual standard deviation
# d = number of first differences
# p and q = number of autoregressive and moving average components respecrively
# We do the differencing outside the model so let z[t] = diff(y, differnces = d)
# Likelihood:
# z[t] ~ N(alpha + phi[1] * z[t-1] + ... + phi[p] * z[y-p] + theta_1 ept_{t-1} + ... + theta_q eps_{t-q}, sigma^2)
# Priors
# alpha ~ N(0,10)
# phi ~ N(0,10) - need to be a bit careful with these if you want the process to remain stable
# theta ~ N(0,10)
# sigma ~ cauchy(0,5)

# Simulate data -----------------------------------------------------------

# Some R code to simulate data from the above model
p = 1 # Number of autoregressive terms
d = 0 # Number of differences
q = 1 # Numner of MA terms
T = 100
sigma = 1
alpha = 0
set.seed(123)
theta = runif(q)
phi = sort(runif(p),decreasing=TRUE)
y = rep(NA,T)
y[1:q] = rnorm(q,0,sigma)
eps = rep(NA,T)
eps[1:q] = y[1:q] - alpha
for(t in (q+1):T) {
  ar_mean = sum( phi * y[(t-1):(t-p)] )
  ma_mean = sum( theta * eps[(t-q):(t-1)] )
  y[t] = rnorm(1, mean = alpha + ar_mean + ma_mean, sd = sigma)
  eps[t] = y[t] - alpha - ma_mean - ar_mean
}
plot(1:T,y,type='l')

# Stan code ---------------------------------------------------------------

stan_code = '
data {
  int<lower=1> T; // num observations
  real y[T]; // observed outputs
}
parameters {
  real alpha; // mean coeff
  real phi; // autoregression coeff
  real theta; // moving avg coeff
  real<lower=0> sigma; // noise scale
}
model {
  vector[T] nu; // prediction for time t
  vector[T] err; // error for time t
  nu[1] = alpha + phi * alpha; // assume err[0] == 0
  err[1] = y[1] - nu[1];
  for (t in 2:T) {
  nu[t] = alpha + phi * y[t-1] + theta * err[t-1];
  err[t] = y[t] - nu[t];
}
  alpha ~ normal(0, 10); // priors
  phi ~ normal(0, 10);
  theta ~ normal(0, 10);
  sigma ~ cauchy(0, 5); // Happier with this than the uniform
  //err ~ normal(0, sigma); // likelihood
  y ~ normal(nu, sigma); // likelihood
}
'

# Set up the data ------------------------------
model_data <- list(T = T, y = y)

# Fitting the model: ------------------------------
model_fit = stan(
  data = model_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  refresh = 1000,
  model_code = stan_code # show progress every 'refresh' iterations
)

# Summary of model output:
print(model_fit,
      pars=c("alpha", "phi", "theta", "sigma", "lp__"), # The parameters we want to see
      probs=c(.1,.5,.9), # The credible interval wanted
      digits=2) # Number of decimal places to print

# Note the r-hat values to indicate convergence

# Plots of posterior densities:
plot(model_fit, pars=c("alpha", "phi", "theta", "sigma", "lp__")) # The parameters we want to see

# Remove lp__:
plot(model_fit, pars=c("alpha", "phi", "theta", "sigma")) # The parameters we want to see

# Traceplots to see the full chain:
rstan::traceplot(model_fit, pars = "alpha", inc_warmup = TRUE)
rstan::traceplot(model_fit, pars = "phi", inc_warmup = TRUE)
rstan::traceplot(model_fit, pars = "theta", inc_warmup = TRUE)
rstan::traceplot(model_fit, pars = "sigma", inc_warmup = TRUE)

# Pairs function:
# The “pairs”" plot can be used to get a sense of whether any sampling
# difficulties are occurring in the tails or near the mode:
pairs(model_fit, pars = c("alpha", "phi", "theta", "sigma"), las = 1)
# Note the marginal distribution along the diagonal

# Each off-diagonal square represents a bivariate distribution of the draws
# for the intersection of the row-variable and the column-variable.

# Things to try at home: -------------------------------------------------------------

# Apply this model to the HADcrut data (as already done with Jags)
# Compare the Jags and Stan output

--------------------------------------------------------------------------------------
  
  #####################################################################
# 4. Jags: AutoRegressive Conditional Heteroskesticity (ARCH) models
#####################################################################

# An ARCH model is just like an AR model but with the AR component applied to the variance instead.
# This script just contains an ARCH(1) model

# Some code to clear the workspace, and load in required packages
rm(list=ls()) # Clear the workspace
library(rstan)

# Maths -------------------------------------------------------------------

# Description of the Bayesian model fitted in this file
# Notation:
# y_t = response variable at time t=1,...,T
# alpha = overall mean
# sigma_t = residual standard deviation at time t
# gamma_0 = mean of variance term
# gamma_1 = AR component of variance
# Likelihood - two versions:
# y_t = alpha + epsilon_t
# epsilon_t ~ N(0, sigma_t^2)
# sigma_t^2 = gamma_0 + gamma_1 * epsilon_{t-1}^2
# or equivalently
# y_t ~ N(alpha, sigma_t^2)
# sigma_t^2 = gamma_0 + gamma_1 * (y_{t-1} - alpha)^2
# Note that this works because epsilon_{t-1} = y_{t-1} - alpha in the first equation

# Priors
# gamma_0 ~ unif(0,10) - needs to be positive
# gamma_1 ~ unif(0,1) - ditto, and usually <1 too
# alpha - a real number - vague

# Simulate data -----------------------------------------------------------

# Some R code to simulate data from the above model
T = 100
alpha = 1
gamma_0 = 1
gamma_1 = 0.4
sigma = y = rep(NA,length=T)
set.seed(123)
sigma[1] = runif(1)
y[1] = 0
for(t in 2:T) {
  sigma[t] = sqrt(gamma_0 + gamma_1 * (y[t-1] - alpha)^2)
  y[t] = rnorm(1, mean = alpha, sd = sigma[t])
}
plot(1:T,y,type='l')

# Stan code ---------------------------------------------------------------

# Stan code to fit the model to the simulated data:
stan_code = '
data {
int<lower=0> T; // number of time points
real y[T]; // return at time t
}
parameters {
real alpha; // average return
real<lower=0, upper=10> gamma_0; // noise intercept
real<lower=0, upper=1> gamma_1; // noise slope
}
model {
for (t in 2:T)
y[t] ~ normal(alpha, sqrt(gamma_0 + gamma_1 * pow(y[t-1] - alpha,2)));
}
'

# Set up the data ------------------------------
model_data <- list(T = T, y = y)

# Fitting the model: ------------------------------
model_fit = stan(
  data = model_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  refresh = 1000,
  model_code = stan_code # show progress every 'refresh' iterations
)

# Summary of model output:
print(model_fit,
      pars=c("alpha", "gamma_0", "gamma_1", "lp__"), # The parameters we want to see
      probs=c(.1,.5,.9), # The credible interval wanted
      digits=2) # Number of decimal places to print

# Note the r-hat values to indicate convergence

# Plots of posterior densities:
plot(model_fit, pars=c("alpha", "gamma_0", "gamma_1", "lp__")) # The parameters we want to see

# Remove lp__:
plot(model_fit, pars=c("alpha", "gamma_0", "gamma_1")) # The parameters we want to see

# Traceplots to see the full chain:
rstan::traceplot(model_fit, pars = "alpha", inc_warmup = TRUE)
rstan::traceplot(model_fit, pars = "gamma_0", inc_warmup = TRUE)
rstan::traceplot(model_fit, pars = "gamma_1", inc_warmup = TRUE)

# Pairs function:
# The “pairs”" plot can be used to get a sense of whether any sampling
# difficulties are occurring in the tails or near the mode:
pairs(model_fit, pars = c("alpha", "gamma_0", "gamma_1"), las = 1)
# Note the marginal distribution along the diagonal

# Each off-diagonal square represents a bivariate distribution of the draws
# for the intersection of the row-variable and the column-variable.

# Real example ------------------------------------------------------------

# Run the ARCH(1) model on the ice core data set
ice = read.csv('GISP2_20yr.csv') #ice = read.csv('https://raw.githubusercontent.com/andrewcparnell/tsme_course/master/data/GISP2_20yr.csv')
head(ice)
dim(ice)

with(ice, plot(Age, Del.18O,type='l'))

# Try plots of differences
with(ice, plot(Age[-1],diff(Del.18O,differences=1), type='l'))
with(ice, plot(Age[-(1:2)],diff(Del.18O,differences=2), type='l'))

# Try this on the last 30k years
ice2 = subset(ice,Age>=10000 & Age<=25000)
table(diff(ice2$Age))
with(ice2,plot(Age[-1],diff(Del.18O),type='l'))

# Set up the data
real_data = with(ice2,
                 list(T = nrow(ice2) - 1, y = diff(Del.18O)))

# Run the model - requires longer to converge
# Fitting the model: ------------------------------
real_fit = stan(
  data = real_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  refresh = 1000,
  model_code = stan_code # show progress every 'refresh' iterations
)

print(model_fit)

# Have a look at the ARCH parameters;
# Plots of posterior densities:
plot(model_fit, pars=c("alpha", "gamma_0", "gamma_1", "lp__")) # The parameters we want to see

# Remove lp__:
plot(model_fit, pars=c("alpha", "gamma_0", "gamma_1")) # The parameters we want to see

# Traceplots to see the full chain:
rstan::traceplot(model_fit, pars = "alpha", inc_warmup = TRUE)
rstan::traceplot(model_fit, pars = "gamma_0", inc_warmup = TRUE)
rstan::traceplot(model_fit, pars = "gamma_1", inc_warmup = TRUE)

### Plot the sigma outputs using posterior samples:
burnin = 1000
total = length(model_fit@sim$samples[[1]]$gamma_0)

sample_gamma_0 = model_fit@sim$samples[[1]]$gamma_0[(burnin+1):total] # 1000 samples (should probably thin)
sample_gamma_1 = model_fit@sim$samples[[1]]$gamma_1[(burnin+1):total]
sample_alpha = model_fit@sim$samples[[1]]$alpha[(burnin+1):total]
y = diff(ice2$Del.18O)

# Getting estimates of sigma from the second value onwards:
sigma = matrix(nrow=length(y), ncol=total-burnin)
sigma[1,] = 1

for (i in 2:length(y)) {
  # Calculate estimates of sigma, for each value of t:
  sigma[i,] = sqrt(sample_gamma_0 + sample_gamma_1 * (y[i-1] - sample_alpha)^2)
  
}

# Get the quantiles (specific to each value of t)
sigma_med = apply(sigma, 1, 'quantile',0.5)
sigma_low = apply(sigma, 1,'quantile',0.025)
sigma_high = apply(sigma, 1,'quantile',0.975)

# Plot the median, low and high:
plot(ice2$Age[-1], sigma_med,type='l',ylim=range(c(sigma_low[-1],sigma_high[-1])))
lines(ice2$Age[-1], sigma_low,lty='dotted')
lines(ice2$Age[-1], sigma_high,lty='dotted')
# Some periods of high heteroskesdasticity
