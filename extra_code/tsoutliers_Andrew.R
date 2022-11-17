# See this blog post too: https://robjhyndman.com/hyndsight/tsoutliers/
# Usage
# plot(tsclean_andrew(CO2$CO2_ppm, my_strength = 0.9, my_iqr = 1), type = 'l')

tsoutliers_andrew <- function (x, iterate = 2, lambda = NULL,
                               my_strength = 0.6, my_iqr = 3) 
{
  n <- length(x)
  freq <- frequency(x)
  missng <- is.na(x)
  nmiss <- sum(missng)
  if (nmiss > 0L) {
    xx <- na.interp(x, lambda = lambda)
  }
  else {
    xx <- x
  }
  if (is.constant(xx)) {
    return(list(index = integer(0), replacements = numeric(0)))
  }
  if (!is.null(lambda)) {
    xx <- BoxCox(xx, lambda = lambda)
    lambda <- attr(xx, "lambda")
  }
  if (freq > 1 && n > 2 * freq) {
    fit <- mstl(xx, robust = TRUE)
    rem <- remainder(fit)
    detrend <- xx - trendcycle(fit)
    strength <- 1 - var(rem)/var(detrend)
    if (strength >= my_strength) {
      xx <- seasadj(fit)
    }
  }
  tt <- 1:n
  mod <- supsmu(tt, xx)
  resid <- xx - mod$y
  if (nmiss > 0L) {
    resid[missng] <- NA
  }
  resid.q <- quantile(resid, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(resid.q)
  limits <- resid.q + my_iqr * iqr * c(-1, 1)
  if ((limits[2] - limits[1]) > 1e-14) {
    outliers <- which((resid < limits[1]) | (resid > limits[2]))
  }
  else {
    outliers <- numeric(0)
  }
  x[outliers] <- NA
  x <- na.interp(x, lambda = lambda)
  if (iterate > 1) {
    tmp <- tsoutliers(x, iterate = 1, lambda = lambda)
    if (length(tmp$index) > 0) {
      outliers <- sort(unique(c(outliers, tmp$index)))
      x[outliers] <- NA
      if (sum(!is.na(x)) == 1L) {
        x[is.na(x)] <- x[!is.na(x)]
      }
      else x <- na.interp(x, lambda = lambda)
    }
  }
  return(list(index = outliers, replacements = x[outliers]))
}

tsclean_andrew <- function (x, replace.missing = TRUE, iterate = 2, lambda = NULL,
                            my_strength = 0.6, my_iqr = 3) 
{
  outliers <- tsoutliers_andrew(x, iterate = iterate, lambda = lambda,
                                my_strength = my_strength, my_iqr = my_iqr)
  x[outliers$index] <- outliers$replacements
  if (replace.missing) {
    x <- na.interp(x, lambda = lambda)
  }
  return(x)
}
