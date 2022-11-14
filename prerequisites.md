---
title: "Pre-requisites for Time Series Data Analysis course"
author: "Andrew Parnell"
output: html_document
---

In preparation for the course please install the following, preferably in the below suggested order. Make sure you run these as soon as possible to avoid falling behind. Some of the more advanced later packages are not required until day 2 so if you run into problems you might still be fine for the first day. Remember you will need your own personal computer with administrator access for the duration of the course and a good internet connection.

As this module will be delivered online please install [Zoom](https://www.zoom.us) and [Slack](https://slack.com) to access the videos and interactive components of the course. All the Zoom links to the meeting will be posted to the Slack `#General` channel.

### Step 1

Install the following using the corresponding links

-	R: [http://www.r-project.org](http://www.r-project.org)

-	Rstudio (optional but recommended): [https://www.rstudio.com](https://www.rstudio.com)

- Install JAGS: [http://mcmc-jags.sourceforge.net](http://mcmc-jags.sourceforge.net)

### Step 2

Now install the following packages by going into Rstudio (or R) and typing:
```{r,eval=FALSE}
install.packages(c('R2jags', 
                   'rjags', 
                   'tidyverse', 
                   'forecast'))
```

Note: this might take a few minutes so be patient!

### Troubleshooting

If you run into any problems please drop me a line at <andrew.parnell@mu.ie>.

