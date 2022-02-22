# Authors: Jeremy, Lauren 
# Title: Branching process model for COVID-19
# Date: 15 February 2022 

library(bpmodels)
library(tidyverse)
library(lubridate)

# Parameters
mu_nb = 2
size_nb = 2
proj_window = 14
last_av_date = as.Date("2020-03-13")


## importing data

dd_firstcases_raw <- read.csv('https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_confirmed.csv', header = TRUE)
firstdate = '2020-03-05'
dd_firstcases <- (dd_firstcases_raw 
		  %>% select(case_id, date)
		  %>% mutate(date = dmy(date))
		  %>% mutate(ts = yday(date) - yday(firstdate) + 1)
		  %>% filter(date <= last_av_date)
)

#serial interval log-normally distributed with mean = 4.7, sd = 2.9
si <- function(n){
return(rlnorm(n, meanlog = 4.7, sdlog = 2.9))
}

# simulate a chain
tmp <- chain_sim(n = nrow(dd_firstcases), offspring = 'nbinom', mu = 2, size = 0.38, serial = si, tf = 27, t0 = dd_firstcases$ts)

print(tmp)
