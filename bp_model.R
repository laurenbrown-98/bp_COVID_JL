# Authors: Jeremy, Lauren 
# Title: Branching process model for COVID-19
# Date: 15 February 2022 

library(bpmodels)
library(tidyverse)
library(lubridate)
library(ggplot2)

## parameters
proj_window = 14
last_av_date = as.Date("2020-03-13")
nsims = 5
meanlog_si = log(4.7)
sdlog_si = log(2.9)
offspring_mean = 2
offspring_dispersion = 0.38


## import data

dd_firstcases_raw <- read.csv('https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_confirmed.csv', header = TRUE)
firstdate = '2020-03-05'
dd_firstcases <- (dd_firstcases_raw 
		  %>% select(case_id, date)
		  %>% mutate(date = dmy(date))
		  %>% mutate(ts = yday(date) - yday(firstdate) + 1)
		  %>% filter(date <= last_av_date)
)

projection_start = max(dd_firstcases$ts)
projection_finish = projection_start + proj_window

## serial interval log-normally distributed with mean = 4.7, sd = 2.9
si <- function(n){
return(rlnorm(n, meanlog = meanlog_si, sdlog = sdlog_si))
}

dd_sims <- data.frame(time = c(), cases = c(), sim = c())

for(i in 1:nsims){

## simulate a chain
tmp <- chain_sim(n = nrow(dd_firstcases), offspring = 'nbinom', mu = offspring_mean, size = offspring_dispersion, serial = si, tf = projection_finish, t0 = dd_firstcases$ts)

## print(tmp)

## create a time series 

ts_tmp <- (tmp %>% mutate(time = floor(time))
	       %>% group_by(time) 
	       %>% summarise(cases = n())
	       %>% mutate(sim = i) 
)

dd_sims = rbind(dd_sims, ts_tmp)

}

# making median time series

dd_out <-(dd_sims 
	  %>% group_by(time)
	  %>% summarise(cases_med = median(cases))
	  %>% mutate(date = as.Date('2020-03-05') + time - 1 )
)	  

print(dd_out)
write.csv(dd_out, file = "ts_sim.csv")

# making plot
plot1 <- ggplot(data = dd_out, aes(x = date, y = cases_med)) +
	geom_point()
ggsave(plot = plot1,
       filename = "bp_forecast.jpeg",
	device = "jpeg")


