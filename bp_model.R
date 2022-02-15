# Authors: Jeremy, Lauren 
# Title: Branching process model for COVID-19
# Date: 15 February 2022 

library(bpmodels)
library(tidyverse)

# Parameters

mu_nb = 2
size_nb = 2
proj_window = 14
last_av_date = as.Date("2020-03-13")

# dd <- data.frame(names = c(date, timestep)

input_data <- data.frame(names = c(date, cases, timestep))

start_times = c()
for(i=1:nrow(input_data)){
start_times = c(start_times, rep(input_data[i,"timestep"], input_data[i, "cases"]))
}

si <- function(n){
return(rlnorm(n, meanlog = 4.7, sdlog = 2.9))
}



#log-normally distributed with mean = 4.7, sd = 2.9




tmp <- chain_sim(n = length(start_times), offspring = 'nbinom', mu = 2, size = 0.38, serial = si, tf = 26, t0 = start_times)



print(tmp)
