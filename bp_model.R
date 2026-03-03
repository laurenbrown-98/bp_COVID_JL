# Authors: Jeremy Bingham, Lauren Brown
# Title: Branching process model for COVID-19
# Date: 15 February 2022 | Updated 03 March 2026

# install.packages("remotes")
# remotes::install_github("epiverse-trace/epichains")

suppressPackageStartupMessages({
  library(epichains)
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
})

## Import raw data
dd_firstcases_raw <- read.csv('https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_confirmed.csv', header = TRUE)

## Parameters

# Offspring distribution ~ how many new infections each case produces
# Serial interval ~ when those infections occur

proj_window = 14                         # project 14 days into the future
last_av_date = as.Date("2020-03-13")     # only use observed cases up to 13 March 2020
nsims = 10                              # number of simulations
m = 4.7; s = 2.9                         # mean & std of lognormal serial interval distribution 
offspring_mean = 2                       # mean of offspring distribution - on average each case causes 2 secondary cases
offspring_dispersion = 0.38              # dispersion parameter of offspring distribution - controls heterogeneity of transmission

## Import data
firstdate = '2020-03-05'
dd_firstcases <- (dd_firstcases_raw 
		  %>% select(case_id, date)
		  %>% mutate(date = dmy(date))
		  %>% mutate(ts = yday(date) - yday(firstdate) + 1) # convert dates into simple day index relative to firstdate
		  %>% filter(date <= last_av_date)
)

## View the raw data

dd_daily <- dd_firstcases %>%
  group_by(date) %>%
  summarise(cases = n(), .groups = "drop")

ggplot(dd_daily, aes(x = date, y = cases)) +
  geom_col(width = 0.8) +
  labs(
    title = "Early COVID-19 Cases",
    subtitle = "Daily confirmed cases (March 2020)",
    x = "Date",
    y = "Number of cases"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

## Generate the serial interval distribution
sigma2 <- log(1 + (s^2 / m^2))
sdlog <- sqrt(sigma2)
meanlog <- log(m) - sigma2/2

si <- function(n){
return(rlnorm(n, meanlog = meanlog, sdlog = sdlog))
}

dd_sims <- data.frame(time = c(), cases = c(), sim = c())

for(i in 1:nsims){

## Simulate a chain
tmp <- simulate_chains(
  n_chains       = nrow(dd_firstcases),  # one chain per observed seed case
  statistic      = "size",               # must be "size" or "length"
  offspring_dist = rnbinom,              # RNG function, not a string
  generation_time = si,                  # your serial interval function
  t0             = dd_firstcases$ts,     # start times for each seed case (vector OK)
  tf             = projection_finish,    # cut-off time
  mu             = offspring_mean,       # passed to rnbinom(...)
  size           = offspring_dispersion  # passed to rnbinom(...)
)

## Create a time series 

ts_tmp <- (tmp %>% mutate(time = floor(time))
	       %>% group_by(time) 
	       %>% summarise(cases = n())
	       %>% mutate(sim = i) 
)

dd_sims = rbind(dd_sims, ts_tmp)

}

# making median time series

dd_out <- dd_sims %>%
  group_by(time) %>%
  summarise(
    cases_med = median(cases),
    cases_lo  = quantile(cases, 0.025),
    cases_hi  = quantile(cases, 0.975),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date("2020-03-05") + time - 1)

print(dd_out)
write.csv(dd_out, file = "ts_sim.csv")


chains_plt <- ggplot(dd_out, aes(x = date)) +
  # Uncertainty ribbon
  geom_ribbon(aes(ymin = cases_lo, ymax = cases_hi),
              alpha = 0.25) +
  # Median line
  geom_line(aes(y = cases_med), linewidth = 1) +
  # Optional median points
  geom_point(aes(y = cases_med), size = 2) +
  labs(
    x = "Date",
    y = "Daily cases",
    title = "Branching process forecast",
    subtitle = "Median and 95% simulation interval"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(plot = chains_plt, filename = "bp_forecast.jpeg", device = "jpeg")
