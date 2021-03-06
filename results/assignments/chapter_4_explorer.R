library(ggplot2)
library(dplyr)
library(plotly)
runoff_day <- readRDS("./data/runoff_day.rds")
runoff_month <- readRDS("./data/runoff_month.rds")
runoff_summer <- readRDS("./data/runoff_summer.rds")
runoff_winter <- readRDS("./data/runoff_winter.rds")
runoff_year <- readRDS("./data/runoff_year.rds")

# q1
#there is no difference between the median and 0.5 quantile
#q2
runoff_day[,mean(value)]
runoff_day[,median(value)]
runoff_day[,median(value), by = sname]

#diffence in values due to positive skew, the data has a long tail, very large max values

#q3, 
#rees and lobith both have very large areas and are very low lying, they are very close together, due to the fact #a human border runs between them (germany/netherlands)


#----------q4------------------------------
# with interactive plots, easy to locate min/max points, as well as there dates and values
runoff_month <- runoff_month[value >= 0]
runoff_month[1:3654,3] <- "REES"
runoff_month <- runoff_month[sname != "<NA>"]
p <- runoff_month %>%
  ggplot( aes(x = date, y = value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("discharge m^3/s") +
  facet_wrap(~ sname, scales = 'free')
p <- ggplotly(p)
p
runoff_summer <- runoff_summer[value >= 0]  
q <- runoff_summer %>%
  ggplot( aes(x = year, y = value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("discharge m^3/s") +
  facet_wrap(~ sname, scales = 'free')
q <- ggplotly(q)
q
runoff_winter <- runoff_winter[value >= 0]  
r <- runoff_winter %>%
  ggplot( aes(x = year, y = value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("discharge m^3/s") +
  facet_wrap(~ sname, scales = 'free')
r <- ggplotly(r)
r
runoff_year <- runoff_year[value >= 0]  
s <- runoff_year %>%
  ggplot( aes(x = year, y = value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("discharge m^3/s") +
  facet_wrap(~ sname, scales = 'free')
s <- ggplotly(q)
s
# with interactive plots, easy to locate min/max points, as well as there dates and values
