#q1
library(ggplot2)
list.files("data")
runoff_stats <- readRDS("./data/runoff_stats.rds")
runoff_month <- readRDS("./data/runoff_month.rds")
runoff_stations <- readRDS("./data/runoff_stations_raw.rds")
runoff_day <- readRDS("./data/runoff_day_raw.rds")
runoff_stats$median <- runoff_day[,median(value), by = sname][,2]
runoff_stats_tidy <- melt(runoff_stats, id.vars = 'sname')
ggplot(runoff_stats_tidy, aes(x = sname, y = value, shape = "cyl", color = "cyl", size = 0.5)) + 
  geom_point(aes(col = variable, shape = variable))

##----Question 2 ------
runoff_stats$coefficient_of_variation <- runoff_stats$sd_day/runoff_stats$mean_day
#mean is closer to minimum value, hence data is positively skewed,
runoff_stats$median <- runoff_day[,median(value), by = sname][,2]
runoff_stats$skew <- 3*(runoff_stats$mean_day-runoff_stats$median)/runoff_stats$sd_day
swek_and_coefficient_of_variation <- data.table(runoff_stats$skew, runoff_stats$coefficient_of_variation)
colnames(swek_and_coefficient_of_variation) <- c("skewness", "coefficient of variation")
#q3
labs <- c("high", "medium", "low")
levels(runoff_stats_class$class) <- rev(labs)
levels(runoff_stats_class$runoff_class) <- rev(labs)
ggplot(runoff_month, aes(x = factor(month), y = value,)) +
  geom_boxplot(fill = colset_4[4]) +
  facet_wrap(~ sname, scales = 'free',) + 
  theme_bw()

ggplot(runoff_day, aes(x = date, y = value, group = sname)) +
  geom_boxplot(fill = colset_4[4]) +
  facet_wrap(~ sname, scales = 'free') + 
  theme_bw()
#outliers, tend to be greater than 3rd quartile, no outliers below 1st quartile, this hapens due to the fact the mean is closer to zero than the max point, rainfall function is very erratic and some days can expriance very heavy rainfall
#----q4----
runoff_stations$mean_days <- runoff_stats_tidy$value[1:20]

runoff_stations[, area_class := factor('small')]
runoff_stations[area >= 10000 & area < 130000, area_class := factor('medium')]
runoff_stations[area >= 130000, area_class := factor('large')]

runoff_stations[, alt_class := factor('low')]
runoff_stations[altitude >= 50 & altitude < 400, alt_class := factor('medium')]
runoff_stations[altitude >= 400, alt_class := factor('high')]
ggplot(runoff_stations, aes(x = mean_days, y = area)) + 
  geom_point(aes( size=alt_class, col = area_class)) +
  xlim(c(0, 3000)) + 
  ylim(c(0, 200000)) + 
  labs(subtitle="runoff Vs area", 
       y="area", 
       x="mean run of in a day", 
       title="Scatterplot")
