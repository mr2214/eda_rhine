library(ggplot2)
runoff_stats
runoff_stats_tidy <- melt(runoff_stats, id.vars = 'sname')
str(runoff_stats_tidy)
runoff_stats_tidy
ggplot(runoff_stats_tidy, aes(x = sname, y = value, shape = "cyl", color = "cyl", size = 0.5)) + 
  geom_point(aes(col = variable, shape = variable))

##----Question 2 ------
runoff_stats$coefficient_of_variation <- runoff_stats$sd_day/runoff_stats$mean_day
#mean is closer to minimum value, hence data is positively skewed,
head(runoff_month)
tail(runoff_month)
runoff_stats
head(runoff_day)
head(runoff_month)
#q3
labs <- c("high", "medium", "low")
levels(runoff_stats_class$class) <- rev(labs)
levels(runoff_stats_class$runoff_class) <- rev(labs)
ggplot(runoff_month, aes(x = factor(month), y = value,)) +
  geom_boxplot(fill = colset_4[4]) +
  facet_wrap(~ sname, scales = 'free') + 
  theme_bw()
runoff_stats_class

head(runoff_day)
ggplot(runoff_day, aes(x = date, y = value, group = sname)) +
  geom_boxplot(fill = colset_4[4]) +
  facet_wrap(~ sname, scales = 'free') + 
  theme_bw()
#outliers, tend to be greater than 3rd quartile, no outliers below 1st quartile, this hapens due to the fact the mean is closer to zero than the max point, rainfall function is very erratic and some days can expriance very heavy rainfall
#----q4----
runoff_stations$mean_days <- runoff_stats_tidy$value[1:20]
runoff_stations
ggplot(runoff_stations, aes(x = mean_days, y = area)) + 
  geom_point(aes( size=alt_class, col = area_class)) +
  xlim(c(0, 3000)) + 
  ylim(c(0, 200000)) + 
  labs(subtitle="runoff Vs area", 
       y="area", 
       x="mean run of in a day", 
       title="Scatterplot")
