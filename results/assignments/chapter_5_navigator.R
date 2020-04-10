#q1------------------------------------
library(data.table)
library(ggplot2)
runoff_month_key <- readRDS('data/runoff_month_key.rds')
runoff_year_key <- readRDS('data/runoff_year_key.rds')
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")
theme_set(theme_bw())
year_thres <- 2000
runoff_year_key <- runoff_year_key[value >= 0]  
runoff_month_key <- runoff_month_key[value >= 0]  
head(runoff_month_key)
runoff_year_key[year < year_thres, period := factor('pre_2000')]
runoff_year_key[year >= year_thres, period := factor('aft_2000')]
runoff_month_key[year < year_thres, period := factor('pre_2000')]
runoff_month_key[year >= year_thres, period := factor('aft_2000')]

ggplot(runoff_year_key, aes(year, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "time period") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
##little change in mean values pre 2000 and post 2000
ggplot(runoff_month_key, aes(date, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "time period") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
##small increase in mean values
#q2---------------------------------------------
runoff_day <- readRDS("./data/runoff_day_raw.rds")
str(runoff_day)
head(runoff_day)
tail(runoff_day)
?.N
runoff_day <- runoff_day[value>0]
head(runoff_day)
quantiles <- runoff_day[,quantile(value, probs = seq(0, 1, 1/10)), by=list(sname,month)]
quantiles_2 <- runoff_day[,quantile(value, probs = seq(0, 1, 1/10)), by = month]

print(quantiles, nrow=200)
?quantile
head(runoff_month)
quantiles[selector,]
selector <- sort(x = (c(seq(from = 2, to = 212, by = 11),seq(from = 10, to = 220, by = 11))), decreasing = FALSE)
selector
runoff_day$month <- format(as.Date(runoff_day$date), "%m")
runoff_day

x <- seq(as.POSIXct("2012-05-21"), by=("+1 hour"), length.out=5)
x
data.frame(
  date=x,
  time=format(x, "%H:%M")
)
quantiles <- runoff_day[, q_10 := quantile(value, 0.1), by = .(sname, month)]
quantiles <- runoff_day[, q_90 := quantile(value, 0.9), by = .(sname, month)]
runoff_day$test <- inrange(x = runoff_day$value ,lower = runoff_day$q_10,upper = runoff_day$q_10)
new_runoff_day <- runoff_day[test == TRUE]  
new_runoff_day$year <- format(as.Date(new_runoff_day$date), "%Y")
?.N
head(new_runoff_day)
total <- new_runoff_day[, .N, by = list(sname, month, year)]
ggplot(total, aes(year, N ,colour = sname)) + 
  geom_point()
# density of points increasing with time
#q3---------------------------------------------------------------------------------
runoff_winter <- readRDS('data/runoff_winter.rds')
runoff_summer <- readRDS('data/runoff_summer.rds')
runoff_summary <- readRDS(('data/runoff_summary.rds'))
runoff_winter[, value_norm := scale(value), sname]
runoff_summer[, value_norm := scale(value), sname]
n_stations <- nrow(runoff_summary)
runoff_winter <- runoff_winter[1950 < year]
runoff_winter <- runoff_winter[2010 > year]
ggplot(runoff_winter[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
ggplot(runoff_winter[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
"""falling between 1995 and 2010"""
runoff_summer <- runoff_summer[1950 < year]
runoff_summer <- runoff_summer[2010 > year]
ggplot(runoff_summer[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
runoff_summer <- runoff_summer[1950 < year]
runoff_summer <- runoff_summer[2010 > year]
ggplot(runoff_summer[year > 1950], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
#seems trend is decreasing
  
  