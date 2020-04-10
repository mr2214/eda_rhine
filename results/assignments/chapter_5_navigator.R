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
runoff_month_key
?ggplot
ggplot(runoff_month_key, aes(factor(month), value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "time period") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
ggplot(runoff_month_key) +
  geom_boxplot(aes(y=x,
                   x=reorder(runoff_month_key$month),
                   fill = period)) +
  xlab('Month') + guides(fill=guide_legend(title="Year")) +
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

runoff_day$month <- format(as.Date(runoff_day$date), "%m")
runoff_day$year <- format(as.Date(runoff_day$date), "%Y")


quantiles <- runoff_day[, q_10 := quantile(value, 0.1), by = .(sname, month)]
quantiles <- runoff_day[, q_90 := quantile(value, 0.9), by = .(sname, month)]
quantiles
quantiles[year >= 1986, year_class := factor('post_1986')]
quantiles[year < 1986, year_class := factor('pre_1986')]
tenthq <- [quantiles[,(value<q_10), by = sname]]
tenthq <- subset(x = quantiles, subset = value < q_10)
nine_tenth_q <- subset(x = quantiles, subset = value > q_90)
?.N
tenthq
number_10_days <- tenthq[, .N, by = list(sname,year_class,month,year)]
number_90_days <- nine_tenth_q[, .N, by = list(sname,year_class,month,year)]
number_10_days[(month >= 7) & (month <= 9), season := factor('summer')]
number_10_days[(month >= 1) & (month <= 3), season := factor('winter')]

ggplot(number_10_days, aes(factor(season), N, fill = year_class)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "time period") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()

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
  
  