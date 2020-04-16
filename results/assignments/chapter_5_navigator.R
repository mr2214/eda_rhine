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
runoff_year_key[year < year_thres, period := factor('pre_2000')]
runoff_year_key[year >= year_thres, period := factor('aft_2000')]
runoff_month_key[year < year_thres, period := factor('pre_2000')]
runoff_month_key[year >= year_thres, period := factor('aft_2000')]

ggplot(runoff_month_key, aes(factor(month), value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "time period") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
##little change in mean values pre 2000 and post 2000
ggplot(runoff_month_key, aes(period, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "time period") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
##small increase in mean values
#q2---------------------------------------------
runoff_day <- readRDS("./data/runoff_day_raw.rds")
runoff_day <- runoff_day[value>0]
doma <- subset(x = runoff_day, sname == "DOMA")
basr <- subset(x = runoff_day, sname == "BASR")
koel <- subset(x = runoff_day, sname == "KOEL")
three_sations <- rbind(doma,basr,koel)
three_sations$month <- format(as.Date(three_sations$date), "%m")
three_sations$year <- format(as.Date(three_sations$date), "%Y")
quantiles <- three_sations[, q_10 := quantile(value, 0.1), by = .(sname, month)]
quantiles <- three_sations[, q_90 := quantile(value, 0.9), by = .(sname, month)]

tenthq <- subset(x = quantiles, subset = value < q_10)
nine_tenth_q <- subset(x = quantiles, subset = value > q_90)
mean_low <- tenthq[,mean(value), by = .(sname, month)]
mean_high <- nine_tenth_q[,mean(value), by = .(sname, month)]
ggplot(data = mean_low, aes(x = month, y = V1)) +
  geom_point() +
  facet_wrap(~sname, scales = 'free_y') 
ggplot(data = mean_high, aes(x = month, y = V1)) +
  geom_point() +
  facet_wrap(~sname, scales = 'free_y')
tenthq
number_10_days <- tenthq[, .N, by = list(sname,year_class,month,year)]
number_10_days_summer <- subset(x = number_10_days, subset = (as.numeric(number_10_days$month) >=7 &
                                                                (as.numeric(number_10_days$month) <= 9)))
number_10_days_winter <- subset(x = number_10_days, subset = (as.numeric(number_10_days$month) >=1 &
                                                                (as.numeric(number_10_days$month) <= 3)))
number_90_days <- nine_tenth_q[, .N, by = list(sname,year_class,month,year)]
number_90_days_summer <- subset(x = number_90_days, subset = (as.numeric(number_90_days$month) >=7 &
                                                                (as.numeric(number_90_days$month) <= 9)))
number_90_days_winter <- subset(x = number_90_days, subset = (as.numeric(number_90_days$month) >=1 &
                                                                (as.numeric(number_90_days$month) <= 3)))
number_10_days_summer
ggplot(data = number_10_days_summer, aes(x = sname, y = N, fill = year_class)) +
  geom_boxplot()
ggplot(data = number_10_days_winter, aes(x = sname, y = N, fill = year_class)) +
  geom_boxplot()
ggplot(data = number_90_days_summer, aes(x = sname, y = N, fill = year_class)) +
  geom_boxplot()
ggplot(data = number_90_days_winter, aes(x = sname, y = N, fill = year_class)) +
  geom_boxplot()
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
"falling between 1995 and 2010"
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
  
  