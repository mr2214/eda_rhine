#q1------------------------------------------
library(data.table)
library(ggplot2)
library(plotly)

#doma is at a high altitude, hence high vaiability due to snow melt, and snow accumilation during winter months,
#q2-------------------------------------------
precipitaion_rhine_day <- readRDS('data/precip_day.rds')
precipitaion_rhine_day$month <- format(as.Date(precipitaion_rhine_day$date), "%m")
precipitaion_rhine_day$year <- format(as.Date(precipitaion_rhine_day$date), "%Y")

precipitaion_rhine_day[year < 1815, period := factor('1765-1815')]
precipitaion_rhine_day[(1815 <= year) & (year < 1865), period := factor('1815-1865')]
precipitaion_rhine_day[(1865 <= year) & (year < 1915), period := factor('1865-1915')]
precipitaion_rhine_day[(1915 <= year) & (year < 1965), period := factor('1915-1865')]
precipitaion_rhine_day[(1965 <= year) & (year < 2020), period := factor('1965-2020')]


ggplot(precipitaion_rhine_day, aes(period, value, fill = period)) +
  geom_boxplot() +
  facet_wrap(~month, scales = 'free_y') +
  scale_fill_manual(values =  c("red","blue","orange","green","yellow")) +
  xlab(label = "period") +
  ylab(label = "precipitation (m3/s)") +
  theme_bw()



new_precipation_rhine <- precipitaion_rhine_day[, mean(value), by = list(year)]
new_precipitaion_rhine_month <- precipitaion_rhine_day[, mean(value), by = list(month,year)]
new_precipitaion_rhine_month[year < 1815, period := factor('1765-1815')]
new_precipitaion_rhine_month[(1815 <= year) & (year < 1865), period := factor('1815-1865')]
new_precipitaion_rhine_month[(1865 <= year) & (year < 1915), period := factor('1865-1915')]
new_precipitaion_rhine_month[(1915 <= year) & (year < 1965), period := factor('1915-1865')]
new_precipitaion_rhine_month[(1965 <= year) & (year < 2020), period := factor('1965-2020')]

ggplot(new_precipitaion_rhine_month, aes(year, V1, fill = period)) +
    geom_point() +
    geom_smooth() +
  scale_fill_manual(values =  c("red","blue","orange","green","yellow")) +
  facet_wrap(~month, scales = "free_y")

#q3---------------------------------------------
#changes have taken place around mean discharge of river Rhine, over the past 20 years, mean discharge values have increased over the winter months, and decreased over the summer months, relative to averages over the past 100 years

#q4---------------------------------
#other factors influencing changing rhine discharge could include, changes in evapotranspiration, changes in soil/land capacit to hold water and human influnces on the river, looking at the precipitation data, there is little change over this time period due to precipitation(line 24), which suggests other issuses are at play. The extreme/sharp changes around DOMA, suggest factors around ice/snow are important, hence data, on glacier sizes(over the year as well as historically) and snow/depth would be important to measure.
