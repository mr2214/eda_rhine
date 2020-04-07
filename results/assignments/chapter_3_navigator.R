## Question 1
library(data.table)
library(ggplot2)
runoff_stations <- fread('./data/raw/runoff_stations.csv')
runoff_day_raw <- readRDS('./data/runoff_day_raw.rds')
station_names <- runoff_stations$station
head(runoff_stations)
newnames <- abbreviate(names.arg = station_names, minlength = 4)
head(runoff_stations)
runoff_stations_data <- data.table(runoff_stations$station, runoff_stations$area, runoff_stations$altitude)
names_ <- runoff_stations[, 2]
names_
length(names_)
colnames(runoff_stations_data) <- c("station", "area", "altitude")
colnames(runoff_stations_data)
runoff_stations_data

### question 2
ggplot(data = runoff_stations_data) +
  geom_line(aes(x = area, y = altitude))
### Question 3
ggplot(data = runoff_stations, aes(x = area, y = altitude)) +
  geom_text(aes(label = newnames), check_overlap = TRUE)
ggplot(data = runoff_stations, aes(x = lon, y = lat)) +
  geom_point(aes(size = altitude)) +
  geom_text(aes(label = newnames), check_overlap = TRUE)
### question 4
ggplot(data = runoff_day_raw, aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~sname) + 
  theme_bw()


            

            
runoff_stations
