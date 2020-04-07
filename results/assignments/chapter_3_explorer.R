
##question 1
"""
1. km^2 are units for river catchment areas,
runnoff in m^3/s
"""
#question 2
runoff_stations
average_catchment_area <- sum(runoff_stations$area)/20   #km^2
seperated_values <- split(x = runoff_day_raw$value, f = levels(runoff_day_raw$sname))
seperated_values$ANDE[seperated_values$ANDE < 0] <- NA
mean_result <- c()
for (index in 1:length(seperated_values)){
  seperated_values[[index]][seperated_values[[index]] < 0] <- NA
  mean_result[index] <- mean(x = seperated_values[[index]], na.rm = TRUE)
}
mean_result[1:20] ## mean average runoff for each catchment
average_runoff <- (unlist(mean_result[1:20]))

### question 3
barplot(height = average_runoff, names.arg = newnames, width = 0.2, axis.lty = c(1000,1300))

runoff_stations
##question 4
"""
at higher altitudes the area is smaller, at lower altitudes, rivers widen. in mountains you will always be close to a watershed point, and amount of land decreases as a function of height
"""
