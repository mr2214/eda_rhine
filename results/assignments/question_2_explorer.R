rainfall <- 5 * 10 ** (-3)  ## meters/hour
total_catchment_area <- 220000 * 10 ** (6)  ##square meters
total_rainfall_in_day <- rainfall * 24 * total_catchment_area
average_runoff <- 2900  ###m^3/s
total_excess <- (average_runoff * 60 * 60* 24) + total_rainfall_in_day
new_runoff <- total_excess/(60 * 60 *24)
difference <- new_runoff - average_runoff
### Average runoff would increase by 305556m^3/s

##assume no water stored in soil, plants, groundwater