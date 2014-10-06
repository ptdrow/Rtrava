#tidyDataEfforts <- function(dataKOM) {
segments_ids <- numeric(length(dataKOM))
cities <- numeric(length(dataKOM))
states <- numeric(length(dataKOM))
countries <- numeric(length(dataKOM))

for(i in seq_along(dataKOM)){
      segments_ids[i] <- dataKOM[[i]]$segment$id
      cities[i] <- dataKOM[[i]]$segment$city
      states[i] <- dataKOM[[i]]$segment$state
      countries[i] <- dataKOM[[i]]$segment$country
}
cities <- unique(cities)
states <- unique(states)
countries <- unique(countries)

segments_num <- length(segments_ids)

