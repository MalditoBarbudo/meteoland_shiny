# Script to get the stations info, as well as setting the corresponding network
# to the station.

coords <- as.data.frame(interpolator@coords)
coords <- cbind(rownames(interpolator@MaxTemperature), coords)
names(coords) <- c('St_Id', 'long', 'lat')
coordinates(coords) <- ~long+lat
proj4string(coords) <- CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +towgs84=0,0,0")
stations_data <- spTransform(coords, CRS("+proj=longlat +datum=WGS84"))

st_df <- stations_data@data

library(stringr)
library(dplyr)

st_df %>%
  mutate(st_network = if_else(
    str_detect(St_Id, regex("^[a-zA-Z]")),
    "SMC", "AEMET"
  )) -> st_df

stations_data@data <- st_df

save(stations_data, file = 'Data/stations_data.RData')
