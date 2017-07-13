# Script for the calibration of the 61 daily stations data (current mode)

# libraries
library(meteoland)

# Let's create the interpolation data object
# date vector and length
datevec <- seq(as.Date('2017-01-01'), as.Date('2017-06-30'), 1)
datevec <- datevec[-148]
datevec <- datevec[-164]
ndays <- length(datevec)
excludeRainFromStations <- character(0)

# load the meteorological files
day_data <- vector('list', ndays)
for (i in seq_along(datevec)) {
  # check if file exists
  file_to_read <- file.path('Data', 'DailyCAT', paste0(as.character(datevec[[i]]), '.txt'))
  if (!file.exists(file_to_read)) {
    stop('Faltan dias')
  }
  # read data
  day_data[[i]] <- readmeteorologypoint(file_to_read)
  # codes
  codes <- row.names(day_data[[i]])
  # excluded codes
  excodes <- codes[codes %in% excludeRainFromStations]
  # NAs to excluded (apply quality check results)
  day_data[[i]][excodes, 'Precipitation'] <- NA
}

# get general info needed later
stations_codes <- row.names(day_data[[1]])
stations_elevation <- day_data[[1]]$elevation
stations_slope <- rep(0, length(stations_elevation))
stations_aspect <- rep(0, length(stations_elevation))
stations_coords <- cbind(day_data[[1]]$coords.x1, day_data[[2]]$coords.x2)
stations_coords_sp <- SpatialPoints(
  stations_coords, CRS("+proj=longlat +datum=WGS84")
)
station_coords_utm <- spTransform(
  stations_coords_sp,
  CRS("+init=epsg:3043 +proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
)

stations_n <- length(stations_elevation)

# reshape the data to build the MetereologyInterpolationData object
MinTemperature <- matrix(
  NA, nrow = stations_n, ncol = ndays,
  dimnames = list(stations_codes, as.character(datevec))
)
MaxTemperature <- MinTemperature
Precipitation <- MinTemperature
RelativeHumidity <- MinTemperature
Radiation <- MinTemperature
WindSpeed <- MinTemperature
WindDirection <- MinTemperature

# fill the data
for (i in seq_along(datevec)) {
  MinTemperature[,i] <- day_data[[i]][stations_codes, 'MinTemperature']
  MaxTemperature[,i] <- day_data[[i]][stations_codes, 'MaxTemperature']
  Precipitation[,i] <- day_data[[i]][stations_codes, 'Precipitation']
  RelativeHumidity[,i] <- day_data[[i]][stations_codes, 'MeanRelativeHumidity']
  Radiation[,i] <- day_data[[i]][stations_codes, 'Radiation']
  WindSpeed[,i] <- day_data[[i]][stations_codes, 'WindSpeed']
  WindDirection[,i] <- day_data[[i]][stations_codes, 'WindDirection']
}

# Finally, we build the interpolator object
interpolator_current <- MeteorologyInterpolationData(
  points = station_coords_utm,
  elevation = stations_elevation,
  slope = stations_slope,
  aspect = stations_aspect,
  MinTemperature = MinTemperature,
  MaxTemperature = MaxTemperature,
  Precipitation = Precipitation,
  RelativeHumidity = RelativeHumidity,
  Radiation = Radiation,
  WindSpeed = WindSpeed,
  WindDirection = WindDirection,
  params = defaultInterpolationParams()
)

# check for the spatial and the temporal coverage
head(interpolation.coverage(interpolator_current, percent = TRUE))
head(interpolation.coverage(interpolator_current, type = 'temporal', percent = TRUE))

# calibration, for each variable
# tmin
tmin_cal <- interpolation.calibration(
  interpolator_current, variable = 'Tmin',
  N_seq = seq(5, 60, by = 5),
  alpha_seq = seq(0.5, 10, by = 0.5)
)
# tmax
tmax_cal <- interpolation.calibration(
  interpolator_current, variable = 'Tmax',
  N_seq = seq(5, 60, by = 5),
  alpha_seq = seq(0.5, 10, by = 0.5)
)
# tdew
tdew_cal <- interpolation.calibration(
  interpolator_current, variable = 'Tdew',
  N_seq = seq(5, 60, by = 5),
  alpha_seq = seq(0.5, 10, by = 0.5)
)
# Prec
prec_cal <- interpolation.calibration(
  interpolator_current, variable = 'Prec',
  N_seq = seq(5, 60, by = 5),
  alpha_seq = seq(1, 10, by = 1)
)

# check the results
z = tmin_cal$MAE
x = as.numeric(rownames(tmin_cal$MAE))
y = as.numeric(colnames(tmin_cal$MAE))
filled.contour(x,y,z, main = "Minimum temperature",
               plot.axes = {points(tmin_cal$N, tmin_cal$alpha, cex = 1, pch = 4)
                 axis(1, 5:60)
                 axis(2, 0.5:10)},
               xlab = "Station density (N)",
               ylab = "Shape parameter (alpha)",
               color.palette = colorRampPalette(c("white","red","black")))

z = tmax_cal$MAE
x = as.numeric(rownames(tmax_cal$MAE))
y = as.numeric(colnames(tmax_cal$MAE))
filled.contour(x,y,z, main = "Maximum temperature",
               plot.axes = {points(tmax_cal$N, tmax_cal$alpha, cex = 1, pch = 4)
                 axis(1, 5:60)
                 axis(2, 0.5:10)},
               xlab = "Station density (N)",
               ylab = "Shape parameter (alpha)",
               color.palette = colorRampPalette(c("white","red","black")))

z = tdew_cal$MAE
x = as.numeric(rownames(tdew_cal$MAE))
y = as.numeric(colnames(tdew_cal$MAE))
filled.contour(x,y,z, main = "Dew temperature",
               plot.axes = {points(tdew_cal$N, tdew_cal$alpha, cex = 1, pch = 4)
                 axis(1, 5:60)
                 axis(2, 0.5:10)},
               xlab = "Station density (N)",
               ylab = "Shape parameter (alpha)",
               color.palette = colorRampPalette(c("white","red","black")))

z = prec_cal$MAE
x = as.numeric(rownames(prec_cal$MAE))
y = as.numeric(colnames(prec_cal$MAE))
filled.contour(x,y,z, main = "Precipitation",
               plot.axes = {points(prec_cal$N, prec_cal$alpha, cex = 1, pch = 4)
                 axis(1, 5:60)
                 axis(2, 1:10)},
               xlab = "Station density (N)",
               ylab = "Shape parameter (alpha)",
               color.palette = colorRampPalette(c("white","red","black")))

# updateamos el interpolator con los parámetros obtenidos
interpolator_current@params$N_MinTemperature = tmin_cal$N
interpolator_current@params$alpha_MinTemperature = tmin_cal$alpha
interpolator_current@params$N_MaxTemperature = tmax_cal$N
interpolator_current@params$alpha_MaxTemperature = tmax_cal$alpha
interpolator_current@params$N_DewTemperature = tdew_cal$N
interpolator_current@params$alpha_DewTemperature = tdew_cal$alpha
interpolator_current@params$N_PrecipitationEvent = prec_cal$N
interpolator_current@params$alpha_PrecipitationEvent = prec_cal$alpha
interpolator_current@params$N_PrecipitationAmount = prec_cal$N
interpolator_current@params$alpha_PrecipitationAmount = prec_cal$alpha
interpolator_current@params$St_Precipitation = 5
interpolator_current@params$pop_crit = 0.50
interpolator_current@params$f_max = 0.95

# save the objects to be able to recover without repeating the calibration
save(tmin_cal, tmax_cal, tdew_cal, prec_cal, file = 'Data/calibrations.RData')

# cross validation
