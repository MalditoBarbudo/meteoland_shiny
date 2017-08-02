# Script to produce the netCDF file with the topography data

# libraries
library(meteoland)
library(ncdf4)

# load rdata topography object
load('Data/topo30m.rdata')

# helper function
write_topo_grid_helper <- function(data, grid, proj4string, file) {
  
  # get grid dimensions
  nx <- grid@cells.dim[1]
  ny <- grid@cells.dim[2]
  
  # Writes rows by decreasing order
  putvargriddata <- function(nc, var, datavec) {
    for (i in 1:ny) {
      ncvar_put(nc, var,
                datavec[((i - 1) * nx + 1):(i * nx)],
                start = c(1, ny - i + 1),
                count = c(nx, 1))
    }
  }
  
  # define dimensions and variables
  dimX <- ncdim_def("X", "meters", sort(unique(coordinates(grid)[,1])))
  dimY <- ncdim_def("Y", "meters", sort(unique(coordinates(grid)[,2])))
  varElevation <- ncvar_def('Elevation', 'meters', list(dimX, dimY), NA)
  varSlope <- ncvar_def('Slope', 'degrees', list(dimX, dimY), NA)
  varAspect <- ncvar_def('Aspect', 'degrees from north', list(dimX, dimY), NA)
  
  # open the connection to the netCDF file with the vars previously created
  nc <- nc_create(file, list(varElevation, varSlope, varAspect))
  
  # write the global attributes of the netCDF file
  ncatt_put(nc, 0, "proj4string", as.character(proj4string))
  # ncatt_put(nc, 0, "date", as.character(date))
  
  # populate the variables with putvargriddata function
  putvargriddata(nc, varElevation, data$elevation)
  putvargriddata(nc, varSlope, data$slope)
  putvargriddata(nc, varAspect, data$aspect)
  
  # close the connection
  nc_close(nc)
}

write_topo_grid <- function(object, file) {
  write_topo_grid_helper(object@data, object@grid,
                         '+init=epsg:3043 +proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs',
                         file)
}

write_topo_grid(landtopo, 'Data/Topology_grid.nc')

# plotting the file with raster to take a quick look
par(mfrow = c(2,2))
plot(raster::raster('Data/Topology_grid.nc', varname = "Elevation")[[1]])
plot(raster::raster('Data/Topology_grid.nc', varname = "Aspect")[[1]])
plot(raster::raster('Data/Topology_grid.nc', varname = "Slope")[[1]])
par(mfrow = c(1,1))


### how to extract a variable value for an specific coordinates

## read and test the file created
nc <- nc_open('Data/Topology_grid.nc')

# get x and y coordinates
x_coord <- nc$dim$X$vals
y_coord <- nc$dim$Y$vals

# desired x and y coordinate
desired_x <- 258030
desired_y <- 4484980

# difference for x
dist_x <- abs(x_coord - desired_x)
index_x <- which.min(dist_x)

# difference for y
dist_y <- abs(y_coord - desired_y)
index_y <- which.min(dist_y)

# index
index_ok <- c(index_x, index_y)

ncvar_get(nc, 'Elevation', index_ok, c(1,1))
ncvar_get(nc, 'Slope', index_ok, c(1,1))
ncvar_get(nc, 'Aspect', index_ok, c(1,1))

nc_close(nc)
