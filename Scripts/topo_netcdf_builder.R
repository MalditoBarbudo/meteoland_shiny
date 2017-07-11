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
