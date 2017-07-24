## Script to get the topo grid info

load('Data/pointtopo.Rdata')

coord_old_system <- as.matrix(
  read.table('Data/maskcat_1k_poi.txt',
             sep = '\t', header = TRUE)[, c('x', 'y')]
)

coords <- SpatialPoints(
  coord_old_system,
  CRS('+init=epsg:3043 +proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
)

topo_data <- points@data

grid_as_points_topography <- SpatialPointsTopography(
  coords,
  topo_data$elevation,
  topo_data$slope,
  topo_data$aspect
)

################################################################################
