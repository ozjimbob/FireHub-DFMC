# Make aus mask
library(tidyverse)
library(sf)
library(raster)

template = raster("VPD/VPD_20181231.tif")

values(template)=0
aus <- read_sf("E:/geodata/Aus_Coastline/australia/aust.gpkg")
aus <- aus %>% summarise()
aus <- st_buffer(aus,dist=0.1)

temp <- mask(template,aus)
crs(temp)<-CRS(SRS_string = "EPSG:4326")
writeRaster(temp,"aus_mask.tif",overwrite=TRUE)
