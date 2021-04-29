#!/usr/local/bin/Rscript
library(raster)
library(tidyverse)
library(tidync)
library(glue)

dates<-Sys.Date() - 2
dnow <- format(dates,"%Y%m%d")
dyear <- format(dates,"%Y")


precip_list <- glue("http://opendap.bom.gov.au:8080/thredds/dodsC/agcd/precip/total/r005/01day/{dyear}/precip_total_r005_{dnow}_{dnow}.nc")
vp_list <- glue("http://opendap.bom.gov.au:8080/thredds/dodsC/agcd/vapourpres_h15/mean/r005/01day/{dyear}/vapourpres_h15_mean_r005_{dnow}_{dnow}.nc")
tmax_list <- glue("http://opendap.bom.gov.au:8080/thredds/dodsC/agcd/tmax/mean/r005/01day/{dyear}/tmax_mean_r005_{dnow}_{dnow}.nc")

proc_ncdf_1 <- function(x){
  tidync(x[1]) %>% hyper_tibble
}

precip_df <- map_df(precip_list,proc_ncdf_1)
vp_df <- map_df(vp_list,proc_ncdf_1)
tmax_df <- map_df(tmax_list,proc_ncdf_1)

precip_df <- precip_df %>% group_by(time) %>% nest()
vp_df <- vp_df %>% group_by(time) %>% nest()
tmax_df <- tmax_df %>% group_by(time) %>% nest()

precip_df <- precip_df %>% mutate(time = as.Date(time,origin="1850-01-01"))
vp_df <- vp_df %>% mutate(time = as.Date(time,origin="1850-01-01"))
tmax_df <- tmax_df %>% mutate(time = as.Date(time,origin="1850-01-01"))


list_to_raster <- function(x){
  ext_1 <- extent(min(x$lon)-0.025,max(x$lon)+0.025,min(x$lat)-0.025,max(x$lat)+0.025)
  temp_raster <- raster(ext_1,res=0.05,crs=CRS("EPSG:4326"))
  values(temp_raster)<-unlist(x[,1])
  temp_raster
}



precip_df <- precip_df %>% mutate(rast = map(data,list_to_raster))
vp_df <- vp_df %>% mutate(rast = map(data,list_to_raster))
tmax_df <- tmax_df %>% mutate(rast = map(data,list_to_raster))


for(i in 1:nrow(vp_df)){
  this_date <- vp_df$time[i]
  writeRaster(vp_df$rast[[i]],glue("VP3pm/VP3pm_{this_date}.tif"),overwrite=TRUE)
}
for(i in 1:nrow(precip_df)){
  this_date <- precip_df$time[i]
  writeRaster(precip_df$rast[[i]],glue("pcp/pcp_{this_date}.tif"),overwrite=TRUE)
}
for(i in 1:nrow(tmax_df)){
  this_date <- tmax_df$time[i]
  writeRaster(tmax_df$rast[[i]],glue("Tmx/Tmx_{this_date}.tif"),overwrite=TRUE)
}

rm(precip_df,vp_df,tmax_df)

Tmx.tmp.r<-raster(glue("Tmx/Tmx_{this_date}.tif"))
ea.tmp.r<-raster(glue("VP3pm/VP3pm_{this_date}.tif"))   # in hPa from AWAP, so multiply by 0.1 to convert to KPa
pcp.tmp.r<-raster(glue("pcp/pcp_{this_date}.tif"))

DFMCfun <- function(x) {
  y <- 6.79+27.43*exp(-1.05*x)
  return(y)}

# Make precipitation mask
pcp.tmp.r[pcp.tmp.r>2] = NA
pcp.tmp.r[!is.na(pcp.tmp.r)] = 0

ea.tmp2.r<-ea.tmp.r*0.1
# From daily mean temperature (T) we can compute saturation vapour pressure:
es.tmp.r<-0.6108*exp(17.27*Tmx.tmp.r/(Tmx.tmp.r + 237.3)) # In KPa!!
# ...and VPD in KPa, constrain to 0 when calculated RH >100%
D.tmp.r<-ea.tmp2.r-es.tmp.r
D.tmp.r[D.tmp.r>0]<-0
D.tmp.r <- D.tmp.r*-1
print(summary(getValues(D.tmp.r)))
# compute DFMC (%)
DFMC.tmp.r<-calc(x=D.tmp.r, fun=DFMCfun)
print(summary(getValues(DFMC.tmp.r)))

mask_r <- raster("aus_mask.tif")
D.tmp.r = D.tmp.r + mask_r




VPD_outlist <- glue("VPD/VPD_{dnow}.tif")
writeRaster(D.tmp.r,VPD_outlist, overwrite=TRUE)


DFMC_outlist <- glue("DFMC/DFMC_{dnow}.tif")
DFMC.tmp.r = DFMC.tmp.r + pcp.tmp.r
DFMC.tmp.r = DFMC.tmp.r + mask_r
writeRaster(DFMC.tmp.r,DFMC_outlist, overwrite=TRUE)


# Trend Analysis
file_list = list.files("VPD",full.names=TRUE)
lfl <- length(file_list)
file_list = file_list[(lfl-10):lfl]
VPD_stack <- stack(file_list)

modl <- function(x){
  if(is.na(x[1])){return(c(NA,NA))}
  day <- 1:length(x)
  md <- lm(x~day)
  slp <- md$coefficients[2]
  r2 <- summary(md)$r.squared
  as.numeric(c(slp,r2))
}

test <- calc(VPD_stack,fun=modl)

mask_r = test[[2]]
mask_r[mask_r<0.3]=NA
mask_r[!is.na(mask_r)]=0
test[[1]]=test[[1]] + mask_r

writeRaster(test[[1]],"VPD_trend.tif",overwrite=TRUE)

