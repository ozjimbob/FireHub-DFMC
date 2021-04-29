#!/usr/local/bin/Rscript
library(raster)
library(tidyverse)
library(tidync)
library(glue)

dates<-seq(Sys.Date() - 30, Sys.Date()-2,by="day")
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

# Delete old files
VP3pm_delete <- list.files("VP3pm",full.names = TRUE)
pcp_delete <- list.files("pcp",full.names = TRUE)
Tmx_delete <- list.files("Tmx",full.names = TRUE)

unlink(VP3pm_delete)
unlink(pcp_delete)
unlink(Tmx_delete)

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

Tmx.tmp.r<-stack(list.files("Tmx/",pattern="tif",full.names = TRUE))
ea.tmp.r<-stack(list.files("VP3pm/",pattern="tif",full.names = TRUE))   # in hPa from AWAP, so multiply by 0.1 to convert to KPa
pcp.tmp.r<-stack(list.files("pcp/",pattern="tif",full.names = TRUE))

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


VPD_delete <- list.files("VPD",full.names = TRUE)
DFMC_delete <- list.files("DFMC",full.names = TRUE)

unlink(VP3pm_delete)
unlink(pcp_delete)



VPD_outlist <- glue("VPD/VPD_{dnow}.tif")
for(i in 1:nlayers(D.tmp.r)){
  writeRaster(D.tmp.r[[i]],VPD_outlist[i], overwrite=TRUE)
}

DFMC_outlist <- glue("DFMC/DFMC_{dnow}.tif")
DFMC.tmp.r = DFMC.tmp.r + pcp.tmp.r
DFMC.tmp.r = DFMC.tmp.r + mask_r
for(i in 1:nlayers(D.tmp.r)){
  writeRaster(DFMC.tmp.r[[i]],DFMC_outlist[i], overwrite=TRUE)
}