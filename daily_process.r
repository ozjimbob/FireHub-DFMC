#!/usr/bin/Rscript

# Download AWAP data

#.libPaths(c("C:\\Users\\30038555\\DOCUMENTS\\R", .libPaths()))
library(RCurl)
library(rgdal)
library(sp)
library(raster)
#library("devtools")
#install_version("uncompress","1.34")
#library(uncompress)
# set wd

dir.create("VP3pm")
dir.create("Tmx")
dir.create("pcp")
dir.create("DFMC")
dir.create("VPD")
dir.create("VPDd")

file.remove(list.files("VP3pm",full.names=TRUE))
file.remove(list.files("Tmx",full.names=TRUE))
file.remove(list.files("pcp",full.names=TRUE))

#daily VP (vapour pressure at 3pm)
# Set date range and download 
dates<-seq(Sys.Date()-20,Sys.Date()-2,by="day")

for(i in 1:length(dates)){
  D<-strftime(dates[i],format = "%d")
  d2<-strftime(dates[i+1]-1,format = "%d")
  M<-strftime(dates[i],format = "%m")
  m2<-strftime(dates[i+1]-1,format = "%m")
  Y<-strftime(dates[i],format = "%Y")
  y2<-strftime(dates[i+1]-1,format = "%Y")
  download.file(paste0("http://www.bom.gov.au/web03/ncc/www/awap/vprp/vprph15/daily/grid/0.05/history/nat/",Y,M,D,Y,M,D,".grid.Z"),
                #paste0("E:/Data/Climate/AWAP/AWAP_Daily/VP3pm/VP3pm_",Y,M,D,".grid.Z"),
                paste0("VP3pm/VP3pm_",Y,M,D,".grid.Z"),
                mode="wb")
  message(paste0(i," out of ",length(dates)))
}

for(i in 1:length(dates)){
  D<-strftime(dates[i],format = "%d")
  d2<-strftime(dates[i+1]-1,format = "%d")
  M<-strftime(dates[i],format = "%m")
  m2<-strftime(dates[i+1]-1,format = "%m")
  Y<-strftime(dates[i],format = "%Y")
  y2<-strftime(dates[i+1]-1,format = "%Y")
  download.file(paste0("http://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/daily/grid/0.05/history/nat/",Y,M,D,Y,M,D,".grid.Z"),
                #paste0("E:/Data/Climate/AWAP/AWAP_Daily/VP3pm/VP3pm_",Y,M,D,".grid.Z"),
                paste0("pcp/pcp_",Y,M,D,".grid.Z"),
                mode="wb")
  message(paste0(i," out of ",length(dates)))
}


#daily Tmax (maximum daily temperature)

# Set date range and download 

for(i in 1:length(dates)){
  D<-strftime(dates[i],format = "%d")
  d2<-strftime(dates[i+1]-1,format = "%d")
  M<-strftime(dates[i],format = "%m")
  m2<-strftime(dates[i+1]-1,format = "%m")
  Y<-strftime(dates[i],format = "%Y")
  y2<-strftime(dates[i+1]-1,format = "%Y")
  
  
  download.file(paste0("http://www.bom.gov.au/web03/ncc/www/awap/temperature/maxave/daily/grid/0.05/history/nat/",Y,M,D,Y,M,D,".grid.Z"),
                #paste0("E:/Data/Climate/AWAP/AWAP_Daily/Tmx/Tmx_",Y,M,D,".grid.Z"),
                paste0("Tmx/Tmx_",Y,M,D,".grid.Z"),
                mode="wb")
  
  message(paste0(i," out of ",length(dates)))
}

# Precipitation

################################################################
# name:ZipFunctions.R
uncompress_linux <- function(filename)
{ print(filename)
  system(sprintf('uncompress %s',filename))
}

# tries to find 7 zip exe
ExecutableFileName7Zip <- function()
{ executableName <- "C:\\Program Files\\7-Zip\\7z.exe"
  if(file.exists(executableName))
  { return (executableName)
  }
  #other executable file names and ideas go here ...
  stop("failed to find 7zip")
}

# simple function to extract 7zip file, need to have 7zip installed
Decompress7Zip <- function(zipFileName, outputDirectory, delete)
{  executableName <- ExecutableFileName7Zip()
  arguments <- paste(sep="", "e ", "\"", zipFileName, "\" ",
                     "\"-o", outputDirectory, "\" ", "")
    print( arguments)
    RunProcess(executableName, arguments)
  
  if(delete)
  { unlink(zipFileName);
  }
}

################################################################
# name:ProcessFunctions.R
RunProcess = function(executable, arguments)
{ command = paste(sep="", "\"", executable,  "\" ", arguments);
  print (command)
  exitCode = system(command, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, 
                    wait = TRUE, input = NULL, show.output.on.console = TRUE, invisible = FALSE);
  if(exitCode != 0)
  {stop("Process returned error");
  }
  return (exitCode)
}

################################################################
# Import AWAP Tmax and 15:00 hour actual vapour pressure grids
# Compute VPD and DFMC using Resco et al. model with Nolan et al. 2016 calibration (RSE paper)



# List the Tmax and VP files
Tmx.lst<-list.files(path="Tmx", pattern=".Z", include.dirs = TRUE, full.names=TRUE)
VP.lst<-list.files(path="VP3pm", pattern=".Z", include.dirs = TRUE, full.names=TRUE)
pcp.lst<-list.files(path="pcp", pattern=".Z", include.dirs = TRUE, full.names=TRUE)

# DFMC model (Resco et al., AFM 2016) with the calibration coefficients for SE Australia from Nolan et al. 2016. (RSE)
DFMCfun <- function(x) {
  y <- 6.79+27.43*exp(-1.05*x)
  return(y)}

for (f in 1:length(Tmx.lst)){
  
  if(.Platform$OS.type=="unix"){
    uncompress_linux(Tmx.lst[f])
    uncompress_linux(VP.lst[f]) 
    uncompress_linux(pcp.lst[f]) 
  }else{
    Decompress7Zip(Tmx.lst[f],"Tmx/", FALSE)
    Decompress7Zip(VP.lst[f],"VP3pm/", FALSE) 
    Decompress7Zip(pcp.lst[f],"pcp/", FALSE) 
  }
    
    Tmx.tmp.r<-raster(unlist(strsplit(Tmx.lst[f], ".Z")))
    ea.tmp.r<-raster(unlist(strsplit(VP.lst[f], ".Z")))   # in hPa from AWAP, so multiply by 0.1 to convert to KPa
    pcp.tmp.r<-raster(unlist(strsplit(pcp.lst[f], ".Z"))) 
    
    crs(Tmx.tmp.r)<-CRS(SRS_string = "EPSG:4326")
    crs(ea.tmp.r)<-CRS(SRS_string = "EPSG:4326")
    crs(pcp.tmp.r)<-CRS(SRS_string = "EPSG:4326")
    
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
    outputfile_VPD<-unlist(strsplit(gsub('Tmx/Tmx','VPD/VPD',Tmx.lst[f]),".grid.Z"))
    #outputfile<-unlist(strsplit(gsub('/Tmx/gridfiles/Tmx','/DFMC/DFMC',Tmx.lst[f]),".grid"))
    writeRaster(D.tmp.r,paste0(outputfile_VPD,".tif"), overwrite=TRUE)
    
    outputfile_DFMC<-unlist(strsplit(gsub('Tmx/Tmx','DFMC/DFMC',Tmx.lst[f]),".grid.Z"))
    #outputfile<-unlist(strsplit(gsub('/Tmx/gridfiles/Tmx','/DFMC/DFMC',Tmx.lst[f]),".grid"))
    DFMC.tmp.r = DFMC.tmp.r + pcp.tmp.r
    DFMC.tmp.r = DFMC.tmp.r + mask_r
    writeRaster(DFMC.tmp.r,paste0(outputfile_DFMC,".tif"), overwrite=TRUE)
  }
    
# Clear history
old_dates <- seq(dates-31,dates,by="day")
keep_DFMC_list <- sprintf("DFMC/DFMC_%s%s%s.tif",format(old_dates,"%Y"),format(old_dates,"%m"),format(old_dates,"%d"))
keep_VPD_list <- sprintf("VPD/VPD_%s%s%s.tif",format(old_dates,"%Y"),format(old_dates,"%m"),format(old_dates,"%d"))

DFMC_exist <- list.files("DFMC",full.names = TRUE)
VPD_exist <- list.files("VPD",full.names=TRUE)

DFMC_diff <- setdiff(DFMC_exist,keep_DFMC_list)
VPD_diff <- setdiff(VPD_exist,keep_VPD_list)

unlink(DFMC_diff)
unlink(VPD_diff)

# Trend Analysis
VPD_stack <- stack(list.files("VPD",full.names=TRUE))

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
writeRaster(test[[1]],"VPDd/VPD_trend.tif")