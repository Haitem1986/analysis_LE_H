# Antonio Olinto Avila-da-Silva, Instituto de Pesca, Brasil
# ver 2016-05-17
# https://gist.github.com/aolinto/79e184f6c156c6ab21b3
# script to process Aqua MODIS Sea Surface Temperature
# files downloaded from http://oceancolor.gsfc.nasa.gov/cgi/l3
# Aqua MODIS Sea Surface temperature 11 u daytime Monthly 9 km SMI images
# all .L3m_MO_SST_sst_9km.nc files must be in the working directory
# the script will open each nc file, read date, lon, lat and sst data,
# then select data from specified area and write them into
# a single csv file named MODISA_sst.csv
# Some reference pages
# http://geog.uoregon.edu/GeogR/topics/netCDF-read-ncdf4.html
# https://scottishsnow.wordpress.com/2014/08/24/many-rastered-beast/

# load libraries
# ncdf4 needs libnetcdf-dev netcdf-bin in Linux
#install.packages(c("ncdf4","reshape2"))
library("ncdf4")
library("reshape2")

# set working directory
setwd("modis/sst/")   # indicate the path to the files
file.exists("MODISA_sst.csv")     # caution new data will be appended to this file if it already exists
# file.rename("MODISA_sst.csv","MODISA_sst.old")
# file.remove("MODISA_sst.csv")

# set the study area in decimal degrees
lonmax<- 107
lonmin<- 95
latmax<- 12
latmin<- 0

# create a list of files and indicate its length
(f <- list.files(".", pattern="*.L3m_DAY_SST_sst_4km.nc",full.names=F))
(lf<-length(f))

# variable
var<-"sst"

for (i in 1:lf) {
  # progress indicator
  print(paste("Processing file",i,"from",length(f),f[i],sep=" "))
  # open netCDF file
  data<-nc_open(f[i])
  # extract data
  lon<-ncvar_get(data,"lon")
  lat<-ncvar_get(data,"lat")
  value<-ncvar_get(data,var)
  unit<-ncatt_get(data,var,"units")$value
  # matrix to data.frame
  dimnames(value)<-list(lon=lon,lat=lat)
  dat.var<-melt(value,id="lon")
  # select data from the study area taking out missing data
  dat.varSAtmp<-subset(dat.var,lon<=lonmax & lon>=lonmin & lat<=latmax & lat>=latmin & value<45)
  # extract date information
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year<-substring(datemean,0,4)
  month<-substring(datemean,6,7)
  # prepare final data set
  dat.varSA<-data.frame(rep(as.integer(year,nrow(dat.varSAtmp))),rep(as.integer(month,nrow(dat.varSAtmp))),
                        dat.varSAtmp,rep(unit,nrow(dat.varSAtmp)),rep(var,nrow(dat.varSAtmp)))
  names(dat.varSA)<-c("year","month","lon","lat","value","unit","var")
  # save csv file
  fe<-file.exists("MODISA_sst.csv")
  write.table(dat.varSA,"MODISA_sst.csv",row.names=FALSE,col.names=!fe,sep=";",dec=",",append=fe)
  # close connection
  nc_close(data)
  # clean workspace
  rm(data,lon,lat,value,unit,dat.var,dat.varSAtmp,dateini,dateend,datemean,year,month,dat.varSA,fe)
}
rm(var,f,i,latmax,latmin,lf,lonmax,lonmin)

# Reset the working directory
setwd("/Users/Yusri/Documents/Work/Data_analysis/muka_head")

# Import data
sst_modis <- read.csv('modis/sst/MODISA_sst.csv', sep = ';')

# Change from factors to numeric for lon, lat, and value
sst_modis$lon <- as.character(sst_modis$lon)
sst_modis$lat <- as.character(sst_modis$lat)
sst_modis$value <- as.character(sst_modis$value)

sst_modis$lon <- as.numeric(gsub(",", ".", sst_modis$lon))
sst_modis$lat <- as.numeric(gsub(",", ".", sst_modis$lat))
sst_modis$value <- as.numeric(gsub(",", ".", sst_modis$value))

