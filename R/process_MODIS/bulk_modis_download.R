########################################
## Bulk download data from MODIS website
## Note: You need to determine the type of file
## from the MODIS data website
## 
## Author: Yusri Yusup, PhD
## Affiliation: Environmental Technology,
## School of Industrial Technology, 
## Universiti Sains Malaysia
## Date: 2017-04-20
########################################

# set working directory-just in case
setwd("/Users/Yusri/Documents/Work/Data_analysis/muka_head")

# type of data
type <- 'L3m_DAY_SST_sst_4km.nc' ## CHANGE HERE

# generate names
year <- '2017' #################### CHANGE HERE
start_day <- 1 ################## CHANGE HERE
end_day <- 18
day <- seq(from = start_day,to = end_day)
day1 <- 0
for (i in 1:length(day)){
  if (day[i] < 10){
    day1[i] <- paste('00',day[i], sep='')
  }
  else if (day[i] >= 10 & day[i] < 100) {
    day1[i] <- paste('0',day[i], sep='')
  }
  else if (day[i] >= 100){
    day1[i] <- as.character(day[i])
  }
}
day <- day1
rm(day1)
filenames <- 0
for (i in 1:length(day)) {
  filenames[i] <- paste('A',year,day[i],sep='')
}

# initialize url_modis
url_modis <- 0
# urls
for (i in 1:length(filenames)){
  url_modis[i] <- paste('https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/',filenames[i],
                        '.',type, sep = '')
}

# create names for downloaded files
destfile <- strsplit(url_modis, split = '/')
destfile1 <- 0
for(i in 1:length(destfile)){
  destfile1[i] <- destfile[[i]][6]
}
destfile <- destfile1
rm(destfile1)


# bulk download
for (i in 1:length(url_modis)){
  download.file(url_modis[i], 
                destfile = paste('modis/sst/',destfile[i], sep=''), 
                method="curl")
}

## END
