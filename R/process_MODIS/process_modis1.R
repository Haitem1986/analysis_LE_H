#install.packages("MODISTools")
#install.packages("MODIS")
library(MODIS)
library(MODISTools)

coord <- c(5.4750,100.2025)
product <- "MOD28"
# What to query. You can get the names via GetBands
bands <- c("250m_16_days_EVI","250m_16_days_pixel_reliability") 
# You can save the downloaded File in a specific folder
savedir <- "/Users/Yusri/Documents/Work/Data_analysis/muka_head/modis/" 
# Get the central pixel only (0,0) or a quadratic tile around it
pixel <- c(0,0) 
# To download the pixels
period <- data.frame(lat=coord[1],long=coord[2],
                     start.date=2015,end.date=2017,id=1)

MODISSubsets(LoadDat = period,Products = product,Bands = bands,
             Size = pixel,SaveDir = savedir,StartDate = T)
MODISSummaries(LoadDat = period,FileSep = ",", Product = "MOD13Q1", 
               Bands = "250m_16_days_EVI",ValidRange = c(-2000,10000), 
               NoDataFill = -3000, ScaleFactor = 0.0001,
               StartDate = TRUE,Yield = T,Interpolate = T, 
               QualityScreen = TRUE, QualityThreshold = 0,
               QualityBand = "250m_16_days_pixel_reliability")
# Finally read the output
read.table("MODIS_Summary_MOD13Q1_2014-08-10.csv",header = T,sep = ",")
