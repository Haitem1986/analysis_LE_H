#### Script: Muka Head EC Data import ##########################
# Purpose: To import data from Muka Head EC station
# 
# TITLE: Air-sea interaction of a semi-enclosed tropical ocean:
# a study on carbon, water, and energy budget
#
# Grant: Research University Individual
# Grant no.:  1001/PTEKIND/811316
#
# Author: Yusri Yusup, PhD
# Date: 2015-11-12
# 
# Note: 
# 1. For data before 2015-12-02 10:30:00, all RN_1_1_1 should be
# divided by 13.6, corrections were made for data after this date.
# 2. Before the infrared sensor was TW_1_1_1, now it is TW_0_0_1
# since analysis 2017-05-21
# 3. Changed from TW_0_0_1 back to TW_1_1_1 since 2017-06-17
#### 1. Preliminaries #########################################
source('R/tools/tool_convert_magic.R')
source('R/tools/tool_charactersNumeric.R')
source('R/tools/trapezium_intg_3.R')
source('R/tools/trapezium_intg_2.R')

#### 2. Importing and processing the data #####################
# Import individual processed data files
df <- read.csv(file.choose(),skip=1)
df_biomet <- read.csv(file.choose())

# Delete unnecessary columns and rows in EC data files
df <- df[,-1] # Remove 1st column
df <- df[-1,] # Remove 1st row

# Delete unnecessary columns and rows in Biomet data files
df_biomet <- df_biomet[-1,] # Remove the 1st row
df_biomet <- df_biomet[,-c(3)] # Remove the first 3 columns to combine with df

# Merge the df and df_biomet
df <- merge(df,df_biomet,by=c('date','time'),all=TRUE)
# Combine df_biomet with df
#df <- cbind(df,df_biomet)

# Using convert_magic to convert all columns to 'character' first
df <- convert_magic(df[,c(seq(1,ncol(df)))],c(rep('character',times = ncol(df))))

# Changing all the '-9999.0' or '-9999' (missing data) to NA
for (i in 1:length(df)){
  df[i][df[i] == '-9999' | df[i] == '-9999.0'] <- NA
}
rm(i)

# Formatting time
time_stamp <- paste(df$date,df$time)

# Might need to change format of date 1/1/2014 or 2014-1-1
time_stamp <- strptime(time_stamp,"%Y-%m-%d %H:%M")
df$time <- time_stamp
df <- df[,c(-1)]
colnames(df)[1] <-'time_stamp'
# Remove 'DOY' (Day Of Year)
df <- df[,c(-2)]

# Changing all relevant columns to factors
df$daytime <- as.factor(df$daytime)
df$file_records <- as.factor(df$file_records)
df$used_records <- as.factor(df$used_records)
df$qc_Tau <- as.factor(df$qc_Tau)
df$qc_H <- as.factor(df$qc_H)
df$qc_LE <- as.factor(df$qc_LE)
df$qc_co2_flux <- as.factor(df$qc_co2_flux)
df$qc_h2o_flux <- as.factor(df$qc_h2o_flux)
df$co2_def_timelag <- as.factor(df$co2_time_lag)
df$h2o_def_timelag <- as.factor(df$h2o_def_timelag)
df$spikes <- as.factor(df$spikes)
df$amplitude_resolution <- as.factor(df$amplitude_resolution)
df$drop_out <- as.factor(df$drop_out)
df$absolute_limits <- as.factor(df$absolute_limits)
df$skewness_kurtosis_sf <- as.factor(df$skewness_kurtosis_sf)
df$skewness_kurtosis_hf <- as.factor(df$skewness_kurtosis_hf)
df$discontinuities_sf <- as.factor(df$discontinuities_sf)
df$discontinuities_hf <- as.factor(df$discontinuities_hf)
df$timelag_sf <- as.factor(df$timelag_sf)
df$timelag_hf <- as.factor(df$timelag_hf)
df$attack_angle_hf <- as.factor(df$attack_angle_hf)
df$non_steady_wind_hf <- as.factor(df$non_steady_wind_hf)
df$model <- as.factor(df$model)

# Change all non-factors (or characters) to numeric)
df <- charactersNumeric(df)

# Convert TA_1_1_1, TS_1_1_1, and TS_2_1_1 from K to C
df$TA_1_1_1 <- df$TA_1_1_1 - 273.15
df$TS_1_1_1 <- df$TS_1_1_1 - 273.15
df$TS_2_1_1 <- df$TS_2_1_1 - 273.15

# Remove all improbable values of T
df$TA_1_1_1[which(df$TA_1_1_1 < 0 | df$TA_1_1_1 > 100 )] <- NA
df$TS_1_1_1[which(df$TS_1_1_1 < 0 )] <- NA
df$TS_2_1_1[which(df$TS_2_1_1 < 0 )] <- NA

# Correct TW_1_1_1 values using calibration equation from Mei Thung's experiment
df$TW_1_1_1 <- (df$TW_1_1_1 - 14.00) / 0.69

# Convert RN_1_1_1 to accurate values by dividing by the sensor sensitivity
# value of 13.6 uV/W m-2 but only for data before 2015-12-02 10:30:00
df$RN_1_1_1[which(df$time_stamp < "2015-12-02 10:30:00")] <- 
  df$RN_1_1_1[which(df$time_stamp < "2015-12-02 10:30:00")] / 13.6

# Change column name of (z-d)/L to Z.L
colnames(df)[which(colnames(df) == 'X.z.d..L')] <- 'Z.L'

# Delete temporary variables
rm(time_stamp,df_biomet)

# Obtain the indices to filter out the data for 180 < wind_dir < 270 
# to exclude data from land.
wind_check <- df$wind_dir <= 90 | df$wind_dir >= 270 # index to use flux data
wind_check_strict <- df$wind_dir <= 90 # stricter wind check 
df <- cbind(df,wind_check,wind_check_strict)
rm(wind_check)

#### Classifying into number of days ####
start <- as.numeric(df$time_stamp[1])
difference <- 86400 # 24 * 60 * 60 seconds

index = 1 # The number the days
day = numeric(length=nrow(df)) # Initialize day variable

for (i in 1:nrow(df)){
  # A failsafe if time_stamp is NA
  if(is.na(as.numeric(df$time_stamp[i]))){
    # Assigned the NA value as the day before
    day[i] <- index
  } else {
    if(as.numeric(df$time_stamp[i]) >= start & 
       as.numeric(df$time_stamp[i]) < (difference + start)){
      day[i] <- index
    } else {
      day[i] <- index
      index <- index + 1
      start <- difference + start
    }
  }
}

df <- cbind(df,day)
rm(start,difference,index,day)

#### Filter TS_1_1_1 values ####
# Create temporary TS_1_1_1 value
ts <- df$TS_1_1_1
# Standard dev of TS_1_1_1
ts_sd <- numeric(nrow(df))
# Mean of TS_1_1_1
ts_mean <- numeric(nrow(df))
# Level of standard deviation
level <- 0.5  # Just a very low bandwith filter to ensure that outside water
              # is removed
## Calculate standard deviation of T
# To count number of days
j <- 1
for (i in 1:nrow(df)){
  if(df$day[i] == j){
    ts_sd[i] <- sd(df$TS_1_1_1[which(df$day==j)], na.rm = TRUE)
    ts_mean[i] <- mean(df$TS_1_1_1[which(df$day==j)], na.rm = TRUE)
    temp_sd <- ts_sd[i]
    temp_mean <- ts_mean[i]
    j <- j + 1
  } else {
    ts_sd[i] <- temp_sd
    ts_mean[i] <- temp_mean
  }
}
# Remove all above water temperature readings by X level std. dev.
df$TS_1_1_1[which(ts < ts_mean - (level * ts_sd))] <- NA 
rm(i,j,level,temp_mean,temp_sd,ts,ts_mean,ts_sd)

#### Filter TS_2_1_1 values ####
# Create temporary TS_2_1_1 value
ts2 <- df$TS_2_1_1
# Standard dev of TS_2_1_1
ts2_sd <- numeric(nrow(df))
# Mean of TS_2_1_1
ts2_mean <- numeric(nrow(df))
# Level of standard deviation
level <- 0.5  # Just a very low bandwith filter to ensure that outside water
# is removed
## Calculate standard deviation of T
# To count number of days
j <- 1
for (i in 1:nrow(df)){
  if(df$day[i] == j){
    ts2_sd[i] <- sd(df$TS_2_1_1[which(df$day==j)], na.rm = TRUE)
    ts2_mean[i] <- mean(df$TS_2_1_1[which(df$day==j)], na.rm = TRUE)
    temp_sd2 <- ts2_sd[i]
    temp_mean2 <- ts2_mean[i]
    j <- j + 1
  } else {
    ts2_sd[i] <- temp_sd2
    ts2_mean[i] <- temp_mean2
  }
}
# Remove all above water temperature readings by X level std. dev.
df$TS_2_1_1[which(ts2 > ts2_mean + (level * ts2_sd))] <- NA 
rm(i,j,level,temp_mean2,temp_sd2,ts2,ts2_mean,ts2_sd)

#### Filter TW_1_1_1 values ####
# Create temporary TW_1_1_1 value
tw <- df$TW_1_1_1
# Standard dev of TW_1_1_1
tw2_sd <- numeric(nrow(df))
# Mean of TW_1_1_1
tw2_mean <- numeric(nrow(df))
# Level of standard deviation
level <- 20  # Just a very low bandwith filter to ensure that outside water
# is removed
## Calculate standard deviation of T
# To count number of days
j <- 1
for (i in 1:nrow(df)){
  if(df$day[i] == j){
    tw2_sd[i] <- sd(df$TW_1_1_1[which(df$day==j)], na.rm = TRUE)
    tw2_mean[i] <- mean(df$TW_1_1_1[which(df$day==j)], na.rm = TRUE)
    temp_sd2 <- tw2_sd[i]
    temp_mean2 <- tw2_mean[i]
    j <- j + 1
  } else {
    tw2_sd[i] <- temp_sd2
    tw2_mean[i] <- temp_mean2
  }
}
# Remove all above water temperature readings by X level std. dev.
df$TW_1_1_1[which(tw > tw2_mean + (level * tw2_sd))] <- NA 
rm(i,j,level,temp_mean2,temp_sd2,tw,tw2_mean,tw2_sd)

#### Filter RH_1_1_1 ambient RH ####
# Improbable values of RH
df$RH_1_1_1[which(df$RH_1_1_1 > 100 | df$RH_1_1_1 < 50)] <- NA

# Create temporary RH_1_1_1 value
RH <- df$RH_1_1_1
# Standard dev of RH_1_1_1
RH2_sd <- numeric(nrow(df))
# Mean of RH_1_1_1
RH2_mean <- numeric(nrow(df))
# Level of standard deviation
level <- 10  # Just a very low bandwith filter to ensure that outside water
# is removed
## Calculate standard deviation of T
# To count number of days
j <- 1
for (i in 1:nrow(df)){
  if(df$day[i] == j){
    RH2_sd[i] <- sd(df$RH_1_1_1[which(df$day==j)], na.rm = TRUE)
    RH2_mean[i] <- mean(df$RH_1_1_1[which(df$day==j)], na.rm = TRUE)
    temp_sd2 <- RH2_sd[i]
    temp_mean2 <- RH2_mean[i]
    j <- j + 1
  } else {
    RH2_sd[i] <- temp_sd2
    RH2_mean[i] <- temp_mean2
  }
}
# Remove all above water temperature readings by X level std. dev.
df$RH_1_1_1[which(RH > RH2_mean + (level * RH2_sd))] <- NA 
rm(i,j,level,temp_mean2,temp_sd2,RH,RH2_mean,RH2_sd)

#### Filter TA_1_1_1 ambient RH ####

# Create temporary TA_1_1_1 value
TA <- df$TA_1_1_1
# Standard dev of TA_1_1_1
TA2_sd <- numeric(nrow(df))
# Mean of RH_1_1_1
TA2_mean <- numeric(nrow(df))
# Level of standard deviation
level <- 1  # Just a very low bandwith filter to ensure that outside water
# is removed
## Calculate standard deviation of T
# To count number of days
j <- 1
for (i in 1:nrow(df)){
  if(df$day[i] == j){
    TA2_sd[i] <- sd(df$TA_1_1_1[which(df$day==j)], na.rm = TRUE)
    TA2_mean[i] <- mean(df$TA_1_1_1[which(df$day==j)], na.rm = TRUE)
    temp_sd2 <- TA2_sd[i]
    temp_mean2 <- TA2_mean[i]
    j <- j + 1
  } else {
    TA2_sd[i] <- temp_sd2
    TA2_mean[i] <- temp_mean2
  }
}
# Remove all above water temperature readings by X level std. dev.
df$TA_1_1_1[which(TA > TA2_mean + (level * TA2_sd))] <- NA 
rm(i,j,level,temp_mean2,temp_sd2,TA,TA2_mean,TA2_sd)

#### Filter RN_1_1_1 ####
df$RN_1_1_1[which(df$RN_1_1_1 > 1000)] <- NA

#### Calculate energy storage in water ####
# Only 3 heights including the water surface temperature
# These are estimated heights
# Level 1: water surface = 0.0001 m 
# Level 2: 2 m 
# Level 3: 3 m
heights <- c(1,2) 
# Calculating rho * cp for each level
rho <- 1025 # Density of sea water = 1020 to 1029 kg m-3
c_p <- 3850 # Specific heat capacity of sea water = 3850 J kg-1 C-1
rho_cp <- rho * c_p
rm(rho,c_p)

# Note: Remove water surface temperature contribution to storage
# due to uncertainty in the accuracy of the measurements
# Calculating the difference of rho * c_p * (T2 - T1) in time
# Level 1, 0.0001 m, water surface temperature
#rho_cp_dT1 <- numeric()
#for (i in 1:nrow(df)){
#  rho_cp_dT1[i] <- ((rho_cp*df$TW_1_1_1[i]) - 
#                      (rho_cp*df$TW_1_1_1[i-1]))/(30 * 60)
#}

# Level 2, 2 m
rho_cp_dT2 <- numeric()
for (i in 1:nrow(df)){
  rho_cp_dT2[i] <- ((rho_cp*df$TS_1_1_1[i]) - 
                      (rho_cp*df$TS_1_1_1[i-1]))/(30 * 60)
}

# Level 3, 5 m
rho_cp_dT3 <- numeric()
for (i in 1:nrow(df)){
  rho_cp_dT3[i] <- ((rho_cp*df$TS_2_1_1[i]) - 
                      (rho_cp*df$TS_2_1_1[i-1]))/(30 * 60)
}

# Integrating using the trapezium area rule
H_stor <- numeric()
for (i in 1:nrow(df)){
  H_stor[i] <- trapezium_intg_2(heights,rho_cp_dT2[i],rho_cp_dT3[i])
}

# Adding to df_EC
df <- cbind(df,H_stor)
rm(heights,i,rho_cp,rho_cp_dT2,rho_cp_dT3,H_stor)

#### Filter H_stor values ####

# Create temporary H_stor value
H_stor_filter <- df$H_stor
# Standard dev of H_stor
hstor_sd <- numeric(nrow(df))
# Mean of H_stor
hstor_mean <- numeric(nrow(df))

# Level of standard deviation
level <- 20 # A large bandwidth to ensure most of the data is not removed

## Calculate standard deviation of H_stor
# To count number of days
j <- 1
for (i in 1:nrow(df)){
  if(df$day[i] == j){
    hstor_sd[i] <- sd(df$H_stor[which(df$day==j)], na.rm = TRUE)
    hstor_mean[i] <- mean(df$H_stor[which(df$day==j)], na.rm = TRUE)
    hstor1_sd <- hstor_sd[i]
    hstor1_mean <- hstor_mean[i]
    j <- j + 1
  } else {
    hstor_sd[i] <- hstor1_sd
    hstor_mean[i] <- hstor1_mean
  }
}
# Remove all above water temperature readings by X level std. dev.
H_stor_filter[which(H_stor_filter < hstor_mean - (level * hstor_sd) | 
                      H_stor_filter > hstor_mean + (level * hstor_sd))] <- NA
df <- cbind(df,H_stor_filter)
rm(i,j,level,hstor_mean,hstor_sd,H_stor_filter,hstor1_mean,hstor1_sd)

#### Remove EC data if wind_check fails (= 0) ####
# Note temporary removal
#df$H[which(df$wind_check == 0)] <- NA
#df$LE[which(df$wind_check == 0)] <- NA
#df$co2_flux[which(df$wind_check == 0)] <- NA
#df$Z.L[which(df$wind_check == 0)] <- NA

# Export data
#write.table(df,'data/df1.csv',sep=',')
