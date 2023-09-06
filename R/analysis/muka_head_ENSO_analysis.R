##############################################################
# TITLE: Coastal eddy covariance and meteorology data analysis
# 
#
#
#
##############################################################

#### 0. Preliminaries ####
library(openair)
library(Hmisc)
library(dplyr)
source('R/tools/vap_pres_Buck.R')

#### El-Nino-Southern Oscillation classification ####
# Note: if before 2016-06-01 00:00:00 then ENSO and if not then non-ENSO

enso <- df$time_stamp < as.POSIXct('2016-06-01 00:00:00', 
                                   '%Y-%m-%d %H:%M:%S', 
                                   tz = 'Asia/Kuala_Lumpur')
#### Calculate ea #####

ea <- vap_pres_Buck(df$TA_1_1_1, df$RH_1_1_1/100)/10
# Remove bad maximum ea
ea[which.max(ea)] <- NA
df <- cbind(df,ea)

#### Month and year classification ####
Month_class <- NA 
for (i in 1:nrow(df)) {
  if (df$time_stamp[i] < as.POSIXct('2015-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Nov2015'
  }
  if (df$time_stamp[i] > as.POSIXct('2015-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2015-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Dec2015'
  }
  if (df$time_stamp[i] > as.POSIXct('2015-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-01-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jan2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-01-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-02-29 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Feb2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-02-29 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-03-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Mar2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-03-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-04-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Apr2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-04-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-05-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'May2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-05-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-06-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jun2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-06-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-07-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jul2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-07-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-08-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Aug2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-08-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-09-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Sep2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-09-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-10-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Oct2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-10-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Nov2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Dec2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-01-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jan2017'
  }
}
Month_class <- as.factor(Month_class)

df <- cbind(df,Month_class)
df$Month_class <- factor(df$Month_class, levels = c('Nov2015', 'Dec2015',
                                                    'Jan2016', 'Feb2016',
                                                    'Mar2016', 'Apr2016',
                                                    'May2016', 'Jun2016',
                                                    'Jul2016', 'Aug2016',
                                                    'Sep2016', 'Oct2016',
                                                    'Nov2016', 'Dec2016',
                                                    'Jan2017'))

#### Import weather data from IPENANGP2 for precipitation (mm) ####
weather <- read.csv('data/ipenangp2_weather_data2.csv', header = TRUE)
precip <- weather$HourlyPrecipMM
date <- weather$Time
date <- as.POSIXct(date, '%Y-%m-%d %H:%M:%S', tz = 'Asia/Kuala_Lumpur')
rain <- data.frame(date,precip)
rainHR <- timeAverage(rain, avg.time = '30 min', statistic = 'mean')
# Merge precipitation data with EC data
df$time_stamp<-as.POSIXct(df$time_stamp)
df1 <- merge(df, rainHR, by.x = 'time_stamp', by.y = 'date')
# Check if rain
rain_index <- df1$precip > 0
rain_index[is.na(rain_index)] <- FALSE
rm(rain, rainHR, weather, precip,date)

#### Indices for usable fluxes ####
# An index for co2 of qc = 1, 2 and wind_check_strict = TRUE and rain_index 
indexCO2 <- (df$qc_co2_flux==1 | df$qc_co2_flux == 0) & 
  df$wind_check_strict & rain_index != TRUE 
# Another index for co2 for -5 < co2 < 5
indexCO2_5 <- (df$qc_co2_flux==1 | df$qc_co2_flux == 0) & 
  df$wind_check_strict & rain_index != TRUE & df$co2_flux > -5 & df$co2_flux < 5
indexCO2_5_enso <- indexCO2_5 == TRUE & enso == TRUE
indexCO2_5_xenso <- indexCO2_5 == TRUE & enso == FALSE
# An index for LE of qc = 1, 2 and wind_check_strict = TRUE and rain_index 
indexLE <- (df$qc_LE==1 | df$qc_LE == 0) & 
  df$wind_check_strict & rain_index != TRUE
# An index for H of qc = 1, 2 and wind_check_strict = TRUE and rain_index 
indexH <- (df$qc_H==1 | df$qc_H == 0) & 
  df$wind_check_strict & rain_index != TRUE

# For the current analysis, had to remove all data between rows 15857 to 16898
# df$RN_1_1_1[15857:16960] <- NA
# df$RG_1_1_1[15857:16960] <- NA
# df$TA_1_1_1[15857:16960] <- NA
# df$RH_1_1_1[15857:16960] <- NA
# df$TS_1_1_1[15857:16960] <- NA
# df$TS_2_1_1[15857:16960] <- NA
# df$TW_1_1_1[15858:16960] <- NA

# Using df_20161023a remove a portion of TS_1_1_1 values
# df$TS_1_1_1[14850:14983] <- NA

# Remove improbable wind values
df$wind_speed[which(df$wind_speed > 5)] <- NA

#### Filter TA due to error ####
df$TA_1_1_1[19057:19166] <- NA
df$TS_2_1_1[19057:19200] <- NA

df$TS_1_1_1[which(df$TS_1_1_1 > 40)] <- NA
df$TS_1_1_1[19100:19155] <- NA
df$TS_1_1_1[15000:15080] <- NA

#### Diurnal grouping ####
### Preparation of data ###
# Grouping into hours for all data
# Note: since the update have to create separate groups for mean and sd.
# Need to create another dataframe that only contains usable flux

df_grp_mean <- df %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(co2_flux=mean(co2_flux[which(indexCO2_5 == TRUE)],na.rm=TRUE),
            LE=mean(LE,na.rm=TRUE),
            H=mean(H,na.rm=TRUE),
            WS=mean(wind_speed,na.rm=TRUE),
            WD=mean(wind_dir,na.rm=TRUE),
            zL=mean(Z.L,na.rm=TRUE),
            H_stor=mean(H_stor,na.rm=TRUE),
            H_stor_filter=mean(H_stor_filter,na.rm=TRUE),
            RG=mean(RG_1_1_1,na.rm=TRUE),
            RN=mean(RN_1_1_1,na.rm=TRUE),
            TW1=mean(TS_1_1_1,na.rm=TRUE),
            TW2=mean(TS_2_1_1,na.rm=TRUE),
            TA=mean(TA_1_1_1,na.rm=TRUE),
            RH=mean(RH_1_1_1,na.rm=TRUE),
            TSW=mean(TW_1_1_1,na.rm=TRUE))
df_grp_sd <- df %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(co2_flux_sd=sd(co2_flux[which(indexCO2_5 == TRUE)],na.rm=TRUE),
            LE_sd=sd(LE,na.rm=TRUE),
            H_sd=sd(H,na.rm=TRUE),
            WS_sd=sd(wind_speed,na.rm=TRUE),
            WD_sd=sd(wind_dir,na.rm=TRUE),
            zL_sd=sd(Z.L,na.rm=TRUE),
            H_stor_sd=sd(H_stor,na.rm=TRUE),
            H_stor_filter_sd=sd(H_stor_filter,na.rm=TRUE),
            RG_sd=sd(RG_1_1_1,na.rm=TRUE),
            RN_sd=sd(RN_1_1_1,na.rm=TRUE),
            TW1_sd=sd(TS_1_1_1,na.rm=TRUE),
            TW2_sd=sd(TS_2_1_1,na.rm=TRUE),
            TA_sd=sd(TA_1_1_1,na.rm=TRUE),
            RH_sd=sd(RH_1_1_1,na.rm=TRUE),
            TSW_sd=sd(TW_1_1_1,na.rm=TRUE))
# Merge the two dataframe
df_grp <- merge(df_grp_mean,df_grp_sd,by='hour')
rm(df_grp_sd,df_grp_mean)

#### Monthly grouping ####
### Preparation of data ###
# Grouping into months for all data
# Note: since the update have to create separate groups for mean and sd.
# Need to create another dataframe that only contains usable flux

df_grp_mean <- df %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(year=format(as.POSIXlt(cut(time_stamp,breaks='year')),'%Y'),
           month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%m')) %>%
  summarise(co2_flux=mean(co2_flux[which(indexCO2_5 == TRUE)],na.rm=TRUE),
            LE=mean(LE,na.rm=TRUE),
            H=mean(H,na.rm=TRUE),
            WS=mean(wind_speed,na.rm=TRUE),
            WD=mean(wind_dir,na.rm=TRUE),
            zL=mean(Z.L,na.rm=TRUE),
            H_stor=mean(H_stor,na.rm=TRUE),
            H_stor_filter=mean(H_stor_filter,na.rm=TRUE),
            RG=mean(RG_1_1_1,na.rm=TRUE),
            RN=mean(RN_1_1_1,na.rm=TRUE),
            TW1=mean(TS_1_1_1,na.rm=TRUE),
            TW2=mean(TS_2_1_1,na.rm=TRUE),
            TA=mean(TA_1_1_1,na.rm=TRUE),
            RH=mean(RH_1_1_1,na.rm=TRUE),
            TSW=mean(TW_1_1_1,na.rm=TRUE))
df_grp_sd <- df %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(year=format(as.POSIXlt(cut(time_stamp,breaks='year')),'%Y'),
           month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%m')) %>%
  summarise(co2_flux_sd=sd(co2_flux[which(indexCO2_5 == TRUE)],na.rm=TRUE),
            LE_sd=sd(LE,na.rm=TRUE),
            H_sd=sd(H,na.rm=TRUE),
            WS_sd=sd(wind_speed,na.rm=TRUE),
            WD_sd=sd(wind_dir,na.rm=TRUE),
            zL_sd=sd(Z.L,na.rm=TRUE),
            H_stor_sd=sd(H_stor,na.rm=TRUE),
            H_stor_filter_sd=sd(H_stor_filter,na.rm=TRUE),
            RG_sd=sd(RG_1_1_1,na.rm=TRUE),
            RN_sd=sd(RN_1_1_1,na.rm=TRUE),
            TW1_sd=sd(TS_1_1_1,na.rm=TRUE),
            TW2_sd=sd(TS_2_1_1,na.rm=TRUE),
            TA_sd=sd(TA_1_1_1,na.rm=TRUE),
            RH_sd=sd(RH_1_1_1,na.rm=TRUE),
            TSW_sd=sd(TW_1_1_1,na.rm=TRUE))
# Merge the two dataframe
df_grp_month <- merge(df_grp_mean,df_grp_sd,by=c('year','month'))
rm(df_grp_sd,df_grp_mean)


#### Mean values of fluxes ####
# mean and std dev CO2 flux
mean(df$co2_flux[which(indexCO2_5 == TRUE)], na.rm = TRUE)
sd(df$co2_flux[which(indexCO2_5 == TRUE)], na.rm = TRUE)

# mean co2 flux during ENSO
mean(df$co2_flux[which(indexCO2_5_enso == TRUE)], na.rm = TRUE)
# mean co2 flux after ENSO
mean(df$co2_flux[which(indexCO2_5_xenso == TRUE)], na.rm = TRUE)
t.test(df$co2_flux[which(indexCO2_5_enso == TRUE)],
       df$co2_flux[which(indexCO2_5_xenso == TRUE)])



# mean LE flux during ENSO
mean(df$LE[which(indexLE == TRUE & enso == TRUE)], na.rm = TRUE)
# mean LE after ENSO
mean(df$LE[which(indexLE == TRUE & enso == FALSE)], na.rm = TRUE)
t.test(df$LE[which(indexLE == TRUE & enso == TRUE)],
       df$LE[which(indexLE == TRUE & enso == FALSE)])

# mean H flux during ENSO
mean(df$H[which(indexH == TRUE & enso == TRUE)], na.rm = TRUE)
# mean LE after ENSO
mean(df$H[which(indexH == TRUE & enso == FALSE)], na.rm = TRUE)
t.test(df$H[which(indexH == TRUE & enso == TRUE)],
       df$H[which(indexH == TRUE & enso == FALSE)])


#### Mean values of met parameters ####
# mean net radiation during ENSO
mean(df$RN_1_1_1[which(enso == TRUE)], na.rm = TRUE)
# mean co2 flux after ENSO
mean(df$RN_1_1_1[which(enso == FALSE)], na.rm = TRUE)
t.test(df$RN_1_1_1[which(enso == TRUE)], 
       df$RN_1_1_1[which(enso == FALSE)])

# mean global radiation during ENSO
mean(df$RG_1_1_1[which(enso == TRUE)], na.rm = TRUE)
# mean co2 flux after ENSO
mean(df$RG_1_1_1[which(enso == FALSE)], na.rm = TRUE)
t.test(df$RG_1_1_1[which(enso == TRUE)], 
       df$RG_1_1_1[which(enso == FALSE)])

# mean SST flux during ENSO
mean(df$TW_1_1_1[which(enso == TRUE)], na.rm = TRUE)
# mean SST flux after ENSO
mean(df$TW_1_1_1[which(enso == FALSE)], na.rm = TRUE)
hist(df$TW_1_1_1[which(enso == TRUE)])
hist(df$TW_1_1_1[which(enso == FALSE)])
t.test(df$TW_1_1_1[which(enso == TRUE)],df$TW_1_1_1[which(enso == FALSE)])

# mean atm T flux during ENSO
mean(df$TA_1_1_1[which(enso == TRUE)], na.rm = TRUE)
# mean SST flux after ENSO
mean(df$TA_1_1_1[which(enso == FALSE)], na.rm = TRUE)
t.test(df$TA_1_1_1[which(enso == TRUE)],df$TA_1_1_1[which(enso == FALSE)])

# mean ea during ENSO
mean(df$ea[which(enso == TRUE)], na.rm = TRUE)
# mean ea after ENSO
mean(df$ea[which(enso == FALSE)], na.rm = TRUE)
t.test(df$ea[which(enso == TRUE)], df$ea[which(enso == FALSE)])


# mean TS_1 during ENSO
# Remove TS_1 > 50 
#TS1 <- df$TS_1_1_1
#TS1[which(TS1 > 38)] <- NA
#df$TS_1_1_1 <- TS1

# mean TS_1 during ENSO
mean(df$TS_1_1_1[which(enso == TRUE)], na.rm = TRUE)
# mean TS_1 after ENSO
mean(df$TS_1_1_1[which(enso == FALSE)], na.rm = TRUE)
hist(df$TS_1_1_1[which(enso == TRUE)])
hist(df$TS_1_1_1[which(enso == FALSE)])

t.test(df$TS_1_1_1[which(enso == TRUE)],df$TS_1_1_1[which(enso == FALSE)])

# mean TS_2 during ENSO
mean(df$TS_2_1_1[which(enso == TRUE)], na.rm = TRUE)
# mean TS_2 after ENSO
mean(df$TS_2_1_1[which(enso == FALSE)], na.rm = TRUE)
t.test(df$TS_2_1_1[which(enso == TRUE)],df$TS_2_1_1[which(enso == FALSE)])

# mean WS during ENSO
mean(df$wind_speed[which(enso == TRUE)], na.rm = TRUE)
# mean WS during non-ENSO
mean(df$wind_speed[which(enso == FALSE)], na.rm = TRUE)
t.test(log(df$wind_speed[which(enso == TRUE)]),
       log(df$wind_speed[which(enso == FALSE)]))

# mean WD during ENSO
mean(df$wind_dir[which(enso == TRUE)], na.rm = TRUE)
# mean WS during non-ENSO
mean(df$wind_dir[which(enso == FALSE)], na.rm = TRUE)
t.test(df$wind_dir[which(enso == TRUE)],
       df$wind_dir[which(enso == FALSE)])

# Precipitation
mean(df$precip[which(enso == TRUE)], na.rm = TRUE)
mean(df$precip[which(enso == FALSE)], na.rm = TRUE)
hist(log(df$precip[which(enso == TRUE)]))
hist(log(df$precip[which(enso == FALSE)]))
t.test(log(df$precip[which(enso == TRUE)]),
       log(df$precip[which(enso == FALSE)]))
plot(df$time_stamp[which(enso == TRUE)], df$precip[which(enso == TRUE)],
     type = 'l', ylim = c(0,40))
plot(df$time_stamp[which(enso == FALSE)], df$precip[which(enso == FALSE)],
     type = 'l', ylim = c(0,40))

# Some plots
plot(df$time_stamp[which(indexCO2_5_enso == TRUE)],
     df$co2_flux[which(indexCO2_5_enso == TRUE)], type = 'l',
     ylim = c(-10,10))
plot(df$time_stamp[which(indexCO2_5_xenso == TRUE)], 
      df$co2_flux[which(indexCO2_5_xenso == TRUE)], type = 'l',
     col = 'red', ylim = c(-10,10))
#boxp1 <- boxplot(df$co2_flux[which(index == TRUE & enso == TRUE)])




#### Atm T and RH plots ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/met.jpg')
jpeg(file=path_fig,width=16,height=16,res=400, units = 'cm')
par(family='Times', oma=c(3.1,0.1,0.1,0.1), mfrow = c(3,1))

par(mai=c(0.05,0.6,0.05, 0.65))
plot(df$time_stamp, df$TA_1_1_1, type = 'l', 
     ylab = '', xlab = '', ylim = c(22, 36),
     col = 'red', xaxt = 'n', cex.axis = 1.5)
minor.tick(ny = 1)
# axis(side = 1, at = c(as.POSIXct('2015-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#                       as.POSIXct('2016-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#                       as.POSIXct('2016-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#                       as.POSIXct('2016-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#                       as.POSIXct('2016-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#                       as.POSIXct('2016-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
#      labels = c('Dec', 'Feb', 'Apr', 'Jun', 'Aug', 'Oct'))
mtext(side = 2, expression('T'['A']), line = 2.5, cex = 1.1)
mtext(side = 4, expression('e'['a']), line = 2.7, cex = 1.1)
legend(as.POSIXct('2015-10-30 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
       24, bty = 'n', lwd = 2, col = 'red', text.col = 'red',
       legend = expression('T'['A']), cex = 1.5)
legend(as.POSIXct('2016-01-15 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
       24, bty = 'n', lwd = 2, col = 'blue', text.col = 'blue',
       legend = expression('e'['a']), cex = 1.5)
par(new = TRUE)
plot(df$time_stamp, df$ea, type = 'l',
     axes = FALSE, xlab = '', ylab = '', col = 'blue', ylim = c(1.5,3.7))
lines(c(as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
        as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
      c(0,120), lwd = 3, lty = 2)
axis(4, cex.axis = 1.5)


#### Wind speed plots ###
#ws <- df$wind_speed
#ws[which(df$wind_speed > 5)] <- NA
#path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/wind_sp.jpg')
#jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
#par(family='Times', oma=c(0.0001,0.0001,0.0001,0.0001))
par(mai=c(0.05,0.6,0.05, 0.65))
plot(df$time_stamp, df$wind_speed, type = 'l', xlab = '', ylab = '', col = 'grey40',
     xaxt = 'n', cex.axis = 1.5)
minor.tick(ny = 1)
# axis(side = 1, at = c(as.POSIXct('2015-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#                       as.POSIXct('2016-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#                       as.POSIXct('2016-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#                       as.POSIXct('2016-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#                       as.POSIXct('2016-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#                       as.POSIXct('2016-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
#      labels = c('Dec', 'Feb', 'Apr', 'Jun', 'Aug', 'Oct'))
lines(c(as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
        as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
      c(0,120), lwd = 3, lty = 2)
mtext(side = 2, 'U', line = 2.5, cex = 1.1)


#### Temperature plots ###
#path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/water_temp.jpg')
#jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
#par(family='Times', oma=c(0.0001,0.001,0.0001,0.0001))
par(mai=c(0.05,0.6,0.05, 0.65))
#plot(df$time_stamp, df$TW_1_1_1, type = 'l', 
#     ylab = '', xlab = '', ylim = c(25, 65),
#     col = 'red', cex.axis = 1.5)
#minor.tick(nx = 1)
#par(new = TRUE)
plot(df$time_stamp, df$TS_1_1_1, type = 'l',
     col = 'darkgreen', ylab = '', xlab = '', ylim = c(26, 34),
     cex.axis = 1.5)
text(as.POSIXct('2016-03-15 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 26.5,
     'ENSO', cex = 1.5)
text(as.POSIXct('2016-08-15 00:00:00', format = '%Y-%m-%d %H:%M:%S'), 26.5,
     'non-ENSO', cex = 1.5)

mtext(side = 2, expression('T'['S']), line = 2.5, cex = 1.1) 
mtext(side = 1, 'Month', line = 2.5, cex = 1.1)
axis(side = 1, at = c(as.POSIXct('2015-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
     labels = c('Dec', 'Feb', 'Apr', 'Jun', 'Aug', 'Oct','Dec'), cex.axis = 1.5)
#legend(as.POSIXct('2015-10-26 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
#       69, bty = 'n', lwd = 2, col = 'red', text.col = 'red',
#       legend = expression('T'['WS']), cex = 1.5)
legend(as.POSIXct('2015-10-26 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
       34.5, bty = 'n', lwd = 2, col = 'darkgreen', text.col = 'darkgreen',
       legend = expression('T'['S1']), cex = 1.5)
legend(as.POSIXct('2016-01-5 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
       34.5, bty = 'n', lwd = 2, col = 'blue', text.col = 'blue',
       legend = expression('T'['S2']), cex = 1.5)
#axis(4, cex.axis = 1.5)
#mtext(side = 4, expression('T'['S']), line = 2.7, cex = 1.1)
par(new = TRUE)
plot(df$time_stamp, df$TS_2_1_1, type = 'l',
     col = 'blue', ylab = '', xlab = '', ylim = c(26,34), 
     axes = FALSE)
lines(c(as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
        as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
      c(20,80), lwd = 3, lty = 2)
dev.off()

#### Wind rose ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/wind_rose.jpg')
jpeg(file=path_fig,width=8,height=8,res=400, units = 'cm', family = 'serif')
par(mai = c(0.7,0.05,0.05, 0.1))
windRose(df, ws = 'wind_speed', wd = 'wind_dir', paddle = FALSE,
         par.settings=list(fontsize=list(text=8)), angle.scale = 45)
dev.off()

#### CO2 flux time series ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/co2_flux_all.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='Times', mar = c(4.1, 4.1, 0.1, 0.1))

plot(df$time_stamp[which(indexCO2_5 == TRUE)],
     df$co2_flux[which(indexCO2_5 == TRUE)], 
     xlab = '', ylab = '',
     type = 'l', cex.axis = 1, yaxt = 'n')
mtext(side = 1, 'Month', line = 2.5, cex = 1.5)
mtext(side = 2, expression(paste('CO'['2'],' flux')), 
      line = 2.1, cex = 1.5)
lines(c(as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
        as.POSIXct('2016-05-31 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
      c(-10,10), lwd = 3, lty = 2)
text(as.POSIXct('2016-03-15 00:00:00', format = '%Y-%m-%d %H:%M:%S'), -4.3,
     'ENSO', cex = 1.5)
text(as.POSIXct('2016-08-15 00:00:00', format = '%Y-%m-%d %H:%M:%S'), -4.3,
     'non-ENSO', cex = 1.5)
axis(side = 1, at = c(as.POSIXct('2015-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
     labels = c('Dec', 'Feb', 'Apr', 'Jun', 'Aug', 'Oct','Dec'), cex.axis = 1)
axis(side = 2, at = c(-4,-2,0,2,4), 
     labels = c(paste('\u2212',4,sep=''),paste('\u2212',2,sep=''),0,2,4))
minor.tick(nx = 1)
dev.off()

#### Boxplots for CO2 between ENSO and non-ENSO ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/co2boxplot.jpg')
jpeg(file=path_fig,width=8,height=8,res=400, units = 'cm')
par(family='Times')
par(mar = c(2.1,4.1,0.1, 0.1))
boxplot(df$co2_flux[which(indexCO2_5_enso == TRUE)],
        df$co2_flux[which(indexCO2_5_xenso == TRUE)], outline = FALSE,
        names = c('ENSO','non-ENSO'), 
        ylab = '', cex.lab = 1.2, yaxt = 'n',
        cex.axis = 1.2, ylim = c(-0.5,0.5))
mtext(side = 2, expression(paste('CO'['2'],' flux')), 
      line = 2.1, cex = 1.2)
axis(side = 2, at = c(-0.4,-0.2,0,0.2,0.4),
     labels = c(paste('\u2212',0.4,sep=''),
                paste('\u2212',0.2,sep=''),
                0,0.2,0.4))
dev.off()


#### Overall diurnal CO2 flux plot ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/co2_diurnal.jpg')
jpeg(file=path_fig,width=8,height=8,res=400, units = 'cm')
par(family='Times')
par(mar = c(3.1,3.4,0.1, 0.1))
plot(df_grp$hour, df_grp$co2_flux, type = 'l', ylim = c(-1,1.5), xlim = c(-1,25),
     xlab = '', ylab = '', xaxt = 'n')
mtext(side = 1, 'Hour (local time)', line = 2.1, cex = 1.2)
mtext(side = 2, expression(paste('CO'['2'],' flux')), 
      line = 2.1, cex = 1.2)
axis(side = 1, at = c(0,3,6,9,12,18,24), labels = c(0,3,6,9,12,18,24))
axis(side = 1, at = c(15,21), labels = c(15,21))

#hour <- df_grp$hour
#co2_down <- df_grp$co2_flux - df_grp$co2_flux_sd
#co2_up <- df_grp$co2_flux + df_grp$co2_flux_sd
#polygon(c(hour, rev(hour)), c(co2_up, rev(co2_down)),
#        col=adjustcolor("grey",alpha.f=0.5), border = NA)
dev.off()

#### Correlational analysis and plots between CO2 and physical drivers ####
# Sea surface temperature
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/SSTandUcor.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='Times', mfrow = c(1,2))
par(mar = c(3,3,0.5,0))
plot(df$TS_1_1_1[which(indexCO2_5_enso == TRUE)],
     df$co2_flux[which(indexCO2_5_enso == TRUE)], pch = 16,
     xlab = '', ylab = '',
     xlim= c(26,32), ylim = c(-1,1), col = alpha('red',0.2), yaxt = 'n')
points(df$TS_1_1_1[which(indexCO2_5_xenso == TRUE)],
       df$co2_flux[which(indexCO2_5_xenso == TRUE)], pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-1,-0.5,0,0.5,1), 
     labels = c(paste('\u2212',1,sep=''),paste('\u2212',0.5,sep=''),0,0.5,1))
minor.tick()
mtext(expression(paste('CO'['2'],' flux')),side = 2,line=2)
mtext(expression('T'['S']),side = 1,line=2)
lmT <- lm(df$co2_flux[which(indexCO2_5 == TRUE)] ~ 
            df$TS_1_1_1[which(indexCO2_5 == TRUE)])
lmTenso <- lm(df$co2_flux[which(indexCO2_5_enso == TRUE)] ~ 
            df$TS_1_1_1[which(indexCO2_5_enso == TRUE)])
lmTxenso <- lm(df$co2_flux[which(indexCO2_5_xenso == TRUE)] ~ 
                df$TS_1_1_1[which(indexCO2_5_xenso == TRUE)])
abline(lmTenso, col = 'red', lwd = 3, lty = 2)
abline(lmTxenso, col = 'blue', lwd = 3, lty = 2)
abline(lmT, col = 'black', lwd = 3, lty = 2)
summary(lmT)
summary(lmTenso)
summary(lmTxenso)


# Wind speed
# path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/Ucor.jpg')
# jpeg(file=path_fig,width=8,height=8,res=400, units = 'cm')
# par(family='Times')
par(mar = c(3,2.5,0.5,0.5))
plot(df$wind_speed[which(indexCO2_5_enso == TRUE)],
     df$co2_flux[which(indexCO2_5_enso == TRUE)], pch = 16,
     xlab = '', ylab = '',
     xlim= c(0,4.5), ylim = c(-1,1), col = alpha('red',0.2),yaxt = 'n')
# points(df$wind_speed[which(indexCO2_5_enso == TRUE)],
#        df$co2_flux[which(indexCO2_5_enso == TRUE)], pch = 16,
#        col = alpha('red',0.2))
points(df$wind_speed[which(indexCO2_5_xenso == TRUE)],
       df$co2_flux[which(indexCO2_5_xenso == TRUE)], pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-1,-0.5,0,0.5,1), 
     labels = c(paste('\u2212',1,sep=''),paste('\u2212',0.5,sep=''),0,0.5,1))
#mtext(expression(paste('CO'['2'],' flux')),side = 2,line=2)
mtext('U',side = 1,line=2)
minor.tick()
lmU <- lm(df$co2_flux[which(indexCO2_5 == TRUE)] ~ 
            df$wind_speed[which(indexCO2_5 == TRUE)])
lmUenso <- lm(df$co2_flux[which(indexCO2_5_enso == TRUE)] ~ 
                df$wind_speed[which(indexCO2_5_enso == TRUE)])
lmUxenso <- lm(df$co2_flux[which(indexCO2_5_xenso == TRUE)] ~ 
                 df$wind_speed[which(indexCO2_5_xenso == TRUE)])
abline(lmUenso, col = 'red', lwd = 3, lty = 2)
abline(lmUxenso, col = 'blue', lwd = 3, lty = 2)
abline(lmU, col = 'black', lwd = 3, lty = 2)
summary(lmU)
summary(lmUenso)
summary(lmUxenso)
dev.off()

# Net radiation
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/RNcor.jpg')
jpeg(file=path_fig,width=8,height=8,res=400, units = 'cm')
par(family='Times')
par(mar = c(4.1,4.1,0.5, 0.5))
plot(df$RN_1_1_1[which(indexCO2_5 == TRUE)],
     df$co2_flux[which(indexCO2_5 == TRUE)], pch = 19,
     xlab = 'RN', ylab = expression(paste('CO'['2'],' flux')),
     xlim= c(-100,300), ylim = c(-1,1), col = 'grey60')
axis(side = 2, at = c(-0.2,0), labels = c(-0.2,0))
minor.tick()
lmRN <- lm(df$co2_flux[which(indexCO2_5 == TRUE)] ~ 
            df$RN_1_1_1[which(indexCO2_5 == TRUE)])
lmRNenso <- lm(df$co2_flux[which(indexCO2_5_enso == TRUE)] ~ 
                df$RN_1_1_1[which(indexCO2_5_enso == TRUE)])
lmRNxenso <- lm(df$co2_flux[which(indexCO2_5_xenso == TRUE)] ~ 
                 df$RN_1_1_1[which(indexCO2_5_xenso == TRUE)])
abline(lmRN, col = 'green', lwd = 3, lty = 2)
abline(lmRNenso, col = 'red', lwd = 3, lty = 2)
abline(lmRNxenso, col = 'blue', lwd = 3, lty = 2)
summary(lmRN)
summary(lmRNenso)
summary(lmRNxenso)
dev.off()








#### Density plots for CO2 between ENSO and non-ENSO ####
co2_enso <- density(df$co2_flux[which(indexCO2_5_enso == TRUE)])
co2_xenso <- density(df$co2_flux[which(indexCO2_5_xenso == TRUE)])
plot(co2_enso, ylim = c(0, 7))
lines(co2_xenso, col = 'red')

#### RN and RG ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/RN_RG.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='Times')
par(mai = c(0.7,0.6,0.05, 0.7))
plot(df$time_stamp, df$RG_1_1_1, type = 'l', 
     ylab = '', xlab = '', #ylim = c(22, 36),
     col = 'red')
axis(side = 1, at = c(as.POSIXct('2015-12-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-02-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-04-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-06-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-08-01 00:00:00', format = '%Y-%m-%d %H:%M:%S'),
                      as.POSIXct('2016-10-01 00:00:00', format = '%Y-%m-%d %H:%M:%S')),
     labels = c('Dec', 'Feb', 'Apr', 'Jun', 'Aug', 'Oct'))
par(new = TRUE)
plot(df$time_stamp, df$RN_1_1_1, type = 'l',
     ylab = '', xlab = '', 
     col = 'orange')
dev.off()

#### Monthly boxplots of CO2 fluxes ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/co2_box.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='Times', mar = c(4.1, 4.1, 0.1, 0.1))
boxplot(df$co2_flux[indexCO2_5] ~ df$Month_class[indexCO2_5], 
        outline = F, yaxt = 'n',names = c('','Dec', '2016','Feb','',
                               'Apr','','Jun','','Aug','',
                               'Oct','','Dec','2017'))
axis(side = 1, at = c(3,15), labels = c('2016','2017'))
axis(side = 2, at = c(-0.4,-0.2,0,0.2,0.4), 
     labels = c(paste('\u2212',0.4,sep=''),paste('\u2212',0.2,sep=''),0,0.2,0.4))
mtext(side = 1, 'Month', line = 2.5, cex = 1.5)
mtext(side = 2, expression(paste('CO'['2'],' flux')), 
      line = 2.1, cex = 1.5)
lines(c(8,8),c(-1,1), lwd = 3, lty = 2)
minor.tick(nx=0)
dev.off()

boxplot(df$TS_2_1_1 ~ df$Month_class)
boxplot(df$wind_speed ~ df$Month_class)
boxplot(df$u.[indexCO2_5] ~ df$Month_class[indexCO2_5])

boxplot(df$precip~df$Month_class, outline = F)
lines(c(8,8),c(-1,12), lwd = 3, lty = 2)


#### Monthly boxplots of U fluxes ####
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/U_box.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='Times', mar = c(4.1, 4.1, 0.1, 0.1))
boxplot(df$wind_speed[indexCO2_5] ~ df$Month_class[indexCO2_5], 
        outline = F, names = c('','Dec', '2016','Feb','',
                                          'Apr','','Jun','','Aug','',
                                          'Oct','','Dec','2017'))
axis(side = 1, at = c(3,15), labels = c('2016','2017'))
mtext(side = 2, 'U', 
      line = 2.1, cex = 1.5)
lines(c(8,8),c(-1,5), lwd = 3, lty = 2)
minor.tick(nx=0)
dev.off()



#### Correlational analysis of monthly averages ####
plot(df_grp_month$TW2, df_grp_month$co2_flux,pch=19)
lm_co2_ts_month <- lm(df_grp_month$co2_flux ~ df_grp_month$TW2)
summary(lm_co2_ts_month)

plot(df_grp_month$WS, df_grp_month$co2_flux, pch =19)
lm_co2_U_month <- lm(df_grp_month$co2_flux ~ df_grp_month$WS)
summary(lm_co2_U_month)

#### Fitting U to CO2 flux ####

co2fluxfit <- df$co2_flux[indexCO2_5]
Ufit <- df$wind_speed[indexCO2_5]

# Convert to natural log scale to fit
ln_co2 <- log(co2fluxfit)
ln_u <- log(Ufit)

# Plot using ln scale
plot(ln_u,ln_co2)
lm_fit <- lm(ln_co2 ~ Ufit)
summary(lm_fit)
abline(lm_fit, col ='red', lwd = 2)

# Plot the fit using normal scale
plot(Ufit, co2fluxfit, pch = 19)
xfit <- seq(0,5,0.1)
yfit <- (17.3/10000) * xfit^-0.19
lines(xfit, yfit, col = 'red')