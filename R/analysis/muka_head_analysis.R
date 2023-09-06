##############################################################
# TITLE: Coastal eddy covariance and meteorology data analysis
# 
#
#
#
##############################################################

#### 1. Preliminaries ####

library(openair)
library(Hmisc)
library(dplyr)



# Saving old plotting parameters
#old.par <- par()

#### 2. Analysis and plots ####

# This step is needed if the dataframes are named other than 'df'
# Just need to change the name of df here and the rest of the script
# would be the same.
df <- df

#### El-Nino-Southern Oscillation classification ####
# Note: if before 2016-06-01 00:00:00 then ENSO and if not then non-ENSO

enso <- df$time_stamp < as.POSIXct('2016-06-01 00:00:00', 
                                   '%Y-%m-%d %H:%M:%S', 
                                   tz = 'Asia/Kuala_Lumpur')

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

#### Temperature and relative humidity ####
# Plot temperature, T [deg C]
plot(df$time_stamp,df$TA_1_1_1,type='l',xlab='Date',ylab='T',lwd=2,
     main='T is black; RH is red')
# Plot a new plot on top of the previous plot
par(new=T)
# Plot relative humidity, RH [%]
plot(df$time_stamp,df$RH,type='l',axes=FALSE,xlab=NA,ylab=NA,
     col='red')
axis(side = 4)
mtext(side = 4, line = 3, 'RH')

# Water surface temperature and below water surface temperatures
# Plot water surface temperature, TW [deg C]
plot(df$time_stamp,df$TW_1_1_1,type='l',xlab='Date',ylab='TS1',lwd=2,
     main='Infrared Water Sensor (black)')

# Plot water surface temperature TS1 [deg C]
lines(df$time_stamp,df$TS_2_1_1,col='red')

#### Some water temperature and water heat storage ####

plot(df$time_stamp,df$TS_1_1_1,type='l',lwd=2,xlab='Date',ylab='Water temperature')
lines(df$time_stamp,df$TS_2_1_1,lwd=2,col='red')
#lines(df$time_stamp,df$TW_1_1_1,lwd=2,xlab = 'Date',ylab='Temperature',type='l')

plot(df$time_stamp,df$H_stor_filter,type='l',xlab='Date',ylab='Energy',lwd=2,
     main='Filtered H storage')
plot(df$time_stamp,df$H_stor,type='l',xlab='Date',ylab='Energy',lwd=2,
     main='Unfiltered H storage')

#### Diurnal plots ####
### Preparation of data ###
# Grouping into hours for all data
# Note: since the update have to create separate groups for mean and sd.
df_grp_mean <- df %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
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
            RH=mean(RH_1_1_1,na.rm=TRUE))
df_grp_sd <- df %>%
  mutate(time_stamp=as.POSIXct(time_stamp)) %>%
  group_by(hour=format(as.POSIXlt(cut(time_stamp,breaks='hour')),'%H')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
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
            RH_sd=sd(RH_1_1_1,na.rm=TRUE))
# Merge the two dataframe
df_grp <- merge(df_grp_mean,df_grp_sd,by='hour')
rm(df_grp_sd,df_grp_mean)


### Diurnal plots ###
# Diurnal H
plot(df_grp$hour,df_grp$H,lwd=2,type='l',ylab='H',xlab='Date')
# Diurnal LE
plot(df_grp$hour,df_grp$LE,lwd=2,type='l',ylab='LE',xlab='Date')
# Diurnal CO2
plot(df_grp$hour,df_grp$co2_flux,lwd=2,type='l',ylab='CO2 flux',xlab='Date')
# Diurnal atmospheric stability
plot(df_grp$hour,df_grp$zL,lwd=2,type='l',ylab='z/L',xlab='Date')
# Diurnal wind speed
plot(df_grp$hour,df_grp$WS,lwd=2,type='l',ylab='WS',xlab='Date')
# Diurnal wind direction
plot(df_grp$hour,df_grp$WD,lwd=2,type='l',ylab='WD',xlab='Date')
# Diurnal heat storage
plot(df_grp$hour,df_grp$H_stor,lwd=2,type='l',ylab='H storage',xlab='Date')
# Diurnal heat storage (filtered)
plot(df_grp$hour,df_grp$H_stor_filter,lwd=2,type='l',ylab='Filtered H storage',
     xlab='Date')

#### FINAL PLOTS ####

#### * Diurnal plot of global and net radiation ####
## from Nov 2015 to Mar 2016 (5 months)
library(Hmisc)
# Comparison between diurnal RG and RN
# Diurnal global radiation
png('figs/rg_rn.png', res = 360, width = 8, height = 8, units = 'cm')
par(mai = c(0.6,0.6,0.1,0.1))
plot(df_grp$hour,df_grp$RG,lwd=2,type='l',ylab='',col='red',
     xlab='', xaxt = 'n', ylim = c(-50,850))
#minor.tick()
axis(side = 1, at = c(0, 3, 6, 9, 12, 15, 18, 21, 24), 
     labels = c('00:00', '03:00', '06:00', '09:00',
                '12:00', '15:00', ' 18:00', '21:00', '24:00'))
title(ylab = 'Radiation', xlab = 'Time (local time)', line = 2)
# Diurnal global radiation
lines(df_grp$hour,df_grp$RN,lwd=2,xlab='Date',col='blue')
legend('topleft', c('RG','RN'),lty = c(1,1),lwd = c(2,2),col = c('red','blue'))
dev.off()

#### * Trend of global and net radiation ####
## from Nov 2015 to Mar 2016 (5 months)
# To calculate maximum value of RG of the day
# Substitute the original data frame with temp data frame to change colnames
# to 'date'
library(openair)
mean_df_now <- df
colnames(mean_df_now)[1] <- 'date'
mean_df_now$date <- as.POSIXct(mean_df_now$date)
mean_df_now <- timeAverage(mean_df_now, avg.time = 'day', statistic = 'mean')
png('figs/rg_trend.png', res = 360, width = 8, height = 8, units = 'cm')
par(mai = c(0.8,0.8,0.1,0.1))
# Remove the first day from the plot because it is too low
with(mean_df_now[-1,],plot(date,RG_1_1_1,type='l',ylab='',xlab='',
                           xaxt = 'n', col = 'red',ylim =c(0,300)))
with(mean_df_now[-1,],lines(date,RN_1_1_1,col='blue'))
title(xlab = 'Date', ylab = 'Daily mean radiation', line = 2.5)
axis(side = 1, at = c(as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2015-12-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-01-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-02-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-03-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-04-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))])),
     labels = c('Dec','Jan','Feb','Mar','Apr'))
legend('bottomright', c('RG','RN'),lty = c(1,1),lwd = c(2,2),col = c('red','blue'))
dev.off()
#rm(mean_df_now)

#### * Water temperature plots ####
png('figs/water_temp.png', res = 360, width = 16, height = 8, units = 'cm')
par(mai = c(0.8,0.8,0.1,0.1))
plot(df$time_stamp, df$TS_1_1_1, col = 'red', type='l',xlab = '',
     ylab = '',xaxt = 'n')
lines(df$time_stamp, df$TS_2_1_1, col = 'blue', type = 'l')
title(xlab = 'Date', ylab = 'Water temperature', line = 2.5)
axis(side = 1, at = c(as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2015-12-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-01-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-02-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-03-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-04-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-05-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))])),
     labels = c('Dec','Jan','Feb','Mar','Apr','May'))
legend('bottomright', c(expression(paste('T'['1'])),expression(paste('T'['2']))),
       lty = c(1,1),lwd = c(2,2),col = c('red','blue'))

dev.off()


#### * Heat stored in water plots ####
png('figs/heat_stored.png', res = 360, width = 16, height = 8, units = 'cm')
par(mai = c(0.8,0.8,0.1,0.1))
plot(df$time_stamp, df$H_stor_filter, col = 'red', type = 'l', ylab = '',
     xlab = '', xaxt = 'n', ylim = c(-1000,1500))
title(xlab = 'Date', ylab = 'Heat stored in water', line = 2.5)
axis(side = 2, at = -1000, labels = '-1000')
axis(side = 1, at = c(as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2015-12-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-01-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-02-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-03-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-04-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))])),
     labels = c('Dec','Jan','Feb','Mar','Apr'))
dev.off()

#### * Energy balance ####
index_wind <- which(df$wind_check == 1)
quality_check_index_LE <- which(df$wind_check == 1 & df$qc_LE != 2 & df$qc_H != 2)
quality_check_index_H <- which(df$wind_check == 1 & df$qc_H != 2 & df$qc_LE != 2)

energy_out <- df$LE[index_wind] + df$H[index_wind] + df$H_stor_filter[index_wind]
energy_in <- df$RN_1_1_1[index_wind]
# Need to remove qc 2 from data
png('figs/energy_balance.png', res = 360, width = 8, height = 8, units = 'cm')
par(mai = c(0.6,0.6,0.1,0.1))
plot(energy_in, energy_out, pch = 19, col = 'grey10', cex = 0.4,
     xlim = c(-50,500), ylim = c(-500,1000), xlab = '', ylab ='')
title(xlab = 'LE + H + G', ylab = 'RN', line = 2)
lmEB <- lm(energy_out ~ energy_in)
abline(lmEB, col ='blue', lwd = 4, lty = 2)
abline(1,1, lwd = 4)
legend('topleft', c('Diagonal','Regression'), lty = c(1,2), lwd = c(2,2), 
       col = c('black','blue'))
dev.off()
# Remove temporary variables
rm(energy_in, energy_out, index_wind, lmEB, 
   quality_check_index_H, quality_check_index_LE)

#### * Footprint and wind rose ####
library(openair)
# Windrose
png('figs/wind_rose.png', res = 400, width = 8, height = 8, units = 'cm')
windRose(df,ws='wind_speed',wd='wind_dir',
         paddle=FALSE, par.settings=list(fontsize=list(text=8)))
dev.off()
# 90% Flux
png('figs/footprint.png',res = 400, width = 8, height = 8, units = 'cm')
names(df)[which(names(df) == 'x_90.')] <- 'Distance'
polarPlot(df, x = 'Distance', wd = 'wind_dir', pollutant = 'wind_speed',statistic = 'mean',
          exclude.missing = TRUE, key = list(header = 'Wind speed', footer = ''),
          par.settings=list(fontsize=list(text=8)),angle.scale = 225)
names(df)[which(names(df) == 'Distance')] <- 'x_90.'
dev.off()

# LE
png('figs/LE_footprint.png',res = 400, width = 8, height = 8, units = 'cm')
names(df)[which(names(df) == 'x_90.')] <- 'Distance'
polarPlot(df, x = 'Distance', wd = 'wind_dir', pollutant = 'LE',statistic = 'mean',
          exclude.missing = TRUE, key = list(header = 'LE', footer = ''),
          par.settings=list(fontsize=list(text=8)),angle.scale = 225)
names(df)[which(names(df) == 'Distance')] <- 'x_90.'
dev.off()

# H
png('figs/H_footprint.png',res = 400, width = 8, height = 8, units = 'cm')
names(df)[which(names(df) == 'x_90.')] <- 'Distance'
polarPlot(df, x = 'Distance', wd = 'wind_dir', pollutant = 'H',statistic = 'mean',
          exclude.missing = TRUE, key = list(header = 'H', footer = ''),
          par.settings=list(fontsize=list(text=8)),angle.scale = 225)
names(df)[which(names(df) == 'Distance')] <- 'x_90.'
dev.off()

# CO2 flux
png('figs/co2_footprint.png',res = 400, width = 8, height = 8, units = 'cm')
names(df)[which(names(df) == 'x_90.')] <- 'Distance'
polarPlot(df, x = 'Distance', wd = 'wind_dir', pollutant = 'co2_flux',statistic = 'mean',
          exclude.missing = TRUE, key = list(header = quickText('CO2'), footer = ''),
          par.settings=list(fontsize=list(text=8)),angle.scale = 225)
names(df)[which(names(df) == 'Distance')] <- 'x_90.'
dev.off()

#### * CO2 flux ####
png('figs/co2_flux.png',width= 8, height= 8, res = 360, units='cm')
par(mar=c(3.1,3.1,0.5,0.5))
index <- which((df$qc_co2_flux==1 | df$qc_co2_flux == 0) & df$wind_check_strict == TRUE)
#plot(df$time_stamp[-index],df$co2_flux[-index], pch=19,col='red')
plot(df$time_stamp[index],df$co2_flux[index],col='black',type='l',lwd=2,
     xlab='', ylab = '',xaxt='n')
title(xlab = 'Date', ylab = expression('CO'['2']), line = 2)
axis(side = 1, at = c(as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2015-12-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-01-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-02-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-03-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-04-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-05-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))])),
     labels = c('Dec','Jan','Feb','Mar','Apr','May'))
dev.off()

#### * H flux ####

png('figs/H_flux.png',width= 8, height= 8, res = 360, units='cm')
par(mar=c(3.1,3.1,0.5,0.5))
index <- which((df$qc_H==1 | df$qc_H == 0) & df$wind_check_strict == TRUE)
# Note that due to possible outliers but not detected by qc, the two
# very negative points are excluded from the plot
H_plot <- df$H
H_plot[which(H_plot < -100)] <- NA

plot(df$time_stamp[index],H_plot[index],col='black',type='l',lwd=2,
     xlab='', ylab = '',xaxt='n')
title(xlab = 'Date', ylab = 'H', line = 2)
axis(side = 1, at = c(as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2015-12-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-01-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-02-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-03-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-04-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-05-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))])),
     
     labels = c('Dec','Jan','Feb','Mar','Apr','May'))
dev.off()
rm(H_plot)
#### * LE flux ####
png('figs/LE_flux.png',width= 8, height= 8, res = 360, units='cm')
par(mar=c(3.1,3.1,0.5,0.5))
index <- which((df$qc_LE==1 | df$qc_LE == 0) & df$wind_check_strict == TRUE)
#plot(df$time_stamp[-index],df$co2_flux[-index], pch=19,col='red')
plot(df$time_stamp[index],df$LE[index],col='black',type='l',lwd=2,
     xlab='', ylab = '',xaxt='n')
title(xlab = 'Date', ylab = 'LE', line = 2)
axis(side = 1, at = c(as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2015-12-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-01-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-02-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-03-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-04-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-05-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))])),
     labels = c('Dec','Jan','Feb','Mar','Apr','May'))
dev.off()

#### * T and RH plots ####
png('figs/RHT_plot.png', res = 360, width = 16, height = 8, units = 'cm')
par(mai = c(0.7,0.7,0.1,0.7))
plot(df$time_stamp, df$TA_1_1_1, col = 'red', type='l',xlab = '',
     ylab = '',xaxt = 'n', ylim =c(20,34))
par(new = TRUE)
plot(df$time_stamp, df$RH, type = "l", col='blue',
     axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4, ylim=c(0,100),las=1)
mtext("RH", side=4, line=2.5)
title(xlab = 'Date', ylab = 'Air temperature', line = 2.5)
axis(side = 1, at = c(as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2015-12-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-01-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-02-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-03-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))]),
                      as.numeric(mean_df_now$date
                                 [which(mean_df_now$date == 
                                          as.POSIXct(strptime('2016-04-01',
                                                              format = '%Y-%m-%d', 
                                                              tz = 'GMT')))])),
     labels = c('Dec','Jan','Feb','Mar','Apr'))
legend('bottomleft', c('T', 'RH'),
       lty = c(1,1),lwd = c(2,2),col = c('red','blue'))

dev.off()