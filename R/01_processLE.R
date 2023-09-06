#install.packages("openair")
#install.packages("plyr")
#install.packages('sjPlot')
#install.packages("psych")
#install.packages("forecast")
##### packages ####
library(dplyr)
library(ggpubr)
library(rstatix)
library(lubridate)
library(plyr)
library(openair)
library(ggplot2)
library(sjPlot)
library(forecast)

source('R/tools/tool_convert_magic.R')
source('R/tools/tool_charactersNumeric.R')
source('R/tools/trapezium_intg_2.R')
source('R/tools/trapezium_intg_3.R')
source('R/tools/vap_pres_Buck.R')
source('R/tools/unst_stab_category.R') # To categorize atmospheric stability
source('R/tools/unst_stab_category2.R') # To categorize atmospheric stability to either unstable or stable


#### Load EC ######
df_ec <- read.csv('data/Advanced mode/eddypro_muka_head2020_full_output_2022-10-13T182913_adv.csv')

# changing all -9999.0 or -9999 missing data to be NA
for (i in 1: length(df_ec)) {
  df_ec[i][df_ec[i] == '-9999' | df_ec[i] == '-9999.0' | df_ec[i] == '--'] <- NA  
  
}
rm(i) 
# delete unnecessary row
df_ec <- df_ec[(-2),] ###remove 2 row units ##

# remove unnecessary column
df_ec <- df_ec[,c(-1)]  ###remove file name###


colnames(df_ec) <- c("date","time","DOY","daytime","file_records","used_records","Tau","qc_Tau","rand_err_Tau","H","qc_H",
                     "rand_err_H","LE","qc_LE","rand_err_LE","co2_flux","qc_co2_flux","rand_err_co2_flux","h2o_flux",
                     "qc_h2o_flux","rand_err_h2o_flux","H_strg","LE_strg","co2_strg","h2o_strg","co2_v-adv","h2o_v-adv",
                     "co2_molar_density","co2_mole_fraction","co2_mixing_ratio","co2_time_lag","co2_def_timelag",
                     "h2o_molar_density","h2o_mole_fraction","h2o_mixing_ratio","h2o_time_lag","h2o_def_timelag",
                     "sonic_temperature","air_temperature","air_pressure","air_density","air_heat_capacity","air_molar_volume",
                     "ET","water_vapor_density","e","es","specific_humidity","RH","VPD","Tdew","u_unrot","v_unrot","w_unrot",
                     "u_rot","v_rot","w_rot","wind_speed","max_wind_speed","wind_dir","yaw","pitch","roll","u*","TKE","L",
                     "(z-d)/L","bowen_ratio","T*","model","x_peak","x_offset","x_10%","x_30%","x_50%","x_70%","x_90%","un_Tau",
                     "Tau_scf","un_H","H_scf","un_LE","LE_scf","un_co2_flux","co2_scf","un_h2o_flux","h2o_scf","spikes_hf",
                     "amplitude_resolution_hf","drop_out_hf","absolute_limits_hf","skewness_kurtosis_hf","skewness_kurtosis_sf",
                     "discontinuities_hf","discontinuities_sf","timelag_hf","timelag_sf","attack_angle_hf","non_steady_wind_hf","u_spikes",
                     "v_spikes","w_spikes","ts_spikes","co2_spikes","h2o_spikes","chopper_LI-7500","detector_LI-7500",
                     "pll_LI-7500","sync_LI-7500","mean_value_RSSI_LI-7500","u_var","v_var","w_var","ts_var","co2_var",
                     "h2o_var","w/ts_cov","w/co2_cov","w/h2o_cov","vin_sf_mean","co2_mean","h2o_mean","dew_point_mean",
                     "co2_signal_strength_7500_mean","NA")

# remove old named row 1
df_ec <- df_ec[(-1),]



#### Load biomet ####
df_biomet <- read.csv('data/Advanced mode/biomet.csv')
#### changing all -9999.0 or -9999 missing data to be NA ###
for (i in 1: length(df_biomet)) {
  df_biomet[i][df_biomet[i] == '-9999' | df_biomet[i] == '-9999.0' | df_biomet[i] == '--'] <- NA  
  
}
rm(i) 
# delete unnecessary col 
df_biomet <- df_biomet[,c(-3)]  ##remove (DOY)

# remove unnecessary row, remove (units)
df_biomet <- df_biomet[-1,]

date1 <- paste(df_ec$date, df_ec$time)
date2 <- paste(df_biomet$date, df_biomet$time)

df_ec <- cbind(date1,df_ec)
df_biomet <- cbind(date2,df_biomet)
df_ec <- df_ec[,-c(2,3)]
df_biomet <- df_biomet[,-c(2,3)]
df_biomet$date2 <- strptime(df_biomet$date2, format = "%m/%d/%Y %H:%M", tz= "Asia/Kuala_Lumpur")
df_ec$date1 <- strptime(df_ec$date1, format = "%Y-%m-%d %H:%M", tz= "Asia/Kuala_Lumpur")

# renamed date
colnames(df_ec)[1] <- "date"
colnames(df_biomet)[1] <- "date"


# merge df ec with biomet
df <- merge(df_ec,df_biomet,by= c('date'))
#rm(date1,date2,df_biomet,df_ec) 
rm(date1,date2)

# using covert magic to convert all columns to character first

#ndf <- convert_magic(df[,c(seq(1,ncol(df)))],c(rep('character',times =ncol(df))))




# change all non factors or characters to be numeric

sapply(df,class)
df[,-1] <- charactersNumeric(df[,-1])

# change the date to POSIXCT format
df$date <- strptime(df$date, format = "%Y-%m-%d %H:%M", tz= "Asia/Kuala_Lumpur")
df$date <- as.POSIXct(df$date)

# summary(df)


#### Data processing and filtering ####
###### Convert TA, TS from K to Celsius ######
df$TA_1_1_1<- df$TA_1_1_1 - 273.15
df$TS_1_1_1<- df$TS_1_1_1 - 273.15
# Remove all improbable values of T
df$TA_1_1_1[which(df$TA_1_1_1 < 0 | df$TA_1_1_1 > 100 )] <- NA
df$TS_1_1_1[which(df$TS_1_1_1 < 0 )] <- NA

df$TA_1_1_1[df$TA_1_1_1 < 23.5 | df$TA_1_1_1 > 34] <- NA
df$TS_1_1_1[df$TS_1_1_1 < 26.5 | df$TS_1_1_1 > 32] <- NA

###### RH filtering ######
#df$RH[df$RH < 25 | df$RH > 100] <- NA
df$RH_1_1_1[df$RH_1_1_1 > 0 & df$RH_1_1_1< 60] <- NA

###### LE and H filtering ######
df$LE[which(df$qc_LE == 2)] <- NA 
df$H[which(df$qc_H == 2)] <- NA 


###### WD filtering, higher than 45 and less than 315 ######

df$LE[df$wind_dir > 45 & df$wind_dir < 315] <- NA
df$H[df$wind_dir > 45 & df$wind_dir< 315] <- NA

###### Rain filtering ######
df$LE[df$P_RAIN_1_1_1 > 0] <- NA
df$H[df$P_RAIN_1_1_1 > 0] <- NA


####  ea, es, deltae calculation ####
rh <- df$RH_1_1_1 / 100
ea <- vap_pres_Buck(df$TA_1_1_1,rh)/10
es1 <- vap_pres_Buck(df$TS_1_1_1,1)/10

df <- cbind(df, ea)
df <- cbind(df, es1)



delta_e <- es1 - ea 
delta_T <- df$TS_1_1_1 - df$TA_1_1_1

df <- cbind(df, delta_e, delta_T)

rm(delta_e,delta_T)
rm(ea,es1,rh)



#### Ce and CH calculation ####
Ce<- df$LE/2.54 * 10^6 * df$wind_speed * df$water_vapor_density
CH<- df$H/df$air_density * 1.0035 * 1000 * df$wind_speed * df$delta_T
df<- cbind(df,Ce,CH)
rm(Ce,CH)

##### U_deltaE & U_deltaT calculation ######
U_deltaE <- df$wind_speed * df$delta_e

U_deltaT <- df$wind_speed * df$delta_T


df<- cbind(df,U_deltaE,U_deltaT)
rm(U_deltaE,U_deltaT)

###### Rename ZL ######
names(df)[66] <- 'Z.L'

#### Positive and negative Z.L, others  ####

###### Positive and negative Z.L ######
df_zlP<- df[,c(1,9,12,66)]
df_zlP$Z.L[which(df_zlP$Z.L< 0)]<- NA

df_zlN <-df[,c(1,9,12,66)]
df_zlN$Z.L[which(df_zlN$Z.L> 0)]<- NA

###### Positive and negative deltaT, u* ######

df_deltaTP<- df[,c(1,63,137)]
df_deltaTP$delta_T[which(df_deltaTP$delta_T< 0)]<- NA

df_deltaTN<- df[,c(1,63,137)]
df_deltaTN$delta_T[which(df_deltaTN$delta_T> 0)]<- NA

###### Positive and negative U_deltaT, u* ######
df_U_deltaTP<- df[,c(1,63,141)]
df_U_deltaTP$U_deltaT[which(df_U_deltaTP$U_deltaT< 0)] <- NA
df_U_deltaTN<- df[,c(1,63,141)]
df_U_deltaTN$U_deltaT[which(df_U_deltaTN$U_deltaT> 0)] <- NA



#### Stability categories ####
df$Z.L[which(df$Z.L < -10 | df$Z.L > 10 )] <- NA

stability_no <- rep(NA,nrow(df))
stability <- rep(NA,nrow(df))
stability_no <- sapply(df$Z.L,unst_stab_category)
stability <- sapply(df$Z.L,unst_stab_category2)

df <- cbind(df,stability_no,stability)

rm(stability,stability_no)

#### Filtering stability categories ####
# The data is filtered because of the low number of data points
# in stability categories 9 and 10
df$LE[which(df$stability_no == 9)] <- NA
df$LE[which(df$stability_no == 10)] <- NA
df$H[which(df$stability_no == 9)] <- NA
df$H[which(df$stability_no == 10)] <- NA

#### merge month data #####
df_month <- timeAverage(df, avg.time = "1 month")

#### merge day data ####

df$date <- strptime(df$date, format = "%Y-%m-%d %H:%M",tz = "Asia/Kuala_Lumpur")
df$date<- as.POSIXct.POSIXlt(df$date)
colnames(df)[1] <- "date"
df_day <- timeAverage(df,avg.time ="1 day")

#### merge hour data ####


df_hour <- timeAverage(df,avg.time ="1 hour")



df_week <- timeAverage(df,avg.time ="1 week")

#### p-value ####

tab_corr((df)[c(12,9,57,136,137,140,141)],na.deletion = c("listwise"),
         show.p = TRUE,corr.method = c("pearson"),p.numeric = F)


#tab_corr((df_cor)[c(1,2,3,4,5,6,7)],na.deletion = c("listwise"),
#         show.p = TRUE,corr.method = c("pearson"),p.numeric = F)



