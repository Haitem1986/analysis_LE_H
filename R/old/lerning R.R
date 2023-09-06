#install.packages("openair")
library(openair)
#install.packages("plyr")
library(plyr)
library(dplyr)
source('R/tools/tool_convert_magic.R')
source('R/tools/tool_charactersNumeric.R')
source('R/tools/trapezium_intg_2.R')
source('R/tools/trapezium_intg_3.R')
source('R/tools/vap_pres_Buck.R')


####download data EC######
df_ec <- read.csv('data/Advanced mode/eddypro_muka_head2020_full_output_2022-10-13T182913_adv.csv')

#### changing all -9999.0 or -9999 missing data to be NA ###
for (i in 1: length(df_ec)) {
  df_ec[i][df_ec[i] == '-9999' | df_ec[i] == '-9999.0' | df_ec[i] == '--'] <- NA  
  
}
rm(i) 
#####delete unnecessary row ####
df_ec <- df_ec[(-2),] ###remove 2 row units ##

### remove unnecessary column##  
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

##### remove old named row 1 ####
df_ec <- df_ec[(-1),]



#####download biomet data######
df_biomet <- read.csv('data/Advanced mode/biomet.csv')
#### changing all -9999.0 or -9999 missing data to be NA ###
for (i in 1: length(df_biomet)) {
  df_biomet[i][df_biomet[i] == '-9999' | df_biomet[i] == '-9999.0' | df_biomet[i] == '--'] <- NA  
  
}
rm(i) 
####delete unnecessary col ####
df_biomet <- df_biomet[,c(-3)]  ##remove (DOY)

### remove unnecessary row ####remove (units)
df_biomet <- df_biomet[-1,]

date1 <- paste(df_ec$date, df_ec$time)
date2 <- paste(df_biomet$date, df_biomet$time)

df_ec <- cbind(date1,df_ec)
df_biomet <- cbind(date2,df_biomet)
df_ec <- df_ec[,-c(2,3)]
df_biomet <- df_biomet[,-c(2,3)]
df_biomet$date2 <- strptime(df_biomet$date2, format = "%m/%d/%Y %H:%M", tz= "Asia/Kuala_Lumpur")
df_ec$date1 <- strptime(df_ec$date1, format = "%Y-%m-%d %H:%M", tz= "Asia/Kuala_Lumpur")

  ###renamed date ####
colnames(df_ec)[1] <- "date"
colnames(df_biomet)[1] <- "date"


#### merge df ec with biomet #####
df <- merge(df_ec,df_biomet,by= c('date'))
#rm(date1,date2,df_biomet,df_ec) 
rm(date1,date2)
####using covernt magic to convert all columns to character first #####

##df <- convert_magic(df[,c(seq(1,ncol(df)))],c(rep('character',times =ncol(df))))




###### change all non factors or characters to be numeric #####
 
sapply(df,class)
df[,-1] <- charactersNumeric(df[,-1])
 
 #### change the date to POSIXCT format ####
 df$date <- strptime(df$date, format = "%Y-%m-%d %H:%M", tz= "Asia/Kuala_Lumpur")
 df$date <- as.POSIXct(df$date)

 

####df <- df[which(df$DATE >= as.POSIXct("2019-01-01 00:00",
                                  #   format = "%Y-%m-%d %H:%M", 
                                  #   tz= "Asia/Kuala_Lumpur")
                     #& df$DATE <= as.POSIXct("2020-12-31 00:00",
                                           #   format = "%Y-%m-%d %H:%M", 
                                            #  tz= "Asia/Kuala_Lumpur")),]




# extracting related variables from raw variables
#dfex <- data.frame(df$DATE,df$DOY,df$WS,df$WD,df$RH_1_1_1,df$H2O,df$H,df$LE,df$ZL,df$P_RAIN_1_1_1,df$TA_1_1_1,df$ET,df$USTAR,
                #   df$TS_1_1_1,df$H_QC,df$LE_QC,df$FH2O,df$FH2O_QC,df$RHOA)


#Rename variables
#colnames(dfex) <- c("date","DOY","WS","WD","RH","H2O",
                 # "H","LE","ZL","P_RAIN","TA","ET",
                  #"USTAR","TS","H_QC","LE_QC","FH2O","FH2O_QC","RHOA")
summary(df)

# Convert TA, TS from K to Celsius
df$TA_1_1_1<- df$TA_1_1_1 - 273.15
df$TS_1_1_1<- df$TS_1_1_1 - 273.15
# Remove all improbable values of T
df$TA_1_1_1[which(df$TA_1_1_1 < 0 | df$TA_1_1_1 > 100 )] <- NA
df$TS_1_1_1[which(df$TS_1_1_1 < 0 )] <- NA


##### random filter######
df$TA_1_1_1[df$TA_1_1_1 < 23.5 | df$TA_1_1_1 > 34] <- NA
df$TS_1_1_1[df$TS_1_1_1 < 26.5 | df$TS_1_1_1 > 32] <- NA
#df$RH[df$RH < 25 | df$RH > 100] <- NA
df$LE[which(df$qc_LE == 2)] <- NA 
df$H[which(df$qc_H == 2)] <- NA 
df$RH_1_1_1[df$RH_1_1_1 > 0 & df$RH_1_1_1< 60] <- NA


# WD <- dfex$WD
# WD[dfex$WD > 45 & dfex$WD < 315] <- NA
              
# dfex$WD[dfex$WD > 45 & dfex$WD < 315] <- NA

#####remove LE when the Wd higher than 45 and less than 315 #####

df$LE[df$wind_dir > 45 & df$wind_dir < 315] <- NA
df$H[df$wind_dir > 45 & df$wind_dir< 315] <- NA





class(df$RH)



rh <- df$RH_1_1_1 / 100
ea <- vap_pres_Buck(df$TA_1_1_1,rh)/10
es <- vap_pres_Buck(df$TS_1_1_1,1)/10
delta_e <- es - ea 
delta_T <- df$TS_1_1_1 - df$TA_1_1_1

df <- cbind(df, delta_e,delta_T)



summary(df$wind_speed)

#I <- df[which(df$wind_speed < 1),] 
#II<- df[which(df$wind_speed  > 1 & df$wind_speed < 1.5),]
#III <- dfex[which(df$wind_speed  > 1.5 & df$wind_speed < 2),] 
#IV <- df[which(df$wind_speedS  < 2.5),]
#aLE <- (df$LE) /2540000

Ce<- df$LE/2.54*10^6*(df$wind_speed)*(df$water_vapor_density)
CH<- df$H/(df$air_density)*1.0035*1000*(df$wind_speed)*(df$delta_T)



 
  
####XXXXXXX#####

####plot ####
jpeg(filename='FIG/LE&U.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(3,3,0.5,0.5))
plot(df$wind_speed, df$LE,pch = 19,cex= 0.2, xlab= "", ylab="", xlim = c(0,2.5), 
     ylim = c(-10,50))
abline(lm(df$LE ~ df$wind_speed), col = "red", lty = 3, lwd = 2)
mtext(side = 1, text = expression(paste('U', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('LE', sep = )),line = 2)
dev.off()


jpeg(filename='FIG/LEQ&U.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df$wind_speed, df$LE,pch = 19,cex= 0.5, xlab= "", ylab="")
abline(lm(df$LE ~ df$wind_speed), col = "red")
mtext(side = 1, text = expression(paste('U', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('LE', sep = )),line = 2)
dev.off()

####LE AND DELTA_E##
jpeg(filename='FIG/LE&delta_e.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(delta_e,df$LE,pch = 19,cex= 0.5, xlab= "", ylab="", ylim = c(-10,50), 
     xlim = c(0,1.5))
abline(lm(df$LE ~ delta_e), col = "red")
mtext(side = 1, text = expression(paste('delta_e', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('LE', sep = )),line = 2)
dev.off()


###LE AND CE ###
jpeg(filename='FIG/LE&Ce.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(Ce, df$LE,pch = 19,cex= 0.5, xlab= "", ylab="")
abline(lm(df$LE ~ Ce), col = "red")
mtext(side = 1, text = expression(paste('CE', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('LE', sep = )),line = 2)
dev.off()



####H AND U ####
jpeg(filename='FIG/H&U.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df$wind_speed, df$H,pch = 19,cex= 0.5, xlab= "", ylab="", ylim =c(-5,15), xlim =c(0.0,2.5))
abline(lm(df$H ~ df$wind_speed), col = "red")
mtext(side = 1, text = expression(paste('U', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('H', sep = )),line = 2)
dev.off()  


####H AND delta_t####
jpeg(filename='FIG/H&delta_T.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df$delta_T, df$H,pch = 19,cex= 0.5, xlab= "", ylab="" ,ylim =c(-2,10))
abline(lm(df$H ~ df$delta_T), col = "red")
mtext(side = 1, text = expression(paste('delta_t', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('H', sep = )),line = 2)
dev.off()

####H& CH ######
jpeg(filename='FIG/H&CH.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(CH, df$H,pch = 19,cex= 0.5, xlab= "", ylab="",ylim =c(-2,3))
abline(lm(df$H ~ CH), col = "red")
mtext(side = 1, text = expression(paste('CH', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('H', sep = )),line = 2)
dev.off()


####H& air density  ######
jpeg(filename='FIG/H&air density .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df$air_density, df$H,pch = 19,cex= 0.5, xlab= "", ylab="", ylim = c(-5,15))
abline(lm(df$H ~ df$air_density), col = "red")
mtext(side = 1, text = expression(paste('air density', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('H', sep = )),line = 2)
dev.off()



##### U _ deltaE & delta T######
U_deltaE <- (df$wind_speed* df$delta_e)

U_deltaT <- (df$wind_speed* df$delta_T)


df<- cbind(df,U_deltaE,U_deltaT)



#####LE&u_deltae#####

jpeg(filename='FIG/LE&u_delta E .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df$U_deltaE, df$LE,pch = 19,cex= 0.5, xlab= "", ylab="", ylim = c(-25,50))
abline(lm(df$LE ~ df$U_deltaE), col = "red")
mtext(side = 1, text = expression(paste('U delta E', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('LE', sep = )),line = 2)
dev.off()

#####H&u_deltaT#####

jpeg(filename='FIG/H&u_delta T .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df$U_deltaT, df$H,pch = 19,cex= 0.5, xlab= "", ylab="", ylim = c(-10,35))
abline(lm(df$H ~ df$U_deltaT), col = "red")
mtext(side = 1, text = expression(paste('U delta T', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('H', sep = )),line = 2)
dev.off()

#####LE&U#####

jpeg(filename='FIG/H&U .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
plot(df$U_deltaT, df$H,pch = 19,cex= 0.5, xlab= "", ylab="", ylim = c(-10,35))
abline(lm(df$H ~ df$U_deltaT), col = "red")
mtext(side = 1, text = expression(paste('U delta T', sep = "")),line = 2)
mtext(side = 2, text = expression(paste('H', sep = )),line = 2)
dev.off()

##### ggplot ####
library(ggplot2)

jpeg(filename='FIG/ggplot_LE&u_delta e .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(U_deltaE, LE)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('U delta e') + coord_cartesian(xlim=c(0,1.7), ylim=c(-12,50)) +
  theme_bw()
dev.off()

jpeg(filename='FIG/ggplot_H&u_delta T .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(U_deltaT, H)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green', method = "lm") +
  xlab('U delta T') + coord_cartesian(xlim=c(0,2.5), ylim=c(-3,13)) +
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_LE&U .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(wind_speed, LE)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('U ') + coord_cartesian(xlim=c(0,2.5), ylim=c(-12,50)) +
  theme_bw()
dev.off()
 

jpeg(filename='FIG/ggplot_H&U .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(wind_speed, H)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',method = "lm") +
  xlab('U ') + coord_cartesian(xlim=c(0,2.5), ylim=c(-3,13)) +
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_LE&delta e .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(delta_e, LE)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('Delta e ') + coord_cartesian(xlim=c(0,1.3), ylim=c(-12,50)) +
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_H_deltaT.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(delta_T, H)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green', method = "lm") +
  xlab('ΔT') + coord_cartesian(xlim=c(-3.6,2.5), ylim=c(-3,13)) +
  theme_bw()
dev.off()


##### ZL ####


jpeg(filename='FIG/ggplot_LE&zl .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(`(z-d)/L`, LE)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('ZL') +coord_cartesian(xlim=c(-1,1), ylim=c(-12,50))+
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_LE&zl+ .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(`(z-d)/L`, LE)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('ZL') +coord_cartesian(xlim=c(0,1), ylim=c(-12,50))+
  theme_bw()
dev.off()

jpeg(filename='FIG/ggplot_LE&zl- .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(`(z-d)/L`, LE)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('ZL') +coord_cartesian(xlim=c(-1,0), ylim=c(-12,50))+
  theme_bw()
dev.off()



jpeg(filename='FIG/ggplot_H&zl .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(`(z-d)/L`, H)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "loess") +
  xlab('ZL') +coord_cartesian(xlim=c(-5,0), ylim=c(-1,5))+
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_H&zl+ .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(`(z-d)/L`, H)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "l0ess") +
  xlab('ZL') +coord_cartesian(xlim=c(0,0.5), ylim=c(-5,5))+
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_H&zl- .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(`(z-d)/L`, H)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('ZL') +coord_cartesian(xlim=c(-1,0), ylim=c(-3,13))+
  theme_bw()
dev.off()

df_zlP<- df[,c(1,9,12,66)]
df_zlP$`(z-d)/L`[which(df_zlP$`(z-d)/L`< 0)]<- NA

df_zlN <-df[,c(1,9,12,66)]
df_zlN$`(z-d)/L`[which(df_zlN$`(z-d)/L`> 0)]<- NA

jpeg(filename='FIG/ggplot_H&zlN .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_zlN, aes(`(z-d)/L`, H)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('ZL') +coord_cartesian(xlim=c(-1,0), ylim=c(-3,13))+
  theme_bw()
dev.off()

jpeg(filename='FIG/ggplot_H&zlP .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_zlP, aes(`(z-d)/L`, H)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('ZL') +coord_cartesian(xlim=c(0,1), ylim=c(-3,13))+
  theme_bw()
dev.off()

jpeg(filename='FIG/ggplot_LE&zlN .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_zlN, aes(`(z-d)/L`, LE)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('ZL') +coord_cartesian(xlim=c(-1,0), ylim=c(-12,50))+
  theme_bw()
dev.off()

jpeg(filename='FIG/ggplot_LE&zlP .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_zlP, aes(`(z-d)/L`, LE)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('ZL') +coord_cartesian(xlim=c(0,1), ylim=c(-12,50))+
  theme_bw()
dev.off()

#### ustar ####

jpeg(filename='FIG/ggplot_ustar&U .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(wind_speed,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  theme_bw()
dev.off()



jpeg(filename='FIG/ggplot_ustar&deltae .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(delta_e,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  theme_bw()
dev.off()  
  

jpeg(filename='FIG/ggplot_ustar&deltaT .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(delta_T,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_ustar&Udeltae .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(U_deltaE,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  theme_bw()
dev.off()



jpeg(filename='FIG/ggplot_ustar&UdeltaT .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(U_deltaT,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_ustar&LE .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(`u*`,`LE`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  coord_cartesian(xlim=c(0,0.2), ylim=c(-12,50))+
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_ustar&H .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(`u*`,`H`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  coord_cartesian(xlim=c(0,0.15), ylim=c(-7,20))+
  theme_bw()
dev.off()



df_deltaTP<- df[,c(1,63,135)]
df_deltaTP$delta_T[which(df_deltaTP$delta_T< 0)]<- NA

df_deltaTN<- df[,c(1,63,135)]
df_deltaTN$delta_T[which(df_deltaTN$delta_T> 0)]<- NA

jpeg(filename='FIG/USTAR&DELTAT_N .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_deltaTN, aes(delta_T,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('ΔT') +coord_cartesian(xlim=c(-4.5,0), ylim=c(0,0.3))+
  theme_bw()
dev.off()


jpeg(filename='FIG/USTAR&DELTAT_P .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_deltaTP, aes(delta_T,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('ΔT') +coord_cartesian(xlim=c(0,5), ylim=c(0,0.3))+
  theme_bw()
dev.off()


df_U_deltaTP<- df[,c(1,63,137)]
df_U_deltaTP$U_deltaT[which(df_U_deltaTP$U_deltaT< 0)]<- NA


df_U_deltaTN<- df[,c(1,63,137)]
df_U_deltaTN$U_deltaT[which(df_U_deltaTN$U_deltaT> 0)]<- NA

jpeg(filename='FIG/USTAR&U_DELTAT_N .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_U_deltaTN, aes(U_deltaT,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('UΔT') +coord_cartesian(xlim=c(-4,0), ylim=c(0,0.3))+
  theme_bw()
dev.off()


jpeg(filename='FIG/USTAR&U_DELTAT_P .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_U_deltaTP, aes(U_deltaT,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  xlab('UΔT') +coord_cartesian(xlim=c(0,10), ylim=c(0,0.3))+
  theme_bw()
dev.off()


source('R/tools/unst_stab_category.R') # To categorize atmospheric stability
source('R/tools/unst_stab_category2.R') # To categorize atmospheric stability to either unstable or stable

names(df)[66] <- 'Z.L'


df$Z.L[which(df$Z.L < -10 | df$Z.L > 10 )] <- NA

stability_no <- rep(NA,nrow(df))
stability <- rep(NA,nrow(df))
stability_no <- sapply(df$Z.L,unst_stab_category)
stability <- sapply(df$Z.L,unst_stab_category2)

df <- cbind(df,stability_no,stability)

rm(stability,stability_no)

library(ggplot2)

ggplot(df, aes(factor(stability_no),LE)) + 
  geom_boxplot(outlier.size=0,fill="white") + coord_cartesian(ylim=c(-10,50))



ggplot(df, aes(factor(stability_no),H)) + 
  geom_boxplot(outlier.size=0,fill="white") + coord_cartesian(ylim=c(-5,5))




ggplot(df, aes(factor(stability_no),delta_e)) + 
  geom_boxplot(outlier.size=0,fill="white") + coord_cartesian(ylim=c(-2,2.5))



ggplot(df, aes(factor(stability_no),U_deltaE)) + 
  geom_boxplot(outlier.size=0,fill="white") + coord_cartesian(ylim=c(-2,3))



ggplot(df, aes(factor(stability_no),wind_speed)) + 
  geom_boxplot(outlier.size=0,fill="white") + coord_cartesian(ylim=c(-2.5,5))


ggplot(df, aes(factor(stability_no),LE)) + 
  geom_boxplot(outlier.size=0,fill="white") + 
  geom_jitter(width = 0.2, size = 2, alpha = 0.5) +
  coord_cartesian(ylim=c(-10,50))



 #####packages ####
 library(dplyr)
 library(ggpubr)
 library(rstatix)
 library(lubridate)
 library(plyr)

 #### merge month data #####
dfex_month <- timeAverage(dfex , avg.time = "1 month")

 ####plot time serious  ######
 
 ####LE ####
 LE_2019 <- selectByDate(dfex_month,year = 2019)[,c(1,8)]
 LE_2020 <- selectByDate(dfex_month,year = 2020)[,c(1,8)]
LE_2019$date <- format(as.POSIXct(LE_2019$date),"%m")
LE_2020$date <- format(as.POSIXct(LE_2020$date),"%m")

 jpeg(filename='fig/LE_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
 par(mar=c(4,4,1,1),xpd=FALSE)
 plot(LE_2019$date, LE_2019$LE,  type = 'l', xlab = '', ylab = '', ylim = c(5,20),col = "Red", xaxt = "n")
 points(LE_2019$date,LE_2019$LE, pch = 19, col = "Red")
 points(LE_2020$date,LE_2020$LE, pch = 19, col = "Green")
 lines(LE_2020$date,LE_2020$LE, pch = 19, col = "Green")
 axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
# mtext('Months', side = 1, line = 2.5)
 mtext(expression(paste('LE', sep = "")), side = 2, line = 2)
 legend_order <- c(2019:2020)
 legend_colour<-c( "Red","Green")
 legend("topleft", legend = legend_order, bty = "n",
        col = legend_colour, pch=19, cex = 0.85, ncol = 2)
 dev.off()
 
 ## H ###
 H_2019 <- selectByDate(dfex_month,year = 2019)[,c(1,7)]
H_2020 <- selectByDate(dfex_month,year = 2020)[,c(1,7)]
 H_2019$date <- format(as.POSIXct(H_2019$date),"%m")
 H_2020$date <- format(as.POSIXct(H_2020$date),"%m")
 
 jpeg(filename='fig/H_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
 par(mar=c(4,4,1,1),xpd=FALSE)
 plot(H_2019$date, H_2019$H,  type = 'l', xlab = '', ylab = '', ylim = c(0,3),col = "Red", xaxt = "n")
 points(H_2019$date,H_2019$H, pch = 19, col = "Red")
 points(H_2020$date,H_2020$H, pch = 19, col = "Green")
 lines(H_2020$date,H_2020$H, pch = 19, col = "Green")
 axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
# mtext('Months', side = 1, line = 2.5)
 mtext(expression(paste('H', sep = "")), side = 2, line = 2)
 legend_order <- c(2019:2020)
 legend_colour<-c( "Red","Green")
 legend("topleft", legend = legend_order, bty = "n",
        col = legend_colour, pch=19, cex = 0.85, ncol = 2)
 dev.off()
 
 ##U ##
 U_2019 <- selectByDate(dfex_month,year = 2019)[,c(1,3)]
 U_2020 <- selectByDate(dfex_month,year = 2020)[,c(1,3)]
 U_2019$date <- format(as.POSIXct(U_2019$date),"%m")
 U_2020$date <- format(as.POSIXct(U_2020$date),"%m")
 
 
 jpeg(filename='fig/U_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
 par(mar=c(4,4,1,1),xpd=FALSE)
 plot(U_2019$date, U_2019$WS,  type = 'l', xlab = '', ylab = '', ylim = c(0,2),col = "Red", xaxt = "n")
 points(U_2019$date,U_2019$WS, pch = 19, col = "Red")
 points(U_2020$date,U_2020$WS, pch = 19, col = "Green")
 lines(U_2020$date,U_2020$WS, pch = 19, col = "Green")
 axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
 # mtext('Months', side = 1, line = 2.5)
 mtext(expression(paste('U', sep = "")), side = 2, line = 2)
 legend_order <- c(2019:2020)
 legend_colour<-c( "Red","Green")
 legend("topleft", legend = legend_order, bty = "n",
        col = legend_colour, pch=19, cex = 0.85, ncol = 2)
 dev.off()
 
 
 ## delta e ##
 delta_e_2019 <- selectByDate(dfex_month,year = 2019)[,c(1,20)]
 delta_e_2020 <- selectByDate(dfex_month,year = 2020)[,c(1,20)]
 delta_e_2019$date <- format(as.POSIXct(delta_e_2019$date),"%m")
 delta_e_2020$date <- format(as.POSIXct(delta_e_2020$date),"%m")
 
 
 jpeg(filename='fig/delta_e_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
 par(mar=c(4,4,1,1),xpd=FALSE)
 plot(delta_e_2019$date, delta_e_2019$delta_e,  type = 'l', xlab = '', ylab = '', ylim = c(-0.5,2),col = "Red", xaxt = "n")
 points(delta_e_2019$date,delta_e_2019$delta_e, pch = 19, col = "Red")
 points(delta_e_2020$date,delta_e_2020$delta_e, pch = 19, col = "Green")
 lines(delta_e_2020$date,delta_e_2020$delta_e, pch = 19, col = "Green")
 axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
 # mtext('Months', side = 1, line = 2.5)
 mtext(expression(paste('delta_e', sep = "")), side = 2, line = 2)
 legend_order <- c(2019:2020)
 legend_colour<-c( "Red","Green")
 legend("topleft", legend = legend_order, bty = "n",
        col = legend_colour, pch=19, cex = 0.85, ncol = 2)
 dev.off()
 
 
 ## delta t ##
 delta_t_2019 <- selectByDate(dfex_month,year = 2019)[,c(1,21)]
 delta_t_2020 <- selectByDate(dfex_month,year = 2020)[,c(1,21)]
 delta_t_2019$date <- format(as.POSIXct(delta_t_2019$date),"%m")
 delta_t_2020$date <- format(as.POSIXct(delta_t_2020$date),"%m")
 
 
 jpeg(filename='fig/delta_t_years.jpg', unit = 'cm', width = 10, height = 10, res = 360)
 par(mar=c(4,4,1,1),xpd=FALSE)
 plot(delta_t_2019$date, delta_t_2019$delta_T,  type = 'l', xlab = '', ylab = '', ylim = c(-5,7),col = "Red", xaxt = "n")
 points(delta_t_2019$date,delta_t_2019$delta_T, pch = 19, col = "Red")
 points(delta_t_2020$date,delta_t_2020$delta_T, pch = 19, col = "Green")
 lines(delta_t_2020$date,delta_t_2020$delta_T, pch = 19, col = "Green")
 axis(side = 1, at = c(1:12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), las = 2, cex = 0.5)
 # mtext('Months', side = 1, line = 2.5)
 mtext(expression(paste('delta_e', sep = "")), side = 2, line = 2)
 legend_order <- c(2019:2020)
 legend_colour<-c( "Red","Green")
 legend("topleft", legend = legend_order, bty = "n",
        col = legend_colour, pch=19, cex = 0.85, ncol = 2)
 dev.off()
 
 #####LE &  U DELTA_E  ####
 jpeg(filename='FIG/LE& U_delta_e.jpg', unit = 'cm', width = 10, height = 10, res = 360)
 par(mar = c(4,4,1,1))
 plot((df$delta_e & df$wind_speed) ,df$LE,pch = 19,cex= 0.5, xlab= "", ylab="", ylim = c(-10,80),
      xlim = c(0,2))
 abline(lm(df$LE ~ df$delta_e), col = "red")
 abline(lm(df$LE ~ df$wind_speed), col = "blue")
 

 
 mtext(side = 1, text = expression(paste('delta_e&U', sep = "")),line = 2)
 mtext(side = 2, text = expression(paste('LE', sep = )),line = 2)
 dev.off()
 
 

 
 