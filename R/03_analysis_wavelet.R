library(WaveletComp)
library(zoo)

#load('data/student_haitem/exported/MY-MKH_30min_2022-03_2022-09_2023-09-13.RData')


#### Convert data to a time series object ####
df_ts <- ts(df$LE, frequency = 0.5)
df_ts1 <- ts(df$wind_speed, frequency = 0.5)
df_ts2 <- ts(df$delta_e, frequency = 0.5)
df_ts3 <- ts(df$U_deltaE, frequency = 0.5)
df_ts4 <- ts(df$wind_dir, frequency = 0.5)
df_ts5 <- ts(df$H, frequency = 0.5)
df_ts6 <- ts(df$delta_T, frequency = 0.5)
df_ts7 <- ts(df$U_deltaT,frequency = 0.5)


#### Fill missing values with linear interpolation ####
df_ts_filled <- na.approx(df_ts)
df_ts_filled1 <- na.approx(df_ts1)
df_ts_filled2 <- na.approx(df_ts2)
df_ts_filled3 <- na.approx(df_ts3)
df_ts_filled4 <- na.approx(df_ts4)
df_ts_filled5 <- na.approx(df_ts4)
df_ts_filled6 <- na.approx(df_ts4)
df_ts_filled7 <- na.approx(df_ts4)

# To make the data have the same length
df_ts_filled1 <- df_ts_filled1[-length(df_ts_filled1)]
df_ts_filled2 <- df_ts_filled2[-length(df_ts_filled2)]
df_ts_filled3 <- df_ts_filled3[-length(df_ts_filled3)]
df_ts_filled4 <- df_ts_filled4[-length(df_ts_filled4)]
df_ts_filled5 <- df_ts_filled5[-length(df_ts_filled5)]
df_ts_filled6 <- df_ts_filled6[-length(df_ts_filled6)]
df_ts_filled7 <- df_ts_filled7[-length(df_ts_filled7)]

# # Without interpolation
# df_ts_filled <- df_ts
# df_ts_filled1 <- df_ts1
# df_ts_filled2 <- df_ts2
# df_ts_filled3 <- df_ts3
# df_ts_filled4 <- df_ts4

# plot(df_ts_filled)
# plot(df_ts_filled1)
# plot(df_ts_filled2)
# plot(df_ts_filled3)
# plot(df_ts_filled4)


#### Filled the data with a datetime column
series.length <- length(df_ts_filled)

my.date <- seq(as.POSIXct("2022-03-01 00:00:00", format = "%F %T"), 
               by = "30 min", 
               length.out = series.length)

my.data <- data.frame(date = my.date, LE = df_ts_filled, U = df_ts_filled1,
                      delta_e = df_ts_filled2, U_deltaE = df_ts_filled3,
                      WD = df_ts_filled4, H = df_ts_filled5, delta_t = df_ts_filled6,
                      U_deltaT = df_ts_filled7)


#### Coherence ####

## Computation of cross-wavelet power and wavelet coherence, x over y:
## a natural choice of 'dt' in the case of hourly data is 'dt = 1/24',
## resulting in one time unit equaling one day. 
## This is also the time unit in which periods are measured.
## There is an option to store the date format and time zone as additional
## parameters within object 'my.wc' for later reference. 

##### LE and deltae ####

## Note:                           
## By default, Bartlett windows are used for smoothing in order to obtain
## the wavelet coherence spectrum; window lengths in this example:
## 1*24 + 1 = 25 observations in time direction,
## (1/2)*20 + 1 = 11 units in scale (period) direction. 

my.wc <- analyze.coherency(my.data, c("LE","delta_e"), 
                           loess.span = 0, 
                           dt = 1/48, dj = 1/20, 
                           window.size.t = 1, window.size.s = 1/2, 
                           lowerPeriod = 1/4,
                           make.pval = TRUE, n.sim = 10,
                           date.format = "%F %T", date.tz = "")

# Modify the x-axis label to be empty
my.wc$layout$xlabel <- NULL

## The same plot, now with calendar axis
## (according to date format stored in 'my.wc'):
jpeg(filename='R/fig/wavelet_LE_deltae.jpg', unit = 'cm', width = 18, 
     height = 12, res = 400)
par(mar = c(4,4,1,1))
wc.image(my.wc, main = "",
         legend.params = list(lab = "Cross-Wavelet Power Levels"),
         periodlab = "Period (Days)", show.date = TRUE)   
dev.off()

## Plot of average cross-wavelet power:
wc.avg(my.wc, siglvl = 0.05, sigcol = 'red', 
       periodlab = "period (days)")

## Plot of wavelet coherence 
## (with color breakpoints according to quantiles):
jpeg(filename='R/fig/wavelet2_LE_deltae.jpg', unit = 'cm', width = 18, 
     height = 12, res = 400)
par(mar = c(4,4,1,1))
wc.image(my.wc, which.image = "wc",  main = " ", 
         legend.params = list(lab = "Wavelet Coherence Levels", 
                              lab.line = 3.5, label.digits = 3),
         periodlab = "Period (Days)", show.date = TRUE)
dev.off()
## plot of average coherence:
wc.avg(my.wc, which.avg = "wc", 
       siglvl = 0.05, sigcol = 'red', 
       legend.coords = "topleft", 
       periodlab = "period (days)")

##### LE and U ####

## Note:                           
## By default, Bartlett windows are used for smoothing in order to obtain
## the wavelet coherence spectrum; window lengths in this example:
## 1*24 + 1 = 25 observations in time direction,
## (1/2)*20 + 1 = 11 units in scale (period) direction. 

my.wc <- analyze.coherency(my.data, c("LE","U"), 
                           loess.span = 0, 
                           dt = 1/48, dj = 1/20, 
                           window.size.t = 1, window.size.s = 1/2, 
                           lowerPeriod = 1/4,
                           make.pval = TRUE, n.sim = 10,
                           date.format = "%F %T", date.tz = "")

# Modify the x-axis label to be empty
my.wc$layout$xlabel <- NULL

## The same plot, now with calendar axis
## (according to date format stored in 'my.wc'):
jpeg(filename='R/fig/wavelet_LE_U.jpg', unit = 'cm', width = 18, 
     height = 12, res = 400)
par(mar = c(4,4,1,1))
wc.image(my.wc, main = "",
         legend.params = list(lab = "Cross-Wavelet Power Levels"),
         periodlab = "Period (Days)", show.date = TRUE)   
dev.off()

## Plot of average cross-wavelet power:
wc.avg(my.wc, siglvl = 0.05, sigcol = 'red', 
       periodlab = "period (days)")

## Plot of wavelet coherence 
## (with color breakpoints according to quantiles):
jpeg(filename='R/fig/wavelet2_LE_U.jpg', unit = 'cm', width = 18, 
     height = 12, res = 400)
par(mar = c(4,4,1,1))
wc.image(my.wc, which.image = "wc",  main = " ", 
         legend.params = list(lab = "Wavelet Coherence Levels", 
                              lab.line = 3.5, label.digits = 3),
         periodlab = "Period (Days)", show.date = TRUE)
dev.off()
## plot of average coherence:
wc.avg(my.wc, which.avg = "wc", 
       siglvl = 0.05, sigcol = 'red', 
       legend.coords = "topleft", 
       periodlab = "period (days)")


##### H and deltaT ####

## Note:                           
## By default, Bartlett windows are used for smoothing in order to obtain
## the wavelet coherence spectrum; window lengths in this example:
## 1*24 + 1 = 25 observations in time direction,
## (1/2)*20 + 1 = 11 units in scale (period) direction. 

my.wc <- analyze.coherency(my.data, c("H","delta_t"), 
                           loess.span = 0, 
                           dt = 1/48, dj = 1/20, 
                           window.size.t = 1, window.size.s = 1/2, 
                           lowerPeriod = 1/4,
                           make.pval = TRUE, n.sim = 10,
                           date.format = "%F %T", date.tz = "")

# Modify the x-axis label to be empty
my.wc$layout$xlabel <- NULL

## The same plot, now with calendar axis
## (according to date format stored in 'my.wc'):
jpeg(filename='R/fig/wavelet_H_deltaT.jpg', unit = 'cm', width = 18, 
     height = 12, res = 400)
par(mar = c(4,4,1,1))
wc.image(my.wc, main = "",
         legend.params = list(lab = "Cross-Wavelet Power Levels"),
         periodlab = "Period (Days)", show.date = TRUE)   
dev.off()

## Plot of average cross-wavelet power:
wc.avg(my.wc, siglvl = 0.05, sigcol = 'red', 
       periodlab = "period (days)")

## Plot of wavelet coherence 
## (with color breakpoints according to quantiles):
jpeg(filename='R/fig/wavelet2_LE_deltaT.jpg', unit = 'cm', width = 18, 
     height = 12, res = 400)
par(mar = c(4,4,1,1))
wc.image(my.wc, which.image = "wc",  main = " ", 
         legend.params = list(lab = "Wavelet Coherence Levels", 
                              lab.line = 3.5, label.digits = 3),
         periodlab = "Period (Days)", show.date = TRUE)
dev.off()
## plot of average coherence:
wc.avg(my.wc, which.avg = "wc", 
       siglvl = 0.05, sigcol = 'red', 
       legend.coords = "topleft", 
       periodlab = "period (days)")

##### H and U ####

## Note:                           
## By default, Bartlett windows are used for smoothing in order to obtain
## the wavelet coherence spectrum; window lengths in this example:
## 1*24 + 1 = 25 observations in time direction,
## (1/2)*20 + 1 = 11 units in scale (period) direction. 

my.wc <- analyze.coherency(my.data, c("H","U"), 
                           loess.span = 0, 
                           dt = 1/48, dj = 1/20, 
                           window.size.t = 1, window.size.s = 1/2, 
                           lowerPeriod = 1/4,
                           make.pval = TRUE, n.sim = 10,
                           date.format = "%F %T", date.tz = "")

# Modify the x-axis label to be empty
my.wc$layout$xlabel <- NULL

## The same plot, now with calendar axis
## (according to date format stored in 'my.wc'):
jpeg(filename='R/fig/wavelet_H_U.jpg', unit = 'cm', width = 18, 
     height = 12, res = 400)
par(mar = c(4,4,1,1))
wc.image(my.wc, main = "",
         legend.params = list(lab = "Cross-Wavelet Power Levels"),
         periodlab = "Period (Days)", show.date = TRUE)   
dev.off()

## Plot of average cross-wavelet power:
wc.avg(my.wc, siglvl = 0.05, sigcol = 'red', 
       periodlab = "period (days)")

## Plot of wavelet coherence 
## (with color breakpoints according to quantiles):
jpeg(filename='R/fig/wavelet2_LE_U.jpg', unit = 'cm', width = 18, 
     height = 12, res = 400)
par(mar = c(4,4,1,1))
wc.image(my.wc, which.image = "wc",  main = " ", 
         legend.params = list(lab = "Wavelet Coherence Levels", 
                              lab.line = 3.5, label.digits = 3),
         periodlab = "Period (Days)", show.date = TRUE)
dev.off()
## plot of average coherence:
wc.avg(my.wc, which.avg = "wc", 
       siglvl = 0.05, sigcol = 'red', 
       legend.coords = "topleft", 
       periodlab = "period (days)")

#### Single-Parameter Wavelet ####

my.wt <- analyze.wavelet(my.data, "LE", 
                         loess.span = 0, 
                         dt = 1/48, dj = 1/20, 
                         lowerPeriod = 1/4, 
                         make.pval = TRUE, n.sim = 10,
                         date.format = "%F %T", date.tz = "")

## Plot of wavelet power spectrum (with equidistant color breakpoints):  
wt.image(my.wt, color.key = "interval", main = "wavelet power spectrum",
         legend.params = list(lab = "wavelet power levels"),
         periodlab = "period (days)")

## Plot of average wavelet power:
wt.avg(my.wt, siglvl = 0.05, sigcol = "red", 
       periodlab = "period (days)")
