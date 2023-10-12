library(ggplot2)

#### ggplot ####

# write.csv(df, "myfile2.csv", row.names = FALSE)

jpeg(filename='FIG/ggplot_LE_U.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(wind_speed, LE)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'grey',alpha=0, method = "lm") +
  xlab('U ') + coord_cartesian(xlim=c(0,2.5), ylim=c(-12,50)) +
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_LE_udeltae.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(U_deltaE, LE)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'grey',alpha=0, method = "lm") +
  xlab('U delta e') + coord_cartesian(xlim=c(0,1.7), ylim=c(-12,50)) +
  theme_bw()
dev.off()

jpeg(filename='FIG/ggplot_LE_deltae.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(delta_e, LE)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'grey',alpha=0, method = "lm") +
  xlab('Delta e ') + coord_cartesian(xlim=c(0,1.3), ylim=c(-12,50)) +
  theme_bw()
dev.off()

jpeg(filename='FIG/ggplot_LE_ustar.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(`u*`,`LE`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'grey',alpha=0, method = "lm") +
  coord_cartesian(xlim=c(0,0.2), ylim=c(-12,50))+
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_H_udeltat.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(U_deltaT, H)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'grey', method = "lm") +
  xlab('U delta T') + coord_cartesian(xlim=c(0,2.5), ylim=c(-3,13)) +
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_H_U .jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(wind_speed, H)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'grey',method = "lm") +
  xlab('U ') + coord_cartesian(xlim=c(0,2.5), ylim=c(-3,13)) +
  theme_bw()
dev.off()

jpeg(filename='FIG/ggplot_H_deltat.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(delta_T, H)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'grey', method = "lm") +
  xlab('ΔT') + coord_cartesian(xlim=c(-3.6,2.5), ylim=c(-3,13)) +
  theme_bw()
dev.off()

jpeg(filename='FIG/ggplot_H_ustar.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(`u*`,`H`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'grey',alpha=0, method = "lm") +
  coord_cartesian(xlim=c(0,0.15), ylim=c(-7,20))+
  theme_bw()
dev.off()


#### ustar ####

jpeg(filename='FIG/ggplot_ustar_U.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(wind_speed,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_ustar_deltae.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(delta_e,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_ustar_deltat.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(delta_T,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  theme_bw()
dev.off()

jpeg(filename='FIG/ggplot_ustar_udeltae.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(U_deltaE,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  theme_bw()
dev.off()


jpeg(filename='FIG/ggplot_ustar_udeltat.jpg', unit = 'cm', width = 10, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(U_deltaT,`u*`)) + geom_point(alpha=0.2) + 
  geom_smooth(color = 'green',alpha=0, method = "lm") +
  theme_bw()
dev.off()

#### time series ####

#### LE ####

library(ggplot2)

# Filter the data based on date range
filtered_data <- df[df$date >= "2022-09-01" & df$date <= "2022-09-30", ]

# Convert DateTime column to a proper date-time format
filtered_data$DateTime <- as.POSIXct(filtered_data$date, format = "%m/%d/%Y %H:%M")

# Plot the time series
ggplot(filtered_data, aes(x = DateTime, y = LE)) +
  geom_line() +
  labs(title = "LE Time Series (September 2022)",
       x = "Date Time",
       y = "LE") +
  theme_minimal()

#### H ####

# Filter the data based on date range
filtered_data <- df[df$date >= "2022-03-01" & df$date <= "2022-03-31", ]

# Convert DateTime column to a proper date-time format
filtered_data$DateTime <- as.POSIXct(filtered_data$date, format = "%m/%d/%Y %H:%M")

# Plot the time series
ggplot(filtered_data, aes(x = DateTime, y = H)) +
  geom_line() +
  labs(title = "H Time Series (March 2022)",
       x = "Date Time",
       y = "H") +
  theme_minimal()

#### LE ####

jpeg(filename='FIG/ggplot_LE.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_day, aes(df_day$date, df_day$LE)) + geom_point() +
  geom_smooth(aes(df_day$date,df_day$LE), 
              col = 'grey', alpha = 0.3) +
  labs(x = "Date", y = "LE") +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(0,20) 
dev.off()

#### U ####

jpeg(filename='FIG/ggplot_U.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_day, aes(df_day$date, df_day$wind_speed)) + geom_point() +
  geom_smooth(aes(df_day$date,df_day$wind_speed), 
              col = 'grey', alpha = 0.3) +
  labs(x = "Date", y = "wind_speed") +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(0,1) 
dev.off()

#### H ####


jpeg(filename='FIG/ggplot_H.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_day, aes(df_day$date, df_day$H)) + geom_point() +
  geom_smooth(aes(df_day$date,df_day$H), 
              col = 'grey', alpha = 0.3) +
  labs(x = "Date", y = "H") +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(0,5) 
dev.off()

#### deltae ####


jpeg(filename='FIG/ggplot_deltae.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_day, aes(df_day$date, df_day$delta_e)) + geom_point() +
  geom_smooth(aes(df_day$date,df_day$delta_e), 
              col = 'grey', alpha = 0.3) +
  labs(x = "Date", y = "deltae") +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(0,1.05) 
dev.off()


#### deltat ####


jpeg(filename='FIG/ggplot_deltat.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_day, aes(df_day$date, df_day$delta_T)) + geom_point() +
  geom_smooth(aes(df_day$date,df_day$delta_T), 
              col = 'grey', alpha = 0.3) +
  labs(x = "Date", y = "deltae_T") +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(-2,3) 
dev.off()


#### udeltae ####


jpeg(filename='FIG/ggplot_udeltae.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_day, aes(df_day$date, df_day$U_deltaE)) + geom_point() +
  geom_smooth(aes(df_day$date,df_day$U_deltaE), 
              col = 'grey', alpha = 0.3) +
  labs(x = "Date", y = "udeltae") +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(0,1) 
dev.off()



#### udeltat ####


jpeg(filename='FIG/ggplot_udeltat.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_day, aes(df_day$date, df_day$U_deltaT)) + geom_point() +
  geom_smooth(aes(df_day$date,df_day$U_deltaT), 
              col = 'grey', alpha = 0.3) +
  labs(x = "Date", y = "udeltat") +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(-1.5,1.5) 
dev.off()






jpeg(filename='FIG/ggplot_LE.jpg', unit = 'cm', width = 15, height = 10, res = 360)
par(mar = c(4,4,1,1))
ggplot(df_day, aes(df_day$date, df_day$LE)) + geom_point() +
  geom_smooth(aes(df_day$date,df_day$LE), 
              col = 'grey', alpha = 0.3) +
  labs(x = "Date", y = "LE") +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(0,20) 
dev.off()



#### time series with all months LE  ####


plot1 <- ggplot(df_day, aes(as.Date(date), LE)) + 
  geom_line() + 
  geom_smooth(col = 'blue', alpha = 0.3) +
  ylim(0,20) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
  labs(x = " ", y = "LE") + theme_bw()

plot1


#### H ####

ggplot(df_day, aes(as.Date(date), H)) + 
  geom_line() + 
  geom_smooth(col = 'blue', alpha = 0.3) +
  ylim(0,5) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
  labs(x = " ", y = "H") + theme_bw()


#### U ####


ggplot(df_day, aes(as.Date(date), wind_speed)) + 
  geom_line() + 
  geom_smooth(col = 'blue', alpha = 0.3) +
  ylim(0,1) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
  labs(x = " ", y = "win_speed") + theme_bw()


#### deltae ####


ggplot(df_day, aes(as.Date(date), delta_e)) + 
  geom_line() + 
  geom_smooth(col = 'blue', alpha = 0.3) +
  ylim(0,1.05) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
  labs(x = " ", y = "deltae") + theme_bw()



#### deltat ####


ggplot(df_day, aes(as.Date(date), delta_T)) + 
  geom_line() + 
  geom_smooth(col = 'blue', alpha = 0.3) +
  ylim(-2,3) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
  labs(x = " ", y = "delta_T") + theme_bw()


#### Udeltae ####

ggplot(df_day, aes(as.Date(date), U_deltaE)) + 
  geom_line() + 
  geom_smooth(col = 'blue', alpha = 0.3) +
  ylim(0,1) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
  labs(x = " ", y = "udeltae") + theme_bw()


#### Udeltat ####

ggplot(df_day, aes(as.Date(date), U_deltaT)) + 
  geom_line() + 
  geom_smooth(col = 'blue', alpha = 0.3) +
  ylim(-1,1) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
  labs(x = " ", y = "udeltat") + theme_bw()
