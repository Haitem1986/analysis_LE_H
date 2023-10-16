#library(dplyr)
library(gridExtra)
library(ggtext)
library(ggplot2)

#### Daily-averaged time series for all months ####
# 
 ##### LE ####
# jpeg(filename='R/fig/ggplot_day_LE.jpg', unit = 'cm', width = 18, 
#      height = 12, res = 360)
# par(mar = c(4,4,1,1))
# plot1 <- ggplot(df_day, aes(as.Date(date), LE)) + 
#   geom_line() + 
#   geom_smooth(col = 'navy', alpha = 0.3) +
#   ylim(0,20) +
#   scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
#   labs(x = " ", y = "LE") + theme_bw()
# plot1
# dev.off()
# 
 ##### H ####
# jpeg(filename='R/fig/ggplot_day_H.jpg', unit = 'cm', width = 18, 
#      height = 12, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df_day, aes(as.Date(date), H)) + 
#   geom_line() + 
#   geom_smooth(col = 'navy', alpha = 0.3) +
#   ylim(0,5) +
#   scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
#   labs(x = " ", y = "H") + theme_bw()
# dev.off()
# 
 ##### U ####
# 
# jpeg(filename='R/fig/ggplot_day_U.jpg', unit = 'cm', width = 18, 
#      height = 12, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df_day, aes(as.Date(date), wind_speed)) + 
#   geom_line() + 
#   geom_smooth(col = 'navy', alpha = 0.3) +
#   ylim(0,1) +
#   scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
#   labs(x = " ", y = "U") + theme_bw()
# dev.off()
# 
 ##### deltae ####
# 
# jpeg(filename='R/fig/ggplot_day_deltae.jpg', unit = 'cm', width = 18, 
#      height = 12, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df_day, aes(as.Date(date), delta_e)) + 
#   geom_line() + 
#   geom_smooth(col = 'navy', alpha = 0.3) +
#   ylim(0,1.05) +
#   scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
#   labs(x = " ", y = expression(Δ ~ e)) + theme_bw()
# dev.off()
# 
# 
 ##### deltat ####
# 
# jpeg(filename='R/fig/ggplot_day_deltat.jpg', unit = 'cm', width = 18, 
#      height = 12, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df_day, aes(as.Date(date), delta_T)) + 
#   geom_line() + 
#   geom_smooth(col = 'navy', alpha = 0.3) +
#   ylim(-2,3) +
#   scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
#   labs(x = " ", y = expression(Δ ~ T)) + theme_bw()
# dev.off()
# 
 ##### Udeltae ####
# jpeg(filename='R/fig/ggplot_day_udeltae.jpg', unit = 'cm', width = 18, 
#      height = 12, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df_day, aes(as.Date(date), U_deltaE)) + 
#   geom_line() + 
#   geom_smooth(col = 'navy', alpha = 0.3) +
#   ylim(0,1) +
#   scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
#   labs(x = " ", y = expression(U ~ Δ ~ e)) + theme_bw()
# dev.off()
# 
 ##### Udeltat ####
# 
# jpeg(filename='R/fig/ggplot_day_udeltat.jpg', unit = 'cm', width = 18, 
#      height = 12, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df_day, aes(as.Date(date), U_deltaT)) + 
#   geom_line() + 
#   geom_smooth(col = 'navy', alpha = 0.3) +
#   ylim(-1,1) +
#   scale_x_date(labels=scales::date_format("%b %Y"), breaks = "1 month") +
#   labs(x = " ", y = expression(U ~ Δ ~ T)) + theme_bw()
# dev.off()

#### Half-hourly-averaged time series for all months ####

##### LE ####

jpeg(filename='R/fig/ggplot_LE.jpg', unit = 'cm', width = 15, 
     height = 10, res = 360)
par(mar = c(4,4,1,1))

df1 <- df
df1$LE[is.na(df1$LE)] <- NA

# main plot
mainP <- ggplot(df1, aes(date, LE)) + geom_point(col="grey50", shape=3) +
  labs(x = "", y = bquote(paste("LE ", "(W ", m^-2, ")"))) +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(-10,150) +
  scale_x_datetime(labels=scales::date_format("%b %Y"), breaks = "1 month") 

# inset plot
df_hourly_avg <- df %>%
  filter(!is.na(date)) %>%
  group_by(hour = as.factor(hour(date))) %>%
  summarise(LE = mean(LE, na.rm = TRUE))

insetP <- ggplot(df_hourly_avg, aes(as.integer(hour),LE)) + geom_point(color='grey50', shape=3) +
  geom_smooth(span = 0.5, alpha = 0, col = 'navy') + 
  labs(x = "Hour", y = "") +
  theme_bw() + xlim(0,23)

plot1 <- ggplot(df_day, aes(as.Date(date), LE)) + 
  geom_point(col="grey50", shape=3) + 
  geom_smooth(col = 'navy', alpha = 0.3) +
  ylim(0,20) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "3 month") +
  labs(x = " ", y = " ") + theme_bw()

# combined plot

# Create the main plot (mainP) and inset plot (insetP)

# Specify the dimensions of the inset plot
#inset_width <- 0.2  # Adjust the width as needed
inset_height <- 50  # Adjust the height as needed

# Calculate the x and y positions for the inset
x_positionMin <- as.POSIXct("2022-02-10 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax <- as.POSIXct("2022-05-20 12:00:00", tz = "Asia/Kuala_Lumpur")
y_positionMin <- max(df$LE, na.rm = TRUE) - 35
y_positionMax <- max(df$LE, na.rm = TRUE) + inset_height

x_positionMin1 <- as.POSIXct("2022-05-20 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax1 <- as.POSIXct("2022-08-30 12:00:00", tz = "Asia/Kuala_Lumpur")


# Add the inset plot to the main plot
mainP1 <- mainP +
  annotation_custom(ggplotGrob(insetP),
                    xmin = x_positionMin, xmax = x_positionMax,
                    ymin = y_positionMin, ymax = y_positionMax) +
  annotation_custom(ggplotGrob(plot1),
                    xmin = x_positionMin1, xmax = x_positionMax1,
                    ymin = y_positionMin, ymax = y_positionMax)

mainP1

dev.off()


##### H ####
jpeg(filename='R/fig/ggplot_H.jpg', unit = 'cm', width = 15, 
     height = 10, res = 360)
par(mar = c(4,4,1,1))

df1 <- df
df1$H[is.na(df1$H)] <- NA

# main plot
mainP <- ggplot(df1, aes(date, H)) + geom_point(color='grey50', shape=3)+
  labs(x = "", y = bquote(paste("H ", "(W ", m^-2, ")"))) +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(-5,20) +
  scale_x_datetime(labels=scales::date_format("%b %Y"), breaks = "1 month") 

# inset plot
df_hourly_avg <- df %>%
  filter(!is.na(date)) %>%
  group_by(hour = as.factor(hour(date))) %>%
  summarise(H = mean(H, na.rm = TRUE))

insetP <- ggplot(df_hourly_avg, aes(as.integer(hour),H)) + geom_point(color='grey50',shape=3) +
  geom_smooth(span = 0.5, alpha = 0, col = 'navy') + 
  labs(x = "Hour", y = "") +
  theme_bw() + xlim(0,23)

plot1 <- ggplot(df_day, aes(as.Date(date), H)) + 
  geom_point(col="grey50", shape=3) + 
  geom_smooth(col = 'navy', alpha = 0.3) +
  ylim(0,5) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "3 month") +
  labs(x = " ", y = "H") + theme_bw()
# combined plot

# Create the main plot (mainP) and inset plot (insetP)

# Specify the dimensions of the inset plot
#inset_width <- 0.2  # Adjust the width as needed
inset_height <- 6  # Adjust the height as needed

# Calculate the x and y positions for the inset
x_positionMin <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax <- as.POSIXct("2022-10-10 12:00:00", tz = "Asia/Kuala_Lumpur")
y_positionMin <- max(df$H, na.rm = TRUE) - 7
y_positionMax <- max(df$H, na.rm = TRUE) + inset_height

x_positionMin1 <- as.POSIXct("2022-03-20 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax1 <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")


# Add the inset plot to the main plot
mainP1 <- mainP +
  annotation_custom(ggplotGrob(insetP),
                    xmin = x_positionMin, xmax = x_positionMax,
                    ymin = y_positionMin, ymax = y_positionMax) +
  annotation_custom(ggplotGrob(plot1),
                    xmin = x_positionMin1, xmax = x_positionMax1,
                    ymin = y_positionMin, ymax = y_positionMax)

mainP1

dev.off()

##### deltae ####
jpeg(filename='R/fig/ggplot_deltae.jpg', unit = 'cm', width = 15, 
     height = 10, res = 360)
par(mar = c(4,4,1,1))

df1 <- df
df1$delta_e[is.na(df1$delta_e)] <- NA
df1$delta_e[df$wind_dir > 45 & df$wind_dir < 315] <- NA

# main plot
mainP <- ggplot(df1, aes(date, delta_e)) + geom_point(color='grey50', shape=3) +
  labs(x = "", y = expression(Δ ~ e ~ (kPa))) +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(0,2.5) +
  scale_x_datetime(labels=scales::date_format("%b %Y"), breaks = "1 month") 

# inset plot
df_hourly_avg <- df1 %>%
  filter(!is.na(date)) %>%
  group_by(hour = as.factor(hour(date))) %>%
  summarise(delta_e = mean(delta_e, na.rm = TRUE))

insetP <- ggplot(df_hourly_avg, aes(as.integer(hour),delta_e)) + 
  geom_point(color='grey50') +
  geom_smooth(span = 0.5, alpha = 0, col = 'navy') + 
  labs(x = "Hour", y = "") +
  theme_bw() + xlim(0, 23)

plot1 <- ggplot(df_day, aes(as.Date(date), delta_e)) + 
  geom_point(col="grey50", shape=3) +
  geom_smooth(col = 'navy', alpha = 0.3) +
  ylim(0,1.05) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "3 month") +
  labs(x = " ", y = expression(Δ ~ e)) + theme_bw()
# combined plot

# Create the main plot (mainP) and inset plot (insetP)

# Specify the dimensions of the inset plot
#inset_width <- 0.2  # Adjust the width as needed
inset_height <- 0.0  # Adjust the height as needed

# Calculate the x and y positions for the inset
x_positionMin <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax <- as.POSIXct("2022-10-10 12:00:00", tz = "Asia/Kuala_Lumpur")
y_positionMin <- max(df$delta_e, na.rm = TRUE) - (inset_height)
y_positionMax <- max(df$delta_e, na.rm = TRUE) + 1.15

x_positionMin1 <- as.POSIXct("2022-03-20 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax1 <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")


# Add the inset plot to the main plot
mainP1 <- mainP +
  annotation_custom(ggplotGrob(insetP),
                    xmin = x_positionMin, xmax = x_positionMax,
                    ymin = y_positionMin, ymax = y_positionMax) +
  annotation_custom(ggplotGrob(plot1),
                    xmin = x_positionMin1, xmax = x_positionMax1,
                    ymin = y_positionMin, ymax = y_positionMax)

mainP1

dev.off()


##### deltat ####
jpeg(filename='R/fig/ggplot_deltat.jpg', unit = 'cm', width = 15, 
     height = 10, res = 360)
par(mar = c(4,4,1,1))

df1 <- df
df1$delta_T[is.na(df1$delta_T)] <- NA
df1$delta_T[df$wind_dir > 45 & df$wind_dir < 315] <- NA

# main plot
mainP <- ggplot(df1, aes(date, delta_T)) + geom_point(color='grey50', shape=3) +
  labs(x = "", y = expression(Δ ~ T ~ (K))) +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(-5,10) +
  scale_x_datetime(labels=scales::date_format("%b %Y"), breaks = "1 month") 

# inset plot
df_hourly_avg <- df1 %>%
  filter(!is.na(date)) %>%
  group_by(hour = as.factor(hour(date))) %>%
  summarise(delta_T = mean(delta_T, na.rm = TRUE))

insetP <- ggplot(df_hourly_avg, aes(as.integer(hour),delta_T)) + 
  geom_point(color='grey50',shape=3) +
  geom_smooth(span = 0.5, alpha = 0, col = 'navy') + 
  labs(x = "Hour", y = "") +
  theme_bw() + xlim(0, 23)

plot1 <- ggplot(df_day, aes(as.Date(date), delta_T)) + 
  geom_point(col="grey50", shape=3) +
  geom_smooth(col = 'navy', alpha = 0.3) +
  ylim(-2,3) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "3 month") +
  labs(x = " ", y = expression(Δ ~ T)) + theme_bw()

# combined plot

# Create the main plot (mainP) and inset plot (insetP)

# Specify the dimensions of the inset plot
#inset_width <- 0.2  # Adjust the width as needed
inset_height <- 6  # Adjust the height as needed

# Calculate the x and y positions for the inset
x_positionMin <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax <- as.POSIXct("2022-10-10 12:00:00", tz = "Asia/Kuala_Lumpur")
y_positionMin <- max(df$delta_T, na.rm = TRUE) - 1
y_positionMax <- max(df$delta_T, na.rm = TRUE) + 5.5

x_positionMin1 <- as.POSIXct("2022-03-20 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax1 <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")


# Add the inset plot to the main plot
mainP1 <- mainP +
  annotation_custom(ggplotGrob(insetP),
                    xmin = x_positionMin, xmax = x_positionMax,
                    ymin = y_positionMin, ymax = y_positionMax) +
  annotation_custom(ggplotGrob(plot1),
                    xmin = x_positionMin1, xmax = x_positionMax1,
                    ymin = y_positionMin, ymax = y_positionMax)

mainP1

dev.off()

##### udeltae ####
jpeg(filename='R/fig/ggplot_udeltae.jpg', unit = 'cm', width = 15, 
     height = 10, res = 360)
par(mar = c(4,4,1,1))

df1 <- df
df1$U_deltaE[is.na(df1$U_deltaE)] <- NA
df1$U_deltaE[df$wind_dir > 45 & df$wind_dir < 315] <- NA

# main plot
mainP <- ggplot(df1, aes(date, U_deltaE)) + geom_point(color='grey50', shape=3) +
  labs(x = "", y = bquote(paste("UΔe ", "(m ", s^-1, " kPa)"))) +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(0,4) +
  scale_x_datetime(labels=scales::date_format("%b %Y"), breaks = "1 month") 

# inset plot
df_hourly_avg <- df1 %>%
  filter(!is.na(date)) %>%
  group_by(hour = as.factor(hour(date))) %>%
  summarise(U_deltaE = mean(U_deltaE, na.rm = TRUE))

insetP <- ggplot(df_hourly_avg, aes(as.integer(hour), U_deltaE)) + 
  geom_point(color='grey50',shape=3) +
  geom_smooth(span = 0.5, alpha = 0, col = 'navy') + 
  labs(x = "Hour", y = "") +
  theme_bw() + xlim(0, 23)

plot1 <- ggplot(df_day, aes(as.Date(date), U_deltaE)) + 
  geom_point(col="grey50", shape=3) + 
  geom_smooth(col = 'navy', alpha = 0.3) +
  ylim(0,1) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "3 month") +
  labs(x = " ", y = expression(U ~ Δ ~ e)) + theme_bw()
# combined plot

# Create the main plot (mainP) and inset plot (insetP)

# Specify the dimensions of the inset plot
#inset_width <- 0.2  # Adjust the width as needed
inset_height <- 0.5  # Adjust the height as needed

# Calculate the x and y positions for the inset
x_positionMin <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax <- as.POSIXct("2022-10-10 12:00:00", tz = "Asia/Kuala_Lumpur")
y_positionMin <- max(df$U_deltaE, na.rm = TRUE) - (inset_height)
y_positionMax <- max(df$U_deltaE, na.rm = TRUE) + 1.5

x_positionMin1 <- as.POSIXct("2022-03-20 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax1 <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")


# Add the inset plot to the main plot
mainP1 <- mainP +
  annotation_custom(ggplotGrob(insetP),
                    xmin = x_positionMin, xmax = x_positionMax,
                    ymin = y_positionMin, ymax = y_positionMax) +
  annotation_custom(ggplotGrob(plot1),
                    xmin = x_positionMin1, xmax = x_positionMax1,
                    ymin = y_positionMin, ymax = y_positionMax)

mainP1

dev.off()

##### udeltaT ####
jpeg(filename='R/fig/ggplot_udeltaT.jpg', unit = 'cm', width = 15, 
     height = 10, res = 360)
par(mar = c(4,4,1,1))

df1 <- df
df1$U_deltaT[is.na(df1$U_deltaT)] <- NA
df1$U_deltaT[df$wind_dir > 45 & df$wind_dir < 315] <- NA

# main plot
mainP <- ggplot(df1, aes(date, U_deltaT)) + geom_point(color='grey50', shape=3) +
  labs(x = "", y = bquote(paste("UΔT ", "(m ", s^-1, " K)"))) +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(-2.5,10) +
  scale_x_datetime(labels=scales::date_format("%b %Y"), breaks = "1 month") 

# inset plot
df_hourly_avg <- df1 %>%
  filter(!is.na(date)) %>%
  group_by(hour = as.factor(hour(date))) %>%
  summarise(U_deltaT = mean(U_deltaT, na.rm = TRUE))

insetP <- ggplot(df_hourly_avg, aes(as.integer(hour), U_deltaT)) + 
  geom_point(color='grey50',shape=3) +
  geom_smooth(span = 0.5, alpha = 0, col = 'navy') + 
  labs(x = "Hour", y = "") +
  theme_bw() + xlim(0, 23)

plot1 <- ggplot(df_day, aes(as.Date(date), U_deltaT)) + 
  geom_point(col="grey50", shape=3) +
  geom_smooth(col = 'navy', alpha = 0.3) +
  ylim(-1,1) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "3 month") +
  labs(x = " ", y = expression(U ~ Δ ~ T)) + theme_bw()
# combined plot

# Create the main plot (mainP) and inset plot (insetP)

# Specify the dimensions of the inset plot
#inset_width <- 0.2  # Adjust the width as needed
inset_height <- 5  # Adjust the height as needed

# Calculate the x and y positions for the inset
x_positionMin <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax <- as.POSIXct("2022-10-10 12:00:00", tz = "Asia/Kuala_Lumpur")
y_positionMin <- max(df$U_deltaT, na.rm = TRUE) - (inset_height)
y_positionMax <- max(df$U_deltaT, na.rm = TRUE) + 0.8

x_positionMin1 <- as.POSIXct("2022-03-20 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax1 <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")


# Add the inset plot to the main plot
mainP1 <- mainP +
  annotation_custom(ggplotGrob(insetP),
                    xmin = x_positionMin, xmax = x_positionMax,
                    ymin = y_positionMin, ymax = y_positionMax) +
  annotation_custom(ggplotGrob(plot1),
                    xmin = x_positionMin1, xmax = x_positionMax1,
                    ymin = y_positionMin, ymax = y_positionMax)

mainP1

dev.off()

##### U ####
jpeg(filename='R/fig/ggplot_U.jpg', unit = 'cm', width = 15, 
     height = 10, res = 360)
par(mar = c(4,4,1,1))

df1 <- df
df1$wind_speed[is.na(df1$wind_speed)] <- NA
df1$wind_speed[df$wind_dir > 45 & df$wind_dir < 315] <- NA

# main plot
mainP <- ggplot(df1, aes(date, wind_speed)) + geom_point(color='grey50', shape=3) +
  labs(x = "", y = bquote(paste("U ", "(m ", s^-1, ")"))) +
  theme_bw() + 
  xlim(as.POSIXct("2022-03-01"), as.POSIXct("2022-09-30")) + 
  ylim(0,5) +
  scale_x_datetime(labels=scales::date_format("%b %Y"), breaks = "1 month") 

# inset plot
df_hourly_avg <- df1 %>%
  filter(!is.na(date)) %>%
  group_by(hour = as.factor(hour(date))) %>%
  summarise(wind_speed = mean(wind_speed, na.rm = TRUE))

insetP <- ggplot(df_hourly_avg, aes(as.integer(hour), wind_speed)) + 
  geom_point(color='grey50',shape=3) +
  geom_smooth(span = 0.5, alpha = 0, col = 'navy') + 
  labs(x = "Hour", y = "") +
  theme_bw() + xlim(0, 23)

plot1 <- ggplot(df_day, aes(as.Date(date), wind_speed)) + 
  geom_point(col="grey50", shape=3) +
  geom_smooth(col = 'navy', alpha = 0.3) +
  ylim(0,1) +
  scale_x_date(labels=scales::date_format("%b %Y"), breaks = "3 month") +
  labs(x = " ", y = "U") + theme_bw()

# combined plot

# Create the main plot (mainP) and inset plot (insetP)

# Specify the dimensions of the inset plot
#inset_width <- 0.2  # Adjust the width as needed
inset_height <- 0.5  # Adjust the height as needed

# Calculate the x and y positions for the inset
x_positionMin <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax <- as.POSIXct("2022-10-10 12:00:00", tz = "Asia/Kuala_Lumpur")
y_positionMin <- max(df$wind_speed, na.rm = TRUE) - (inset_height)
y_positionMax <- max(df$wind_speed, na.rm = TRUE) + 1.5

x_positionMin1 <- as.POSIXct("2022-03-20 12:00:00", tz = "Asia/Kuala_Lumpur")
x_positionMax1 <- as.POSIXct("2022-06-30 12:00:00", tz = "Asia/Kuala_Lumpur")


# Add the inset plot to the main plot
mainP1 <- mainP +
  annotation_custom(ggplotGrob(insetP),
                    xmin = x_positionMin, xmax = x_positionMax,
                    ymin = y_positionMin, ymax = y_positionMax) +
  annotation_custom(ggplotGrob(plot1),
                    xmin = x_positionMin1, xmax = x_positionMax1,
                    ymin = y_positionMin, ymax = y_positionMax)

mainP1

dev.off()

#### Correlation Plots ####

##### LE and U ####
jpeg(filename='R/fig/ggplot_lm_LE_U.jpg', unit = 'cm', width = 12, height = 12, 
     res = 360)
par(mar = c(4,4,1,1))
df_temp <- data.frame(LE = df$LE, wind_speed = df$wind_speed)
df_temp <- na.omit(df_temp)
lm_model <- lm(LE ~ wind_speed, data = df_temp)
ggplot(df_temp, aes(wind_speed, LE)) + 
  geom_point(alpha=0.4, shape = 3) + 
  geom_smooth(color = 'navy', alpha = 0, 
              method = "lm", linetype = "dashed", se = TRUE) +
  ylab(bquote(paste("LE ", "(W ", m^-2, ")"))) +
  xlab(bquote(paste("U ", "(m ", s^-1, ")"))) + 
  coord_cartesian(xlim = c(0,3), ylim = c(-10,150)) +
  theme_bw()
dev.off()

##### LE and deltae ####

jpeg(filename='R/fig/ggplot_lm_LE_deltae.jpg', unit = 'cm', width = 12, height = 12, 
     res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(delta_e, LE)) + geom_point(alpha=0.4,shape=3) + 
  geom_smooth(color = 'navy',alpha=0, method = "lm", 
              se= TRUE, linetype ="dashed") +
  ylab(bquote(paste("LE ", "(W ", m^-2, ")"))) +
  xlab(bquote(paste("Δe ", "(kPa)"))) + 
  coord_cartesian(xlim=c(0,1.25), ylim=c(-10,150)) +
  theme_bw()
dev.off()

##### LE and udeltae ####

jpeg(filename='R/fig/ggplot_lm_LE_udeltae.jpg', unit = 'cm', width = 12, height = 12, res = 360)
par(mar = c(4,4,1,1))
ggplot(df, aes(U_deltaE, LE)) + geom_point(alpha=0.4, shape = 3) + 
  geom_smooth(color = 'navy',alpha=0, method = "lm", 
              se= TRUE, linetype ="dashed") +
  ylab(bquote(paste("LE ", "(W ", m^-2, ")"))) +
  xlab(bquote(paste("UΔe ", "(m ", s^-1, " kPa)"))) +
  coord_cartesian(xlim=c(0,1.8), ylim=c(-10,150)) +
  theme_bw()
dev.off()



# jpeg(filename='FIG/ggplot_LE_ustar.jpg', unit = 'cm', width = 10, height = 10, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df, aes(`u*`,`LE`)) + geom_point(alpha=0.2) + 
#   geom_smooth(color = 'grey',alpha=0, method = "lm") +
#   coord_cartesian(xlim=c(0,0.2), ylim=c(-12,50))+
#   theme_bw()
# dev.off()

##### H and U ####
jpeg(filename='R/fig/ggplot_lm_H_U.jpg', unit = 'cm', width = 12, height = 12, 
     res = 360)
par(mar = c(4,4,1,1))


ggplot(df, aes(wind_speed, H)) + geom_point(alpha=0.4, shape = 3) + 
  geom_smooth(color = 'navy',alpha=0, method = "lm", 
              se= TRUE, linetype ="dashed") +
  ylab(bquote(paste("H ", "(W ", m^-2, ")"))) +
  xlab(bquote(paste("U ", "(m ", s^-1, ")"))) +
  coord_cartesian(xlim=c(0,3), ylim=c(-5,20)) +
  theme_bw()


dev.off()

##### H and deltaT ####

jpeg(filename='R/fig/ggplot_lm_H_deltaT.jpg', unit = 'cm', width = 12, height = 12, 
     res = 360)
par(mar = c(4,4,1,1))

ggplot(df, aes(delta_T, H)) + geom_point(alpha=0.4, shape = 3) + 
  geom_smooth(color = 'navy',alpha=0, method = "lm", 
              se= TRUE, linetype ="dashed") +
  ylab(bquote(paste("H ", "(W ", m^-2, ")"))) +
  xlab(bquote(paste("ΔT ", "(K)"))) +
  coord_cartesian(xlim=c(-3,3), ylim=c(-5,20)) +
  theme_bw()

dev.off()

##### H and udeltaT
jpeg(filename='R/fig/ggplot_lm_H_udeltaT.jpg', unit = 'cm', width = 12, height = 12, 
     res = 360)
par(mar = c(4,4,1,1))

ggplot(df, aes(U_deltaT, H)) + geom_point(alpha=0.4, shape = 3) + 
  geom_smooth(color = 'navy',alpha=0, method = "lm", 
              se= TRUE, linetype ="dashed") +
  ylab(bquote(paste("H ", "(W ", m^-2, ")"))) +
  xlab(bquote(paste("UΔT ", "(m ", s^-1, "K)"))) +
  coord_cartesian(xlim=c(-2.5,8), ylim=c(-5,20)) +
  theme_bw()

dev.off()

# jpeg(filename='FIG/ggplot_H_ustar.jpg', unit = 'cm', width = 10, height = 10, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df, aes(`u*`,`H`)) + geom_point(alpha=0.2) + 
#   geom_smooth(color = 'grey',alpha=0, method = "lm") +
#   coord_cartesian(xlim=c(0,0.15), ylim=c(-7,20))+
#   theme_bw()
# dev.off()


# ##### ustar ####
# 
# jpeg(filename='FIG/ggplot_ustar_U.jpg', unit = 'cm', width = 10, height = 10, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df, aes(wind_speed,`u*`)) + geom_point(alpha=0.2) + 
#   geom_smooth(color = 'green',alpha=0, method = "lm") +
#   theme_bw()
# dev.off()
# 
# 
# jpeg(filename='FIG/ggplot_ustar_deltae.jpg', unit = 'cm', width = 10, height = 10, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df, aes(delta_e,`u*`)) + geom_point(alpha=0.2) + 
#   geom_smooth(color = 'green',alpha=0, method = "lm") +
#   theme_bw()
# dev.off()
# 
# 
# jpeg(filename='FIG/ggplot_ustar_deltat.jpg', unit = 'cm', width = 10, height = 10, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df, aes(delta_T,`u*`)) + geom_point(alpha=0.2) + 
#   geom_smooth(color = 'green',alpha=0, method = "lm") +
#   theme_bw()
# dev.off()
# 
# jpeg(filename='FIG/ggplot_ustar_udeltae.jpg', unit = 'cm', width = 10, height = 10, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df, aes(U_deltaE,`u*`)) + geom_point(alpha=0.2) + 
#   geom_smooth(color = 'green',alpha=0, method = "lm") +
#   theme_bw()
# dev.off()
# 
# 
# jpeg(filename='FIG/ggplot_ustar_udeltat.jpg', unit = 'cm', width = 10, height = 10, res = 360)
# par(mar = c(4,4,1,1))
# ggplot(df, aes(U_deltaT,`u*`)) + geom_point(alpha=0.2) + 
#   geom_smooth(color = 'green',alpha=0, method = "lm") +
#   theme_bw()
# dev.off()