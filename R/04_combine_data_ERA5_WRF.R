library(openair)
library(lubridate)


# Import the data
ERA5 <- read.csv('data/ERA5 Hourly Data with local time.csv',sep=',')
WRF <- read.csv('data/WRF_LE_H_data_2022.csv',sep=',')
# Format the time
date <- strptime(ERA5$Local.Time.in.Malaysia, 
                 format = "%m/%d/%y %H:%M",tz='Asia/Kuala_Lumpur')
ERA5 <- cbind(date,ERA5)


# The date in the WRF data is inconsistent. We have to generate a date sequence.

n <- nrow(WRF)

start_date <- as.POSIXct("2022-01-13 00:00:00", tz = "Asia/Kuala_Lumpur")
date_seq <- seq(from = start_date, by = "hour", length.out = n)


WRF <- cbind(date = date_seq, WRF)
rm(date)
WRF <- WRF[,-2]

# Averaging the data
ERA5_day <- timeAverage(ERA5, avg.time = 'day')
WRF_day <- timeAverage(WRF, avg.time = 'day')

ERA5_month <- timeAverage(ERA5, avg.time = '1 month')
WRF_month <- timeAverage(WRF, avg.time = '1 month')

# Merge the day-data
df_merged_day <- merge(df_day, ERA5_day, by = 'date')
df_merged_day <- merge(df_merged_day, WRF_day, by = 'date')

# Merge the month-data
df_merged_month <- merge(df_month, ERA5_month, by = 'date')
df_merged_month <- merge(df_merged_month, WRF_month, by = 'date')


# Pull the data out
write.table(df_merged_day, file = 'data/df_merged_day.csv', sep = ',')
write.table(df_merged_month, file = 'data/df_merged_month.csv', sep = ',')
