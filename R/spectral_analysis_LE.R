
#df_LE <- data.frame(date = df$date, LE = df$LE)
df_LE <- data.frame(LE = df$LE)

# Convert data to a time series object
df_ts <- ts(df_LE, frequency = 1)

# Identify missing values
missing_values <- is.na(df_ts)

# Install and load the 'zoo' package if not already installed
# install.packages("zoo")
library(zoo)

# Fill missing values with linear interpolation
df_ts_filled <- na.approx(df_ts)

# Identify missing values
missing_values_filled <- is.na(df_ts_filled)


# Perform spectral analysis on the filled time series
df_ts_filled_spectrum <- spec.pgram(df_ts_filled)

# Plot the spectrum
plot(df_ts_filled_spectrum, main = "Spectral Analysis (with Missing Values Filled)")

# Find dominant frequency and its power
dominant_frequency <- df_ts_filled_spectrum$freq[which.max(df_ts_filled_spectrum$spec)]
dominant_power <- max(df_ts_filled_spectrum$spec)

# Set a threshold (adjust this value as needed)
threshold <- 1000  # Example threshold value

# Extract frequency and power values from the periodogram
frequencies <- df_ts_filled_spectrum$freq
powers <- df_ts_filled_spectrum$spec

# Identify peaks above the threshold
significant_peaks <- which(powers > threshold)

# Extract frequency and power values of significant peaks
significant_peak_frequencies <- frequencies[significant_peaks]
significant_peak_powers <- powers[significant_peaks]

# Print or plot significant peaks
cat("Significant Peaks:\n")
for (i in 1:length(significant_peak_frequencies)) {
  cat("Frequency:", significant_peak_frequencies[i], "cycles per unit time,", "Power:", significant_peak_powers[i], "\n")
}

# Plot the periodogram
plot(df_ts_filled_spectrum, main = "Periodogram with Significant Peaks")

# Add markers for significant peaks
abline(h = threshold, col = "red", lty = 2)
points(significant_peak_frequencies, significant_peak_powers, pch = 19, col = "blue")


# Smooth the periodogram using the Savitzky-Golay filter
smoothed_spectrum <- smooth.spline(df_ts_filled_spectrum$freq, 
                                   df_ts_filled_spectrum$spec, 
                                   spar = 0.3)  # Adjust spar as needed

# Plot the smoothed periodogram
plot(smoothed_spectrum, type = "l", main = "Smoothed Periodogram")

df_prominence <- data.frame(x = smoothed_spectrum$x, y = smoothed_spectrum$y)

your_data <- df_prominence

# Find local maxima in the data
local_maxima <- which(diff(sign(diff(your_data$y))) == -2) + 1

# Extract the values of local maxima
peak_values <- your_data$y[local_maxima]


# Set a prominence threshold (adjust this value as needed)
prominence_threshold <- 0.5  # Example threshold value

# Identify significant peaks based on the threshold
significant_peaks <- local_maxima[sapply(local_maxima, function(peak_index) {
  calculate_prominence(your_data$y, peak_index) > prominence_threshold
})]


# Create a plot of the original data
plot(your_data, type = "l", main = "Prominence-Based Peak Detection")

# Add markers for significant peaks
abline(v = your_data$x[significant_peaks], col = "red", lty = 2)

# Add frequency labels to the significant peaks
for (peak_position in significant_peaks) {
  frequency_label <- your_data$x[peak_position]  # Replace with the correct way to get the frequency value
  text(x = your_data$x[peak_position], y = max(your_data$y), 
       labels = format(frequency_label, nsmall = 4), pos = 1, col = "red")
}

1 / (your_data$x[significant_peaks] / 30) / 60 / 24
