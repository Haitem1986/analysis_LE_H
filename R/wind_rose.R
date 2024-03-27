library(openair)
library(lattice)
library(plyr)
#install.packages("latticeExtra")
library(latticeExtra)

# Define your custom wind speed breaks
custom_ws_breaks <- c(0, 1, 2, 3, 4)

my.statistic <- list("fun"=length,"unit" = "%","scale" = "all", "lab" = "" , 
                     "fun2" = function(x) signif(mean(x, na.rm = TRUE), 3), 
                     "lab2" = "mean","labcalm" = function(x) round(x, 1))




# Create a wind rose plot with custom wind speed breaks
df_windrose <- data.frame(ws = df$wind_speed, wd = df$wind_dir)
df_windrose$ws[which(df_windrose$wd < 270 & df_windrose$wd > 90)] <- NA
wind_rose_plot <- windRose(df_windrose, 
                            paddle = F, breaks = custom_ws_breaks, cols = "jet",
                            angle = 15, angle.scale = 45, statistic = my.statistic)


# Assuming wind_rose_plot is your lattice plot object

# Set the dimensions and resolution
width <- 6  # in inches
height <- 6  # in inches
dpi <- 360  # resolution in dots per inch
file_path <- "R/fig/windrose.jpg"

# Set smaller margins
mar <- c(2, 2, 2, 2)  # c(bottom, left, top, right)

# Open the JPEG device
jpeg(file_path, width = width, height = height, units = "in", res = dpi)
par(mar=mar)
# Plot your lattice plot
print(wind_rose_plot)

# Close the JPEG device
dev.off()





# Customize the appearance using lattice functions (optional)
par.settings <- list(
  fontsize=list(text=10),
  superpose.symbol = list(col = "blue", pch = 19, cex = 0.8),
  axis.text = list(col = "black", cex = 0.8),  # Change the axis text color to red
  axis.line = list(col = "black"),  # Change the axis line color to red
  layout.heights = list(top.padding = 1, bottom.padding = 1),
  strip.text = list(col = "red")  # Change the color of all text to red
)

trellis.par.set(par.settings)

# Print the wind rose plot
print(wind_rose_plot)


