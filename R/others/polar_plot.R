library(plotrix)
library(colorspace)
library(marmap)


## Manage data
# Radial distance
x_90_polar <- df$x_90.[indexCO2_5]/1000
indexNA <- which(is.na(x_90_polar))
# Direction in degrees
wd_polar <- df$wind_dir[indexCO2_5]
wd_polar[indexNA] <- NA
# CO2 flux
co2_polar <- df$co2_flux[indexCO2_5]
co2_polar[indexNA] <- NA

# Remove NA's
x_90_polar <- na.omit(x_90_polar)
# Remove NA's
wd_polar <- na.omit(wd_polar)
wd_polar_rad <- wd_polar * 2 * pi / 360
# Remove NA's
co2_polar <- na.omit(co2_polar)
# Color code CO2 flux
# ii <- cut(co2_polar, breaks = seq(min(co2_polar), max(co2_polar),
#                                   len = 6),include.lowest = TRUE)
ii <- cut(co2_polar, breaks = seq(min(co2_polar), max(co2_polar),
                                  by = 1))
# Use bin indices, ii, to select color from vector of n-1 equally 
# spaced colors
colors <- colorRampPalette(c("lightgreen", "darkorange"),
                           alpha=0.2)(7)[ii]

# Change polar to long lat coordinates
x <- ((x_90_polar*1000/110815) * sin(wd_polar_rad)) + 100.2025
y <- ((x_90_polar*1000/110584) * cos(wd_polar_rad)) + 5.49


# Color palette
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
# Plot map
jpeg(filename = "figs/co2_flux_footprint.jpg",height=8,width=8,
     bg = "white",units='cm', res = 360)
par(family='Times', mar = c(3.1, 3.1, 0.4, 0.9))
plot(bath_ppinang, image = TRUE, land = TRUE, lwd = 0.1,
     xlim = c(100.15,100.35), ylim = c(5.42,5.62),
     bpal = list(c(0, max(bath_ppinang), "grey"),
                 c(min(bath_ppinang),0,blues)),
     cex.lab = 1, cex.axis = 0.8)
mtext(side = 1, 'Longitude', line = 2)
mtext(side = 2, 'Latitude', line = 2)
# Making the coastline more visible
plot(bath_ppinang, deep = 0, shallow = 0, step = 0,
     lwd = 0.4, add = TRUE)

points(x, y, pch = 19, col = colors, cex = 0.4)
# Station location
points(100.2025,5.49, pch = 19, col = 'black', cex = 1)
scaleBathy(bath_ppinang, deg = 0.045, x = "bottomright", inset = 5, 
           cex = 0.8)
legend('topleft', 
       legend = c(paste('[','\u2212','4,','\u2212','3)',sep=''),
                  paste('[','\u2212','3,','\u2212','2)',sep=''),
                  paste('[','\u2212','2,','\u2212','1)',sep=''),
                  paste('[','\u2212','1',', 0)',sep=''),
                  '(0, 1]',
                  '(1, 2]',
                  '(2, 3]'),
       pch = c(19,19,19,19,19,19,19,19,19), 
       col = c(levels(as.factor(colors))),cex = 0.6)
dev.off()




# Radial plot
# radial.plot(x_90_polar,wd_polar_rad,clockwise = TRUE, start=pi/2,
#             grid.left = TRUE,
#             rp.type = 's', point.col = colors, 
#             point.symbols = 19,
#             show.radial.grid = TRUE, grid.unit = 'km',
#             show.grid.labels = FALSE)
