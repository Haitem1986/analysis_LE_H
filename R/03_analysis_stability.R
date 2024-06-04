#### Boxplot for Stability Analysis ####

# Plotting Filtered CH
df_coeff <- data.frame(Z.L = df$Z.L, CeP = df$Ce, CHP = df$CH, LE = df$LE,
                       H = df$H, U = df$wind_speed, dE = df$delta_e,
                       dT = df$delta_T, U_dE = df$U_deltaE, U_dT = df$U_deltaT)

df_coeff$CHP[is.infinite(df_coeff$CHP)] <- NA

stability_no <- sapply(df_coeff$Z.L,unst_stab_category)

df_coeff <- cbind(df_coeff,stability_no)


df_coeff$CHP[which((df_coeff$dT < 0 & df_coeff$H > 0) | 
                     (df_coeff$dT > 0 & df_coeff$H < 0))] <- NA

df_coeff$CeP[which((df_coeff$dE < 0 & df_coeff$LE > 0) | 
                     (df_coeff$dE > 0 & df_coeff$LE < 0))] <- NA


#### CE ####

library(ggplot2)

# Define custom labels for the x-axis
names_boxplot <- c('−10≤ζ<−1', '−1≤ζ<−0.5', '−0.5≤ζ<−0.1', '−0.1≤ζ<−0.05',
                   '−0.05≤ζ<0', '0≤ζ<0.05', '0.05≤ζ<0.1', '0.1≤ζ<0.5', '0.5≤ζ<1',
                   '1≤ζ<10')

# Updated plot for CeP with darker grey lines and dots
plot_CE <- ggplot(na.omit(df_coeff), aes(x=as.factor(stability_no), y=CeP)) +
  geom_jitter(
    aes(color=factor(as.numeric(factor(stability_no)) > 5)),  # Color based on category position
    alpha=0.5,     # Transparency of the points
    width=0.2      # How much to jitter the points horizontally
  ) +
  scale_color_manual(values=c("red", "blue")) +  # Define manual colors: red for left, blue for right
  stat_summary(
    fun=median, 
    geom="point",  # Use point geom to display the median value
    color="darkgrey",  # Darker shade of grey for the median point
    size=3         # Size of the median point
  ) +
  stat_summary(
    fun=median,
    geom="line",   # Add a line geom to connect the median points
    aes(group=1),  # Ensure all median points are connected by a single line
    color="darkgrey",  # Darker shade of grey for the line
    size=1         # Size of the line
  ) +
  geom_vline(
    xintercept=5.5,        # Position between categories 5 and 6
    linetype="dashed",     # Dashed line type
    color="black",         # Black color for the line
    size=0.5               # Line thickness for the vertical line
  ) +
  coord_cartesian(ylim = c(-0.0001, 0.002)) +  # Adjusted y-axis limits
  labs(
    x = "",  # Remove the x-axis label
    y = expression(C[E])  # Set y-axis label to C subscript E
  ) +
  scale_x_discrete(labels=names_boxplot) +  # Set custom labels for the x-axis
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "plain"),  # Set x-axis title to non-bold
    axis.title.y = element_text(face = "plain"),  # Set y-axis title to non-bold
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "black", fill=NA, size=0.5),  # Line thickness for the box
    legend.position = "none"  # Remove the legend
  )

# Display the plot
print(plot_CE)




#### CH ####

library(ggplot2)

# Define custom labels for the x-axis
names_boxplot <- c('−10≤ζ<−1', '−1≤ζ<−0.5', '−0.5≤ζ<−0.1', '−0.1≤ζ<−0.05',
                   '−0.05≤ζ<0', '0≤ζ<0.05', '0.05≤ζ<0.1', '0.1≤ζ<0.5', '0.5≤ζ<1',
                   '1≤ζ<10')

# Updated plot for CHP with darker grey lines and dots
plot_CH <- ggplot(na.omit(df_coeff), aes(x=as.factor(stability_no), y=CHP)) +
  geom_jitter(
    aes(color=factor(as.numeric(factor(stability_no)) > 5)),  # Color based on category position
    alpha=0.5,     # Transparency of the points
    width=0.2      # How much to jitter the points horizontally
  ) +
  scale_color_manual(values=c("red", "blue")) +  # Define manual colors: red for left, blue for right
  stat_summary(
    fun=median, 
    geom="point",  # Use point geom to display the median value
    color="darkgrey",  # Darker shade of grey for the median point
    size=3         # Size of the median point
  ) +
  stat_summary(
    fun=median,
    geom="line",   # Add a line geom to connect the median points
    aes(group=1),  # Ensure all median points are connected by a single line
    color="darkgrey",  # Darker shade of grey for the line
    size=1         # Size of the line
  ) +
  geom_vline(
    xintercept=5.5,        # Position between categories 5 and 6
    linetype="dashed",     # Dashed line type
    color="black",         # Black color for the line
    size=0.5               # Line thickness for the vertical line
  ) +
  coord_cartesian(ylim = c(-0.005, 0.025)) +  # Adjusted y-axis limits
  labs(
    x = "",  # Remove the x-axis label
    y = expression(C[H])  # Set y-axis label to C subscript H
  ) +
  scale_x_discrete(labels=names_boxplot) +  # Set custom labels for the x-axis
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "plain"),  # Set x-axis title to non-bold
    axis.title.y = element_text(face = "plain"),  # Set y-axis title to non-bold
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "black", fill=NA, size=0.5),  # Line thickness for the box
    legend.position = "none"  # Remove the legend
  )

# Display the plot
print(plot_CH)

#### U ####
library(ggplot2)

# Define custom labels for the x-axis
names_boxplot <- c('−10≤ζ<−1', '−1≤ζ<−0.5', '−0.5≤ζ<−0.1', '−0.1≤ζ<−0.05',
                   '−0.05≤ζ<0', '0≤ζ<0.05', '0.05≤ζ<0.1', '0.1≤ζ<0.5', '0.5≤ζ<1',
                   '1≤ζ<10')

# Updated plot for variable U with y-axis limit from 0 to 3
plot_U <- ggplot(na.omit(df_coeff), aes(x=as.factor(stability_no), y=U)) +
  geom_jitter(
    aes(color=factor(as.numeric(factor(stability_no)) > 5)),  # Color based on category position
    alpha=0.5,     # Transparency of the points
    width=0.2      # How much to jitter the points horizontally
  ) +
  scale_color_manual(values=c("red", "blue")) +  # Define manual colors: red for left, blue for right
  stat_summary(
    fun=median, 
    geom="point",  # Use point geom to display the median value
    color="darkgrey",  # Darker shade of grey for the median point
    size=3         # Size of the median point
  ) +
  stat_summary(
    fun=median,
    geom="line",   # Add a line geom to connect the median points
    aes(group=1),  # Ensure all median points are connected by a single line
    color="darkgrey",  # Darker shade of grey for the line
    size=1         # Size of the line
  ) +
  geom_vline(
    xintercept=5.5,        # Position between categories 5 and 6
    linetype="dashed",     # Dashed line type
    color="black",         # Black color for the line
    size=0.5               # Line thickness for the vertical line
  ) +
  coord_cartesian(ylim = c(0, 3)) +  # Adjusted y-axis limits
  labs(
    x = "",  # Remove the x-axis label
    y = "U"  # Set y-axis label to U
  ) +
  scale_x_discrete(labels=names_boxplot) +  # Set custom labels for the x-axis
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "plain"),  # Set x-axis title to non-bold
    axis.title.y = element_text(face = "plain"),  # Set y-axis title to non-bold
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "black", fill=NA, size=0.5),  # Line thickness for the box
    legend.position = "none"  # Remove the legend
  )

# Display the plot
print(plot_U)


#### dE ####

library(ggplot2)

# Define custom labels for the x-axis
names_boxplot <- c('−10≤ζ<−1', '−1≤ζ<−0.5', '−0.5≤ζ<−0.1', '−0.1≤ζ<−0.05',
                   '−0.05≤ζ<0', '0≤ζ<0.05', '0.05≤ζ<0.1', '0.1≤ζ<0.5', '0.5≤ζ<1',
                   '1≤ζ<10')

# Updated plot for variable dE with Greek letter delta and small e as y-axis label
plot_dE <- ggplot(na.omit(df_coeff), aes(x=as.factor(stability_no), y=dE)) +
  geom_jitter(
    aes(color=factor(as.numeric(factor(stability_no)) > 5)),  # Color based on category position
    alpha=0.5,     # Transparency of the points
    width=0.2      # How much to jitter the points horizontally
  ) +
  scale_color_manual(values=c("red", "blue")) +  # Define manual colors: red for left, blue for right
  stat_summary(
    fun=median, 
    geom="point",  # Use point geom to display the median value
    color="darkgrey",  # Darker shade of grey for the median point
    size=3         # Size of the median point
  ) +
  stat_summary(
    fun=median,
    geom="line",   # Add a line geom to connect the median points
    aes(group=1),  # Ensure all median points are connected by a single line
    color="darkgrey",  # Darker shade of grey for the line
    size=1         # Size of the line
  ) +
  geom_vline(
    xintercept=5.5,        # Position between categories 5 and 6
    linetype="dashed",     # Dashed line type
    color="black",         # Black color for the line
    size=0.5               # Line thickness for the vertical line
  ) +
  coord_cartesian(ylim = c(-0.3, 1.5)) +  # Adjusted y-axis limits
  labs(
    x = "",  # Remove the x-axis label
    y = expression(Delta * e)  # Set y-axis label to Δe
  ) +
  scale_x_discrete(labels=names_boxplot) +  # Set custom labels for the x-axis
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "plain"),  # Set x-axis title to non-bold
    axis.title.y = element_text(face = "plain"),  # Set y-axis title to non-bold
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "black", fill=NA, size=0.5),  # Line thickness for the box
    legend.position = "none"  # Remove the legend
  )

# Display the plot
print(plot_dE)


#### dT ####

library(ggplot2)

# Define custom labels for the x-axis
names_boxplot <- c('−10≤ζ<−1', '−1≤ζ<−0.5', '−0.5≤ζ<−0.1', '−0.1≤ζ<−0.05',
                   '−0.05≤ζ<0', '0≤ζ<0.05', '0.05≤ζ<0.1', '0.1≤ζ<0.5', '0.5≤ζ<1',
                   '1≤ζ<10')

# Updated plot for variable dT with y-axis limit set from -5 to 5
plot_dT <- ggplot(na.omit(df_coeff), aes(x=as.factor(stability_no), y=dT)) +
  geom_jitter(
    aes(color=factor(as.numeric(factor(stability_no)) > 5)),  # Color based on category position
    alpha=0.5,     # Transparency of the points
    width=0.2      # How much to jitter the points horizontally
  ) +
  scale_color_manual(values=c("red", "blue")) +  # Define manual colors: red for left, blue for right
  stat_summary(
    fun=median, 
    geom="point",  # Use point geom to display the median value
    color="darkgrey",  # Darker shade of grey for the median point
    size=3         # Size of the median point
  ) +
  stat_summary(
    fun=median,
    geom="line",   # Add a line geom to connect the median points
    aes(group=1),  # Ensure all median points are connected by a single line
    color="darkgrey",  # Darker shade of grey for the line
    size=1         # Size of the line
  ) +
  geom_vline(
    xintercept=5.5,        # Position between categories 5 and 6
    linetype="dashed",     # Dashed line type
    color="black",         # Black color for the line
    size=0.5               # Line thickness for the vertical line
  ) +
  coord_cartesian(ylim = c(-2.5, 3)) +  # Adjusted y-axis limits
  labs(
    x = "",  # Remove the x-axis label
    y = expression(Delta * T)  # Set y-axis label to Δt
  ) +
  scale_x_discrete(labels=names_boxplot) +  # Set custom labels for the x-axis
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "plain"),  # Set x-axis title to non-bold
    axis.title.y = element_text(face = "plain"),  # Set y-axis title to non-bold
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "black", fill=NA, size=0.5),  # Line thickness for the box
    legend.position = "none"  # Remove the legend
  )

# Display the plot
print(plot_dT)

#### LE ####

library(ggplot2)

# Define custom labels for the x-axis
names_boxplot <- c('−10≤ζ<−1', '−1≤ζ<−0.5', '−0.5≤ζ<−0.1', '−0.1≤ζ<−0.05',
                   '−0.05≤ζ<0', '0≤ζ<0.05', '0.05≤ζ<0.1', '0.1≤ζ<0.5', '0.5≤ζ<1',
                   '1≤ζ<10')

# Updated plot for variable LE with y-axis limit set from -100 to 100
plot_LE <- ggplot(na.omit(df_coeff), aes(x=as.factor(stability_no), y=LE)) +
  geom_jitter(
    aes(color=factor(as.numeric(factor(stability_no)) > 5)),  # Color based on category position
    alpha=0.5,     # Transparency of the points
    width=0.2      # How much to jitter the points horizontally
  ) +
  scale_color_manual(values=c("red", "blue")) +  # Define manual colors: red for left, blue for right
  stat_summary(
    fun=median, 
    geom="point",  # Use point geom to display the median value
    color="darkgrey",  # Darker shade of grey for the median point
    size=3         # Size of the median point
  ) +
  stat_summary(
    fun=median,
    geom="line",   # Add a line geom to connect the median points
    aes(group=1),  # Ensure all median points are connected by a single line
    color="darkgrey",  # Darker shade of grey for the line
    size=1         # Size of the line
  ) +
  geom_vline(
    xintercept=5.5,        # Position between categories 5 and 6
    linetype="dashed",     # Dashed line type
    color="black",         # Black color for the line
    size=0.5               # Line thickness for the vertical line
  ) +
  coord_cartesian(ylim = c(-5, 40)) +  # Adjusted y-axis limits
  labs(
    x = "",  # Remove the x-axis label
    y = "LE"  # Set y-axis label to ΔE (change as needed)
  ) +
  scale_x_discrete(labels=names_boxplot) +  # Set custom labels for the x-axis
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "plain"),  # Set x-axis title to non-bold
    axis.title.y = element_text(face = "plain"),  # Set y-axis title to non-bold
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "black", fill=NA, size=0.5),  # Line thickness for the box
    legend.position = "none"  # Remove the legend
  )

# Display the plot
print(plot_LE)


#### H ####

library(ggplot2)

# Define custom labels for the x-axis
names_boxplot <- c('−10≤ζ<−1', '−1≤ζ<−0.5', '−0.5≤ζ<−0.1', '−0.1≤ζ<−0.05',
                   '−0.05≤ζ<0', '0≤ζ<0.05', '0.05≤ζ<0.1', '0.1≤ζ<0.5', '0.5≤ζ<1',
                   '1≤ζ<10')

# Updated plot for variable H with y-axis limit set from -100 to 100
plot_H <- ggplot(na.omit(df_coeff), aes(x=as.factor(stability_no), y=H)) +
  geom_jitter(
    aes(color=factor(as.numeric(factor(stability_no)) > 5)),  # Color based on category position
    alpha=0.5,     # Transparency of the points
    width=0.2      # How much to jitter the points horizontally
  ) +
  scale_color_manual(values=c("red", "blue")) +  # Define manual colors: red for left, blue for right
  stat_summary(
    fun=median, 
    geom="point",  # Use point geom to display the median value
    color="darkgrey",  # Darker shade of grey for the median point
    size=3         # Size of the median point
  ) +
  stat_summary(
    fun=median,
    geom="line",   # Add a line geom to connect the median points
    aes(group=1),  # Ensure all median points are connected by a single line
    color="darkgrey",  # Darker shade of grey for the line
    size=1         # Size of the line
  ) +
  geom_vline(
    xintercept=5.5,        # Position between categories 5 and 6
    linetype="dashed",     # Dashed line type
    color="black",         # Black color for the line
    size=0.5               # Line thickness for the vertical line
  ) +
  coord_cartesian(ylim = c(-5, 10)) +  # Adjusted y-axis limits
  labs(
    x = "",  # Remove the x-axis label
    y = "H"  # Set y-axis label to H
  ) +
  scale_x_discrete(labels=names_boxplot) +  # Set custom labels for the x-axis
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "plain"),  # Set x-axis title to non-bold
    axis.title.y = element_text(face = "plain"),  # Set y-axis title to non-bold
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "black", fill=NA, size=0.5),  # Line thickness for the box
    legend.position = "none"  # Remove the legend
  )

# Display the plot
print(plot_H)

#### UdE ####

library(ggplot2)

# Define custom labels for the x-axis
names_boxplot <- c('−10≤ζ<−1', '−1≤ζ<−0.5', '−0.5≤ζ<−0.1', '−0.1≤ζ<−0.05',
                   '−0.05≤ζ<0', '0≤ζ<0.05', '0.05≤ζ<0.1', '0.1≤ζ<0.5', '0.5≤ζ<1',
                   '1≤ζ<10')

# Updated plot for variable dE with Greek letter delta and small e as y-axis label
plot_UdE <- ggplot(na.omit(df_coeff), aes(x=as.factor(stability_no), y=U_dE)) +
  geom_jitter(
    aes(color=factor(as.numeric(factor(stability_no)) > 5)),  # Color based on category position
    alpha=0.5,     # Transparency of the points
    width=0.2      # How much to jitter the points horizontally
  ) +
  scale_color_manual(values=c("red", "blue")) +  # Define manual colors: red for left, blue for right
  stat_summary(
    fun=median, 
    geom="point",  # Use point geom to display the median value
    color="darkgrey",  # Darker shade of grey for the median point
    size=3         # Size of the median point
  ) +
  stat_summary(
    fun=median,
    geom="line",   # Add a line geom to connect the median points
    aes(group=1),  # Ensure all median points are connected by a single line
    color="darkgrey",  # Darker shade of grey for the line
    size=1         # Size of the line
  ) +
  geom_vline(
    xintercept=5.5,        # Position between categories 5 and 6
    linetype="dashed",     # Dashed line type
    color="black",         # Black color for the line
    size=0.5               # Line thickness for the vertical line
  ) +
  coord_cartesian(ylim = c(-0.3, 2)) +  # Adjusted y-axis limits
  labs(
    x = "",  # Remove the x-axis label
    y = expression(U * Delta * e)  # Set y-axis label to Δe
  ) +
  scale_x_discrete(labels=names_boxplot) +  # Set custom labels for the x-axis
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "plain"),  # Set x-axis title to non-bold
    axis.title.y = element_text(face = "plain"),  # Set y-axis title to non-bold
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "black", fill=NA, size=0.5),  # Line thickness for the box
    legend.position = "none"  # Remove the legend
  )

# Display the plot
print(plot_UdE)


#### UdT ####

library(ggplot2)

# Define custom labels for the x-axis
names_boxplot <- c('−10≤ζ<−1', '−1≤ζ<−0.5', '−0.5≤ζ<−0.1', '−0.1≤ζ<−0.05',
                   '−0.05≤ζ<0', '0≤ζ<0.05', '0.05≤ζ<0.1', '0.1≤ζ<0.5', '0.5≤ζ<1',
                   '1≤ζ<10')

# Updated plot for variable dT with y-axis limit set from -5 to 5
plot_UdT <- ggplot(na.omit(df_coeff), aes(x=as.factor(stability_no), y=U_dT)) +
  geom_jitter(
    aes(color=factor(as.numeric(factor(stability_no)) > 5)),  # Color based on category position
    alpha=0.5,     # Transparency of the points
    width=0.2      # How much to jitter the points horizontally
  ) +
  scale_color_manual(values=c("red", "blue")) +  # Define manual colors: red for left, blue for right
  stat_summary(
    fun=median, 
    geom="point",  # Use point geom to display the median value
    color="darkgrey",  # Darker shade of grey for the median point
    size=3         # Size of the median point
  ) +
  stat_summary(
    fun=median,
    geom="line",   # Add a line geom to connect the median points
    aes(group=1),  # Ensure all median points are connected by a single line
    color="darkgrey",  # Darker shade of grey for the line
    size=1         # Size of the line
  ) +
  geom_vline(
    xintercept=5.5,        # Position between categories 5 and 6
    linetype="dashed",     # Dashed line type
    color="black",         # Black color for the line
    size=0.5               # Line thickness for the vertical line
  ) +
  coord_cartesian(ylim = c(-3, 3)) +  # Adjusted y-axis limits
  labs(
    x = "",  # Remove the x-axis label
    y = expression(U * Delta * T)  # Set y-axis label to Δt
  ) +
  scale_x_discrete(labels=names_boxplot) +  # Set custom labels for the x-axis
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(face = "plain"),  # Set x-axis title to non-bold
    axis.title.y = element_text(face = "plain"),  # Set y-axis title to non-bold
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for better visibility
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "black", fill=NA, size=0.5),  # Line thickness for the box
    legend.position = "none"  # Remove the legend
  )

# Display the plot
print(plot_UdT)

# Ensure you have your plot object
# plot_H <- ggplot(...) + ...

# Save the plot as a JPEG file at 400 DPI
ggsave("R/fig/plot_H.jpg", plot = plot_H, dpi = 400, width = 8, height = 6)

# Save the plot as a JPEG file at 400 DPI
ggsave("R/fig/plot_LE.jpg", plot = plot_LE, dpi = 400, width = 8, height = 6)

# Save the plot as a JPEG file at 400 DPI
ggsave("R/fig/plot_CE.jpg", plot = plot_CE, dpi = 400, width = 8, height = 6)

# Save the plot as a JPEG file at 400 DPI
ggsave("R/fig/plot_CH.jpg", plot = plot_CH, dpi = 400, width = 8, height = 6)

# Save the plot as a JPEG file at 400 DPI
ggsave("R/fig/plot_U.jpg", plot = plot_U, dpi = 400, width = 8, height = 6)

# Save the plot as a JPEG file at 400 DPI
ggsave("R/fig/plot_dE.jpg", plot = plot_dE, dpi = 400, width = 8, height = 6)

# Save the plot as a JPEG file at 400 DPI
ggsave("R/fig/plot_dT.jpg", plot = plot_dT, dpi = 400, width = 8, height = 6)

# Save the plot as a JPEG file at 400 DPI
ggsave("R/fig/plot_UdE.jpg", plot = plot_UdE, dpi = 400, width = 8, height = 6)

# Save the plot as a JPEG file at 400 DPI
ggsave("R/fig/plot_UdT.jpg", plot = plot_UdT, dpi = 400, width = 8, height = 6)
