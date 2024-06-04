# Filter by direction and stability
CE_filtered <- df$Ce[which((df$delta_e > 0 & df$LE > 0) | 
                             (df$delta_e < 0 & df$LE < 0) & 
                             (df$stability_no == 5 | df$stability_no == 6))]

mean(CE_filtered, na.rm = T)


# Filter by direction and stability
CH_filtered <- df$CH[which((df$delta_T > 0 & df$H > 0) | 
                             (df$delta_T < 0 & df$H < 0) & 
                             (df$stability_no == 5 | df$stability_no == 6))]
mean(CH_filtered, na.rm = T)



