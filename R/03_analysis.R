
# Create the data frame that contains only the needed parameters.

df_cor <- data.frame(LE = df$LE, H = df$H, U = df$wind_speed, deltae = df$delta_e, deltat = df$delta_T, udeltae = df$U_deltaE, udeltat = df$U_deltaT,ustar =df$`u*` )



# Run the Pearson correlation tests

cor(df_cor, use = "complete.obs")
cor.test(df_cor$LE,df_cor$U)
cor.test(df_cor$LE,df_cor$deltae)
cor.test(df_cor$LE,df_cor$udeltae)
cor.test(df_cor$LE,df_cor$udeltat)
cor.test(df_cor$LE,df_cor$H)
cor.test(df_cor$LE,df_cor$ustar)


