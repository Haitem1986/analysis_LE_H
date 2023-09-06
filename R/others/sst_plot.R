

plot(df_t$time_stamp,df_t$TW_1_1_1,type='l',col='red',ylim=c(28,42))
#lines(df_t$time_stamp,df_t$TS_1_1_1,col='blue')
lines(df_t$time_stamp,df_t$TS_2_1_1)

plot(df_t$time_stamp,df_t$TS_1_1_1,col='blue',ylim=c(30,31),type='l')
lines(df_t$time_stamp,df_t$TS_2_1_1,col='green')

plot(ts1,df_t$TW_1_1_1,xlim=c(0,35),ylim=c(0,45),pch=19)
points(df_t$TS_2_1_1,df_t$TW_1_1_1,pch=19,col='blue')
lm1 <- lm(df_t$TW_1_1_1~ts1)
lm2<-lm(df_t$TW_1_1_1~df_t$TS_2_1_1)
abline(lm1)
abline(lm2,col='blue')


plot(df_t$TW_1_1_1,ts1,xlim=c(0,35),ylim=c(0,45),pch=19)
points(df_t$TW_1_1_1,df_t$TS_2_1_1,pch=19,col='blue')
lm1<-lm(ts1~df_t$TW_1_1_1)
lm2<-lm(df_t$TS_2_1_1~df_t$TW_1_1_1)
abline(lm1)
abline(lm2,col='blue')

# Formula to calculate water surface temperature from LM of 
# water sensor below water (TS_1_1_1) and IR sensor (TW_1_1_1)
# LM is produced after removing above water measurements for the 
# TS_1_1_1
tw1 <- (0.13*df_t$TW_1_1_1) + 25.39

plot(df_t$time_stamp,tw1,ylim=c(29,32),type='l')
lines(df_t$time_stamp,ts1,col='blue')
lines(df_t$time_stamp,df_t$TS_2_1_1,col='green')
