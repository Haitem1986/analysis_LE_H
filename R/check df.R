plot(df$ea)
plot(df$es1)
plot(df$delta_e)
plot(df$wind_speed)
plot(df$U_deltaE)
plot(df$U_deltaT)
plot(df$LE)
plot(df$H)
plot(df$delta_T)


plot(df$LE[4336:4400])
df$LE[4336]
df[4336,]

plot(df$LE)
plot(df$date[0:50],df$LE[0:50])

plot(df$LE[0:200], ylim=c(-50,50))

badLE <- c(4336)



plot(df$LE)
plot(df$LE[9560:9600])
plot(df$LE[9560:9600], ylim=c(80,90))
df[6998,]




plot(df$LE[9000:1000])
plot(df$LE[9000:1000], ylim=c(80,90))
plot(df$LE[9000:10000], ylim=c(80,100))



plot(df$LE)
plot(df$LE[9415:9416])
plot(df$LE[9415:9416], ylim=c(50,110))
df[9416,]

plot(df$LE)
plot(df$LE[3900:4200])
plot(df$LE[3949:3949], ylim=c(70,90))
df[3949,]

plot(df$LE)
plot(df$LE[3381:3381])
plot(df$LE[3381:3381], ylim=c(0,50))
df[3381,]

plot(df$LE)
plot(df$LE[3425:3426])
plot(df$LE[3425:3425], ylim=c(30,50))
df[3425,]

plot(df$LE)
plot(df$LE[4961:4962])
plot(df$LE[3961:3961], ylim=c(0,20))
df[3961,]

plot(df$LE)
plot(df$LE[5343:5344])
plot(df$LE[5343:5343], ylim=c(0,20))
df[5343,]

plot(df$LE)
plot(df$LE[2062:2063])
plot(df$LE[2062:2062], ylim=c(-80,-100))
df[2062,]

plot(df$LE)
plot(df$LE[5730:5730])
plot(df$LE[5731:5731], ylim=c(-50,-100))
df[5730,]

plot(df$LE)
plot(df$LE[8920:8940])
plot(df$LE[5731:5731], ylim=c(-50,-100))
df[5730,]

ggplot(df, aes(factor(stability_no),LE)) + 
  geom_boxplot(outlier.size=0,fill="white") + coord_cartesian(ylim=c(-10,50))


ggplot(df, aes(factor(stability_no),H)) + 
  geom_boxplot(outlier.size=0,fill="white") + coord_cartesian(ylim=c(-5,5))

summary(as.factor(df$stability_no))






count(is.na(df$LE[which(df$stability_no == 1)]))
count(is.na(df$LE[which(df$stability_no == 2)]))
count(is.na(df$LE[which(df$stability_no == 3)]))
count(is.na(df$LE[which(df$stability_no == 4)]))
count(is.na(df$LE[which(df$stability_no == 5)]))
count(is.na(df$LE[which(df$stability_no == 6)]))
count(is.na(df$LE[which(df$stability_no == 7)]))
count(is.na(df$LE[which(df$stability_no == 8)]))
count(is.na(df$LE[which(df$stability_no == 9)]))
count(is.na(df$LE[which(df$stability_no == 10)]))

plot(df$U_deltaE,df$LE)
plot(df$U_deltaT,df$H)


plot(df$delta_e,df$LE)
plot(df$delta_T,df$H)

plot(df$wind_speed,df$LE)
plot(df$wind_speed,df$H)


