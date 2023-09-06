# Location of SST from MODIS
# lat = 5.479 lon = 100.1875
map_plot <- ggmap(sitemap) +
  geom_point(aes_string(x = "100.1875",y = "5.479"),size = 5,shape = 16,colour = "black")+
  geom_text(aes_string(x="100.2002",y="5.4685"),label="Muka Head Station",colour="white",size=4.5,
            fontface="bold",hjust=0,vjust=-1.00, family = 'Times') +
  xlab("") + ylab("") +
  theme(plot.title = element_text(lineheight=1, face="bold",size = 25, colour = "grey20"),
        axis.line=element_blank(),
        panel.border = element_rect(colour="grey20",fill=NA,size=0.5),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())
map_plot

# SST analysis
sst <- subset(sst_modis, 
              sst_modis$lat > 5.479 & sst_modis$lat < 5.480 & 
              sst_modis$lon > 100.18 & sst_modis$lon < 100.19)
sst <- sst[-1,]
time_stamp <- as.Date(paste(sst$year,sst$month,"1",sep = "-"))
sst <- cbind(time_stamp, sst)
sst <- sst[,-c(2,3,7,8)]
names(sst)[4] <- 'T'
# For t test analysis
# During ENSO
sst_enso <- sst$T[which(sst$time_stamp < as.Date('2016-06-01'))]
sst_xenso <- sst$T[which(sst$time_stamp > as.Date('2016-06-01'))]
t.test(sst_enso,sst_xenso)
