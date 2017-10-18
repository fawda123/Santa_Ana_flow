#Rafi is playing around with perennial designations

load("data/flos.RData")
head(flos)

load("data/sum1.RData")

head(sum1)



flos$mo2<-factor(flos$mo, levels=c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"))
library(reshape2)
flos.d<-dcast(flos, SMC_Name+COMID+Shape_Leng+clim~mo, value.var = "flo")
head(flos.d)

flos.d<-flos.d[,c("COMID","SMC_Name","Shape_Leng","clim",c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"))]
flos.d$AnnualMin<-apply(flos.d[,5:16],1, min)
tail(flos.d)

min.f<-5
flos.d$Status<-ifelse(flos.d$AnnualMin<=min.f, "Nonperennial","Perennial")



library(plyr)
stat.sum<-ddply(flos.d, .(SMC_Name, clim, Status), summarize, len=sum(Shape_Leng))

library(ggplot2)
ggplot(data = stat.sum, aes(x = clim, y = len)) + 
  geom_bar(aes(fill = Status), stat = "identity", position = "stack") + 
  scale_fill_brewer(palette = "Paired", name = "Discharge") + 
  facet_wrap(~SMC_Name) + xlab("") + ylab("Total stream-length (km)") + 
  ggtitle("Reference flows") + 
  theme(legend.position = "bottom")

###
flos.d2<-flos.d



# flos.d2m<-melt(flos.d2[,c("SMC_Name","COMID","Shape_Leng","clim","AnnualMin")],
#                id.vars=c("SMC_Name","COMID","Shape_Leng","clim"))

flos.d2m<-flos.d2[,c("SMC_Name","COMID","Shape_Leng","clim","AnnualMin")]

com.df<-expand.grid(COMID=flos.d2m$COMID, MinFlow=c(0:10))
head(com.df)
flos.d2m<-join(flos.d2m, com.df)
head(flos.d2m)

flos.d2m$Status<-  ifelse(round(flos.d2m$AnnualMin,1)<=flos.d2m$MinFlow,"Nonperennial","Perennial")
head(flos.d2m)

stat.sum2<-ddply(flos.d2m, .(SMC_Name, clim,MinFlow,Status), summarize, len=sum(Shape_Leng))
head(stat.sum2)

levels(stat.sum2$SMC_Name)<-c("LSA","MSA","SGB","SJC","USA")
stat.sum2$SMC_Name2<-factor(stat.sum2$SMC_Name, 
                            levels=c("San Gabriel","Lower Santa Ana","Middle Santa Ana", "Upper Santa Ana", "San Jacinto"))
levels(stat.sum2$SMC_Name2)

ggplot(data = stat.sum2, aes(x = clim, y = len)) + 
  geom_bar(aes(fill = Status), stat = "identity", position = "stack") + 
  scale_fill_brewer(palette = "Paired", name = "Flow duration") + 
  facet_grid(SMC_Name~MinFlow) + xlab("") + ylab("Total stream-length (km)") + 
  ggtitle("Reference flows") + 
  theme(legend.position = "bottom")

ggplot(data = stat.sum2[which(stat.sum2$Status=="Perennial"),], aes(x = MinFlow, y = len)) + 
  geom_point(aes(color=clim)) +
  geom_path(aes(color=clim))+
  scale_color_brewer(palette = "Set1", name="Climate") + 
  facet_wrap(~SMC_Name, scales="free_y", ncol=1) + 
  xlab("Min annual flow for perennial designation") + ylab("Total stream-length (km)") + 
  ggtitle("Reference flows") + 
  theme(legend.position = "bottom")


ggplot(data = stat.sum2, aes(x = MinFlow, y = len)) + 
  geom_bar(aes(fill = Status), stat = "identity", position = "stack") + 
  scale_fill_brewer(palette = "Paired", name = "Flow duration") + 
  facet_grid(SMC_Name~clim, scales="free_y") + xlab("") + ylab("Total stream-length (km)") + 
  ggtitle("Reference flows") + 
  theme(legend.position = "bottom")


ggplot(data = stat.sum2[which(stat.sum2$MinFlow==8),], aes(x = clim, y = len)) + 
  geom_bar(aes(fill = Status), stat = "identity", position = "stack") + 
  scale_fill_brewer(palette = "Paired", name = "Flow duration") + 
  facet_wrap(~SMC_Name,ncol=1, scales="free_y") + xlab("") + ylab("Total stream-length (km)") + 
  ggtitle("Reference flows") + 
  theme(legend.position = "bottom")
