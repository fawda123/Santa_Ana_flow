library(rpart)
library(rpart.plot)
mydf <- read.csv("ignore/mydf.csv", stringsAsFactors = F)
model <- rpart(mean_mon~DRAIN_SQKM+TOPWET+ppt_watershed+ppt_ws_3mo+HGB+ASPECT_EASTNESS+PERDUN+tmean_3mo+RUNAVE7100+STREAMS_KM+RFACT+HGD+CaO_pct+RRMEAN_30M+ELEV_MEAN_,
               data=mydf)
par(family='serif',mar=c(0,0,0,0))

png('docs/figs/rpartex.png', width = 6, height = 4, units = 'in', res = 600, family = 'serif')
par(mar = c(0, 0, 0, 0))
rpart.plot(model,fallen.leaves=F, extra = 0, shadow.col = 'grey')
dev.off()
