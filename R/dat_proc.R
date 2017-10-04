library(rgdal)
library(sp)
library(tidyverse)

# SMC watersheds in study area
shds <- c('San Gabriel', 'Upper Santa Ana', 'Middle Santa Ana', 'Lower Santa Ana', 'San Jacinto')
prstr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

refs <- readOGR('ignore/Ref_StudyArea_100317.shp')%>% 
  spTransform(CRS(prstr))
ants <- readOGR('ignore/Anthro_StudyArea_100317.shp') %>% 
  spTransform(CRS(prstr))
shed <- readOGR('ignore/StudyArea_Sheds.shp')%>% 
  spTransform(CRS(prstr)) %>%  
  subset(SMC_Name %in% shds)

# intersect the shed names with refs/ants

save(refs, file = 'data/refs.RData', compress = 'xz')
save(ants, file = 'data/ants.RData', compress = 'xz')
save(shed, file = 'data/shed.RData', compress = 'xz')
