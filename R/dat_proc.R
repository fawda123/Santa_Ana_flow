library(rgdal)
library(sp)
library(tidyverse)

# SMC watersheds in study area
shds <- c('San Gabriel', 'Upper Santa Ana', 'Middle Santa Ana', 'Lower Santa Ana', 'San Jacinto')
prstr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

# study area sheds, SMC from abv
shed <- readOGR('raw/StudyArea_Sheds.shp')%>% 
  spTransform(CRS(prstr)) %>%  
  subset(SMC_Name %in% shds)

# reference
refs <- readOGR('raw/Ref_StudyArea_100317.shp')%>% 
  spTransform(CRS(prstr)) %>% 
  raster::intersect(shed) %>% 
  data.frame %>% 
  dplyr::select(COMID, SMC_Name, Shape_Leng, matches('_M$|_ME$')) %>% 
  gather('var', 'flo', -COMID, -SMC_Name, -Shape_Leng) %>% 
  mutate(var = gsub('_M$|_Me$', '', var)) %>% 
  separate(var, c('mo', 'clim'), sep = '_')

# anthropogenic
ants <- readOGR('raw/Anthro_StudyArea_100317.shp') %>% 
  spTransform(CRS(prstr)) %>% 
  data.frame %>% 
  dplyr::select(COMID, matches('Wet|Dry|Norm')) %>%
  gather('var', 'val', -COMID) %>% 
  mutate(
    var = gsub('_I$', '_In', var),
    var = gsub('_D$', '_Di', var)
    ) %>% 
  separate(var, c('mo', 'clim', 'prob'), sep = '_') %>% 
  spread('prob', 'val') %>% 
  mutate(
    chk = Di + In
  ) %>% 
  filter(chk >  0) %>% 
  dplyr::select(-In, -chk)

# combin the two
flos <- inner_join(refs, ants, by = c('COMID', 'mo', 'clim'))

save(flos, file = 'data/flos.RData', compress = 'xz')
save(shed, file = 'data/shed.RData', compress = 'xz')
