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

######
# summary data 

# load data
data(shed)
data(flos)

# catchment stream length
lns <- flos %>% 
  select(SMC_Name, COMID, Shape_Leng) %>% 
  unique %>% 
  group_by(SMC_Name) %>% 
  summarise(totlen = sum(Shape_Leng))

# splits
flcts <- c(1, 10, 100)
dicts <- c(0.1, 0.9)
flos <- flos %>% 
  mutate(
    flocat = cut(flo, breaks = c(-Inf, flcts, Inf), labels = c('< 1 cfs', '1 - 10 cfs', '10 - 100 cfs', '> 100 cfs')),
    dicat = cut(Di, breaks = c(-Inf, dicts, Inf), labels = c('Inflated', 'Stable', 'Diminished'))
  ) %>% 
  left_join(lns, by = 'SMC_Name') %>% 
  mutate(
    SMC_Name = factor(SMC_Name, 
                      levels = c('San Gabriel', 'Lower Santa Ana', 'Middle Santa Ana', 'Upper Santa Ana', 'San Jacinto'), 
                      labels = c('SGB', 'LSA', 'MSA', 'USA', 'SJC')
    ), 
    mo = factor(mo, levels = c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'))
  )


# summarize by flocat
sum1 <- flos %>% 
  group_by(SMC_Name, mo, clim, flocat) %>% 
  summarise(
    len = sum(Shape_Leng), 
    lenper = len / unique(totlen)
  )

# summarize by dicat
sum2 <- flos %>% 
  group_by(SMC_Name, mo, clim, dicat) %>% 
  summarise(
    len = sum(Shape_Leng),
    lenper = len / unique(totlen)
  )

save(sum1, file = 'data/sum1.RData', compress = 'xz') 
save(sum2, file = 'data/sum2.RData', compress = 'xz') 

##
# create comids, stream reaches in fortified format 

# study area sheds, SMC from abv
shed <- readOGR('raw/StudyArea_Sheds.shp') %>% 
  spTransform(CRS(prstr)) %>%  
  subset(SMC_Name %in% shds)

# streams lat/lon by comid
refs <- readOGR('raw/Ref_StudyArea_100317.shp')%>% 
  spTransform(CRS(prstr)) %>% 
  raster::intersect(shed)
key <- refs@data %>% 
  rownames_to_column('id') %>% 
  dplyr::select(id, COMID)  
comd <- tidy(refs) %>% 
  left_join(key, by = 'id')

save(comd, file = 'data/comd.RData', compress = 'xz')

##
# comid (polyline for streams comids) to sf object

# study area sheds, SMC from abv
shed <- readOGR('raw/StudyArea_Sheds.shp') %>% 
  spTransform(CRS(prstr)) %>%  
  subset(SMC_Name %in% shds)

# streams lat/lon by comid
refs <- readOGR('raw/Ref_StudyArea_100317.shp')%>% 
  spTransform(CRS(prstr)) %>% 
  raster::intersect(shed)
comd_sf <- st_as_sf(refs)

save(comd_sf, file = 'data/comd_sf.RData', compress = 'xz')
save(comd_sf, file = 'Santa_Ana_flow/data/comd_sf.RData', compress = 'xz')
