library(shiny)
library(sf)
library(tidyverse)
library(leaflet) 

load(file = 'data/flos.RData')
load(file = 'data/shed.RData')
load(file = 'data/comd_sf.RData')
load(file = 'data/csci_scrs.RData')
shed_st <- st_as_sf(shed)

# splits
flcts <- c(1, 10, 100)
dicts <- c(0.1, 0.9)
flos <- flos %>% 
  mutate(
    flocat = cut(flo, breaks = c(-Inf, flcts, Inf), labels = c('< 1 cfs', '1 - 10 cfs', '10 - 100 cfs', '> 100 cfs')),
    dicat = cut(Di, breaks = c(-Inf, dicts, Inf), labels = c('Inflated', 'Stable', 'Diminished'))
  ) %>% 
  mutate(
    SMC_Name = factor(SMC_Name, 
                      levels = c('San Gabriel', 'Lower Santa Ana', 'Middle Santa Ana', 'Upper Santa Ana', 'San Jacinto')
    ), 
    mo = factor(mo, levels = c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'))
  )

ptsz <- 7

# color palette for dicat
pal_dicat <- colorFactor(
  palette = RColorBrewer::brewer.pal(9, 'Set1')[c(1, 2, 3)],
  na.color = 'yellow',
  domain = c('Diminished', 'Stable', 'Inflated'))

# color palette for flow
pal_flocat <- colorFactor(
  palette = RColorBrewer::brewer.pal(10, 'Spectral'),
  na.color = 'yellow',
  domain = levels(flos$flocat)
)

# color palette
pal_flo <- colorNumeric(
  palette = RColorBrewer::brewer.pal(10, 'Spectral'),
  na.color = 'yellow',
  domain = log10(1 + flos$flo))

# CSCI palette
pal <- colorNumeric(
  palette = c('#d7191c', '#abd9e9', '#2c7bb6'),
  na.color = 'yellow',
  domain = csci_scrs$CSCI)

# server logic
server <- function(input, output) {
  
  # data to plot, polylines
  dat <- reactive({
    
    # inputs
    moinp <- input$mo
    climinp <- input$clim
    shd <- input$shd
    vrs <- input$vrs
    
    # subset flos by inputs
    flos <- flos %>% 
      filter(mo %in% moinp & clim %in% climinp)
    
    if(shd != 'All')
      flos <- flos %>% 
        filter(SMC_Name %in% shd)

    # join flos with spatial data
    out <- comd_sf %>% 
      select(COMID) %>% 
      filter(COMID %in% flos$COMID) %>% 
      left_join(., flos, by ='COMID')

    out
      
  })
  
  # data to plot, polylines
  shed <- reactive({
    
    # inputs
    shd <- input$shd
    
    # subset flos by inputs
    out <- shed_st 
    if(shd != 'All')
      out <- out %>% 
        filter(SMC_Name %in% shd)  

    out
    
  })
  
  # csci scores
  scrs <- reactive({
    
    # inputs
    shd <- input$shd
    
    out <- csci_scrs
    if(shd != 'All')
      out <- out %>% 
        filter(SMC_Name %in% shd)
    
    out
    
  })
  
  # non-reactive base map
  output$map <- renderLeaflet(

    leaflet(shed()) %>%
      addPolygons(data = shed(), opacity = 1, weight = 1, color = 'grey', 
                  fillOpacity = 0.8, fillColor = 'white') %>% 
      addProviderTiles(providers$CartoDB.Positron)
    
  )
  
  # reactive maps
  observe({

    # other inputs
    ln_sz <- input$ln_sz
    pt_sz <- input$pt_sz
    vrs <- input$vrs
    
    # score expectations
    mp <- leafletProxy("map", data = dat()) %>%
      # clearMarkers() %>%
      clearControls() %>%
      clearShapes() %>% 
      addPolygons(data = shed(), opacity = 1, weight = 1, color = 'grey', 
                  fillOpacity = 0.8, fillColor = 'white')

    if(vrs == 'flocat'){
      
      mp %>%       
        addPolylines(opacity = 1, weight = ln_sz, color = ~pal_flo(log10(1 + flo)), 
                     label = ~paste0('COMID ', COMID, ': log-flow ', round(log10(1 + flo), 1))) %>% 
        addCircleMarkers(data = scrs(), radius = pt_sz, weight = 1, fillOpacity = 0.8, 
                          label = ~paste('CSCI:', as.character(round(CSCI, 2))),
                      fillColor = ~pal(CSCI)) %>% 
        addLegend("topright", pal = pal_flo, values = ~log10(1 + flo),
                  title = "Reference flow (log)",
                  opacity = 1) %>% 
        addLegend("topright", pal = pal, values = scrs()$CSCI, 
                  title = 'CSCI score', 
                  opacity = 1)
      
    } else {
      
      mp %>%
        addPolylines(opacity = 1, weight = ln_sz, color = ~pal_dicat(dicat), 
                                label = ~COMID) %>% 
        addCircleMarkers(data = scrs(), radius = pt_sz, weight = 1, fillOpacity = 0.8, 
                         label = ~paste('CSCI:', as.character(round(CSCI, 2))),
                         fillColor = ~pal(CSCI)) %>% 
        addLegend("topright", pal = pal_dicat, values = ~dicat,
                  title = "Likelihood of change",
                  opacity = 1) %>% 
        addLegend("topright", pal = pal, values = scrs()$CSCI, 
                  title = 'CSCI score', 
                  opacity = 1)
      
    }

    
  })
   
}
