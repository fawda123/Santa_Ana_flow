library(leaflet)
library(shinyjs)
library(shinyBS)

mo <- c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep')
shd <- c('San Gabriel', 'Lower Santa Ana', 'Middle Santa Ana', 'Upper Santa Ana', 'San Jacinto', 'All')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = 'styles.css',
  useShinyjs(),
  
  # Application title
  titlePanel('Flow conditions in the Santa Ana Watershed'),
  
  fluidRow(
    
    column(width = 12,

      column(width = 2, 
             selectInput(inputId = 'shd', 
                         label = h4('Select watershed:'), 
                         choices = shd, selected = 'Lower Santa Ana')
      ),  
           
      column(width = 2, 
             selectInput(inputId = 'vrs', 
                         label = h4('Select variable:'), 
                         choices = list('Reference flow' = 'flocat', 'Likelihood of change' = 'dicat')
             )
      ),  
      
      column(width = 2, 
             selectInput(inputId  =  'mo',
                         label = h4('Select month:'),
                         choices = mo, selected = 'Jul')
      ),
      
      column(width = 2, 
             selectInput(inputId  =  'clim',
                         label = h4('Select climate:'),
                         choices = c('Dry', 'Norm', 'Wet'), selected = 'Norm')
      ),

      column(width = 2,
             sliderInput("ln_sz", 
                         label = h4("Line size:"), 
                         min = 0, 
                         max = 5,
                         value = 1, 
                         step = 0.1
             )
      ),
      
      # select point radius
      column(width = 2,
             sliderInput("pt_sz", 
                         label = h4("Point size:"), 
                         min = 0, 
                         max = 15,
                         value = 4, 
                         step = 1
             )
      ),
      
      column(width = 12,
             
             leafletOutput('map', width = '100%', height = 550)
             
      )
            
    )

  )
  
))