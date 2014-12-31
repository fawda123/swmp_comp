# packages to use
library(data.table)
library(ggplot2)
library(gridExtra)
library(plyr)
library(sp)
library(maptools)
library(leaflet)
library(htmltools)

# data to use
load(file = 'data/all_dat.RData')

source('R/funcs.R')

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  ## get data given data_type and var
  dat <- reactive({
    
    var <- input$var
    var <- strsplit(var, ': ')[[1]][2]
    dat <- all_dat[[var]]
    return(dat)

  })
  
  output$years <- renderUI({
  
    yrs <- as.numeric(as.character(unique(dat()$year)))
   
    sliderInput("years", label = '',  
      min = min(yrs), max = max(yrs), 
      value = range(yrs),
      format = '####'
    )
   
  })
    
  plotInput <- reactive({
      
    # input from ui
    years <- input$years
    
    ##
    # processing
    load(file = 'data/meta.RData')
    load(file = 'data/north_am.RData')
    to_plo <- summs_fun(dat(), years, meta_in = meta, poly_in = north_am, noplot = T)
    to_plo$pop_txt <- with(to_plo, paste0(stat, ', ', lab))
    
    col_vec <- unique(to_plo[, c('lab', 'cols')])
    col_vec <- colorFactor(palette = col_vec$cols, levels = col_vec$lab)
    
    ##
    # plots
    m <- leaflet(to_plo) %>% addTiles()
    m <- m %>%  addCircleMarkers(lat = ~Latitude, lng = ~Longitude, radius = ~pval_rad,
                            color = ~col_vec(lab), opacity = 0.8, 
                            popup = ~htmlEscape(pop_txt))
    
    m <- m %>% setView(lng = -105.34, lat = 41.52, zoom = 3, options = list(animate = T, reset = F))
    
    m
    
    })

  output$outplot <- renderLeaflet(print(plotInput()))
  
  output$downloadplot <- downloadHandler(
    filename = function() { paste(input$stat, '.pdf', sep='') },
    content = function(file) {
        device <- function(..., width, height) grDevices::pdf(..., width = 13, height = 7.5)
        ggsave(file, plot = plotInput(), device = device)
    }) 
  
})