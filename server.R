# packages to use
library(leaflet)
library(ShinyDash)
library(RColorBrewer)
library(scales)
library(lattice)
library(plyr)
library(data.table)
require(maps)

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
  
  to_plo <- reactive({
    
    years <- input$years
    
    # processing
    load(file = 'data/meta.RData')
    load(file = 'data/north_am.RData')
    to_plo <- summs_fun(dat(), years, meta_in = meta, poly_in = north_am, noplot = T)
    
    return(to_plo)
    
  })
  
  map <- createLeafletMap(session, "map")
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  statsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(to_plo()[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(to_plo(),
    Latitude >= latRng[1] & Latitude <= latRng[2] &
    Longitude >= lngRng[1] & Longitude <= lngRng[2])
  })


  session$onFlushed(once=TRUE, function() {
    
    paintObs <- observe({
  
      # Clear existing circles before drawing
      map$clearShapes()
      plo <- statsInBounds()
      browser()
      map$addCircle(plo$Longitude, plo$Latitude)
    
  #   m <- m %>% setView(lng = -105.34, lat = 41.52, zoom = 3)
    
  #   m

    })
  
  # TIL this is necessary in order to prevent the observer from
  # attempting to write to the websocket after the session is gone.
  session$onSessionEnded(paintObs$suspend)
  })

})
  
