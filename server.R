library(leaflet)
library(ggplot2)
library(maps)
library(plyr)
library(data.table)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(httr)
library(XML)

# load data
data(all_dat)
data(meta)

# functions
source('R/funcs.R')

# From a future version of Shiny
bindEvent <- function(eventExpr, callback, env=parent.frame(), quoted=FALSE) {
  eventFunc <- exprToFunction(eventExpr, env, quoted)
  
  initialized <- FALSE
  invisible(observe({
    eventVal <- eventFunc()
    if (!initialized)
      initialized <<- TRUE
    else
      isolate(callback())
  }))
}

# reactive functions for app
shinyServer(function(input, output, session) {
  
  ## Define some reactives for accessing the data
  ## get data given data_type and var
  dat <- reactive({

    var <- input$var
    var <- strsplit(var, ': ')[[1]][2]
    dat <- all_dat[[var]]
    return(dat)
    
  })
  
  output$years <- renderUI({
    
    yrs <- as.numeric(as.character(unique(dat()$year)))
    
    sliderInput("years", label = h4('Select date range:'),  
                min = min(yrs), max = max(yrs), 
                value = range(yrs),
                format = '####'
    )
    
  })
  
  # Create the map; this is not the "real" map, but rather a proxy
  # object that lets us control the leaflet map on the page.
  map <- createLeafletMap(session, 'map')
  
  # summarized multi-station data
  to_plo <- reactive({
    
    years <- input$years
    sumby <- input$sumby
    
    # processing
    load(file = 'data/meta.RData')
    
    to_plo <- summs_fun(dat(), years, sumby = sumby, meta_in = meta)
    
    # remove popups if parameters change
    map$clearPopups()
    
    # clear map if parameters change
    map$clearMarkers()
     
    return(to_plo)
    
  })
    

  makeReactiveBinding('selectedstation')
  
  # map points
  observe({

    stations <- to_plo()[['res']]
    
    if (nrow(stations) == 0)
      return()
    
    # colors
    col_vec <- stations[['cols']]
    col_vec <- scales::rescale(col2rgb(col_vec), c(0, 1))
    col_vec <- rgb(t(col_vec))

    map$addCircleMarker(
      stations$Latitude,
      stations$Longitude,
      stations$pval_rad, 
      stations$stat, # this is the id that's returned on map click
      list(
        weight=0, # width of circle perimeters
        fill=TRUE,
        color=col_vec,
        fillOpacity = 0.7,
        opacity = 0 # opacity of circle perimeter
        )
    )

  })
  
  observe({
    
    event <- input$map_marker_click
    
    # this exits the function if nothing is clicked
    if (is.null(event)){
      map$clearPopups()
      return()
    }
    
    map$clearPopups()
    
    # select content to display on popup
    stations <- to_plo()[['res']]
    stat <- stations[stations$stat %in% event$id, ]
    selectedstation <<- stat
    content <- as.character(tagList(
      tags$strong(selectedstation$stat),
      tags$br(),
      selectedstation$lab
    ))
    map$showPopup(event$lat, event$lng, content, event$id)

  })
  
  # for description of widget
  output$desc <- reactive({
    if (is.null(input$map_bounds))
      return(list())
    list(
      lat = round(mean(c(input$map_bounds$north, input$map_bounds$south)), 2),
      lng = round(mean(c(input$map_bounds$east, input$map_bounds$west)), 2),
      zoom = input$map_zoom
    )
  })
  
  # summary plot for individual station
  output$statid <- renderPlot({
    
    event <- input$map_marker_click
    
    # this exits the function if nothing is clicked
    if (is.null(event)){
      map$clearPopups()
      return()
    }
    
    stat <- selectedstation$stat
    sel_dat <- to_plo()[['raw_dat']][[as.character(stat)]]
    param <- gsub('nut: |wq: |met: ', '', input$var)
    trend_in <- data.frame(to_plo()[['res']])
    trend_in <- trend_in[trend_in$stat == as.character(stat), 'lab']
    trend_in <- as.character(trend_in)
    
    plot_summary(sel_dat, param, stat, input$years, trend_in)
   
  })

})