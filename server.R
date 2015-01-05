library(leaflet)
library(ggplot2)
library(maps)
library(plyr)
library(data.table)

data(uspop2000)
data(all_dat)
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
    
  # The stations that are within the visible bounds of the map
  statsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(uspop2000[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
#     browser()
    
    subset(to_plo(),
           Latitude >= latRng[1] & Latitude <= latRng[2] &
             Longitude >= lngRng[1] & Longitude <= lngRng[2])
  })
  
  # Create the map; this is not the "real" map, but rather a proxy
  # object that lets us control the leaflet map on the page.
  map <- createLeafletMap(session, 'map')
  
  makeReactiveBinding('selectedstation')

  observe({
    if (is.null(input$map_click))
      return()
    selectedstation <<- NULL
  })
  
  observe({
    map$clearShapes()
    stations <- statsInBounds()
    
    if (nrow(stations) == 0)
      return()
    
#     browser()
    
    # colors
    col_vec <- unique(stations[, c('lab', 'cols')])
    col_vec <- colorFactor(palette = col_vec$cols, levels = col_vec$lab)
    
#     browser()

    map$addCircleMarker(
      stations$Latitude,
      stations$Longitude,
      stations$pval_rad, 
      rownames(stations),
      list(
        weight=1.2,
        fill=TRUE,
        color=col_vec(stations$lab)
      )
    )

  })
  
  observe({
    event <- input$map_marker_click
    if (is.null(event))
      return()
    map$clearPopups()
    
    stations <- statsInBounds()
    
#     browser()
    isolate({
      stat <- stations[row.names(stations) == event$id,]
      selectedstation <<- stat
      content <- as.character(tagList(
        tags$strong(stat$stat),
        tags$br(),
        stat$lab
      ))
      map$showPopup(event$lat, event$lng, content, event$id)
    })
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
  
#   # plot label here
#   output$cityTimeSeriesLabel <- renderText({
#     if (is.null(selectedstation)) {
#       'Total population of visible cities'
#     } else {
#       paste('Population of ',
#             selectedstation$City,
#             ', ',
#             selectedstation$State,
#             sep='')
#     }
#   })
  
  # plot here
  output$statid <- renderPlot({
    if (!is.null(selectedstation))
      stat <- selectedstation
    else
      return()
    
    p <- plot(rnorm(100))
    print(p)
  })

})