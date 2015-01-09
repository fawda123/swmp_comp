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
library(shinyBS)

# names of SWMP files on server
files_s3 <- httr::GET('https://s3.amazonaws.com/swmpagg/')$content
files_s3 <- rawToChar(files_s3)
files_s3 <- htmlTreeParse(files_s3, useInternalNodes = T)
files_s3 <- xpathSApply(files_s3, '//contents//key', xmlValue)

mo_labs <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
mo_levs <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

# load data
data(all_dat)
data(meta)
data(north_am)

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
    
    sliderInput("years", label = '',  
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
    
    map$clearPopups()
    map$clearShapes()
    
    years <- input$years
    
    # processing
    load(file = 'data/meta.RData')
    
#     browser()
    to_plo <- summs_fun(dat(), years, meta_in = meta)
    
    return(to_plo)
    
  })
    
  # The stations that are within the visible bounds of the map
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
  
  makeReactiveBinding('selectedstation')

  observe({
    if (is.null(input$map_click))
      return()
    selectedstation <<- NULL
  })
  
  # map points
  observe({
    map$clearShapes()
    map$clearPopups()
    stations <- statsInBounds()
    
    if (nrow(stations) == 0)
      return()
    
    # colors
    col_vec <- unique(stations[, c('lab', 'cols')])
    col_vec <- colorFactor(palette = col_vec$cols, levels = col_vec$lab)

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
  
  ## retrieve from AmazonS3, uses httr GET
  sel_dat <- reactive({
    
    if (!is.null(selectedstation))
      stat_txt <- as.character(selectedstation$stat)
    else
      return()
    
    raw_content <- paste0('https://s3.amazonaws.com/swmpagg/', stat_txt, '.RData')
    raw_content <- httr::GET(raw_content)$content
    connect <- rawConnection(raw_content)
    load(connect)
    sel_dat <- get(stat_txt)
    rm(list = stat_txt)
    close(connect) 
    
    # year, month categories
    sel_dat$year <- strftime(sel_dat$datetimestamp, '%Y')
    sel_dat$month <- strftime(sel_dat$datetimestamp, '%m')
    sel_dat$month <- factor(sel_dat$month, labels = mo_levs, levels = mo_levs)
    
    return(sel_dat)
    
  })

  # plot here
  output$statid <- renderPlot({
    if (!is.null(selectedstation))
      stat <- selectedstation
    else
      return()
#     browser()
    param <- gsub('nut: |wq: |met: ', '', input$var)
    stat <- as.character(selectedstation$stat)
    plot_summary(sel_dat(), param, stat, input$years)
   
  })

})