library(leaflet)
library(ggplot2)
library(maps)
library(plyr)
library(data.table)
library(RColorBrewer)
library(httr)
library(XML)
library(RJSONIO)

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
                sep = '', ticks = FALSE
    )
    
  })
  
  # summarized multi-station data
  to_plo <- reactive({
    
    years <- input$years
    sumby <- input$sumby
    
    # processing
    load(file = 'data/meta.RData')
    
    summs_fun(dat(), years, sumby = sumby, meta_in = meta)
  })
  
  stations <- reactive({
    to_plo()[['res']]
  })
  

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles("//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      setView(lng = -94, lat = 40, zoom = 3)
  })
  
  map <- leafletProxy("map")

  # add legend
  map <- map %>% 
      addLegend(position = 'bottomleft', title = 'Trend colors', 
        colors = rgb(t(col2rgb(colors()[c(520, 522, 523, 503, 506, 507)])) / 255),
        labels = c('NEG (nonsig)', 'NEG (p<0.05)', 'NEG (p<0.01)','POS (nonsig)', 'POS (p<0.05)', 'POS (p<0.01)'), 
        opacity = 0.7
      )
  
  makeReactiveBinding('selectedstation')
  
  # Set selected station when marker is clicked
  observeEvent(input$map_marker_click, {
    selectedstation <<- stations()[stations()$stat %in% input$map_marker_click$id]
  })
  # Clear the selected station when map background is clicked
  observeEvent(input$map_click, {
    selectedstation <<- NULL
  })
  
  # Maintain circle markers
  observeEvent(stations(), {

    selectedstation <<- NULL
    map %>% clearMarkers()
    
    if (nrow(stations()) == 0)
      return()
    
    # colors
    col_vec <- stations()[['cols']]
    col_vec <- scales::rescale(col2rgb(col_vec), c(0, 1))
    col_vec <- rgb(t(col_vec))

    map %>% addCircleMarkers(data = stations(),
      ~Longitude,
      ~Latitude,
      radius = ~pval_rad, 
      layerId = ~stat, # this is the id that's returned on map click
      stroke = FALSE,
      color = col_vec,
      fillOpacity = 0.7,
      opacity = 0,
      label = ~paste0(stat, ": ", lab)
    )
    
  })
  
  # for description of widget
  output$desc <- reactive({
    if (is.null(input$map_center)) {
      return(list())
    }
    
    list(
      lat = round(input$map_center$lat, 2),
      lng = round(input$map_center$lng, 2),
      zoom = input$map_zoom
    )
  })
  
  # summary plot for individual station
  output$statid <- renderPlot({
    validate(need(selectedstation, FALSE))    

    stat <- selectedstation$stat
    sel_dat <- to_plo()[['raw_dat']][[as.character(stat)]]
    param <- gsub('nut: |wq: |met: ', '', input$var)
    trend_in <- data.frame(stations())
    trend_in <- trend_in[trend_in$stat == as.character(stat), 'lab']
    trend_in <- as.character(trend_in)
    sumby <- input$sumby
    
    plot_summary(sel_dat, param, stat, input$years, trend_in, sumby)
   
  })

})