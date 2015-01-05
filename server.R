library(leaflet)
library(ggplot2)
library(maps)
library(plyr)

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
  
  makeReactiveBinding('selectedstation')
  
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
  
  # Define some reactives for accessing the data
  
  # Retrieve the name of the column that contains the selected year's
  # population
  popCol <- reactive({
    paste('Pop', input$year, sep='')
  })
  
  popSeries <- function(city) {
    c(
      sum(city$Pop2000),
      sum(city$Pop2001),
      sum(city$Pop2002),
      sum(city$Pop2003),
      sum(city$Pop2004),
      sum(city$Pop2005),
      sum(city$Pop2006),
      sum(city$Pop2007),
      sum(city$Pop2008),
      sum(city$Pop2009),
      sum(city$Pop2010)
    )
  }
  
  # The cities that are within the visible bounds of the map
  citiesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(uspop2000[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    browser()
    
    subset(uspop2000,
           Lat >= latRng[1] & Lat <= latRng[2] &
             Long >= lngRng[1] & Long <= lngRng[2])
  })
  
  # The top N cities (by population) that are within the visible bounds
  # of the map
  topCitiesInBounds <- reactive({
    cities <- citiesInBounds()
    cities <- head(cities[order(cities[[popCol()]], decreasing=TRUE),],
                   as.numeric(input$maxCities))
  })
  
  # Create the map; this is not the "real" map, but rather a proxy
  # object that lets us control the leaflet map on the page.
  map <- createLeafletMap(session, 'map')
  
  observe({
    if (is.null(input$map_click))
      return()
    selectedstation <<- NULL
  })
  
  radiusFactor <- 1000
  observe({
    map$clearShapes()
    cities <- topCitiesInBounds()
    
    if (nrow(cities) == 0)
      return()
    
    map$addCircle(
      cities$Lat,
      cities$Long,
      sqrt(cities[[popCol()]]) * radiusFactor / max(5, input$map_zoom)^2,
      row.names(cities),
      list(
        weight=1.2,
        fill=TRUE,
        color='#4A9'
      )
    )
  })
  
  observe({
    event <- input$map_shape_click
    if (is.null(event))
      return()
    map$clearPopups()
    
    isolate({
      cities <- topCitiesInBounds()
      city <- cities[row.names(cities) == event$id,]
      selectedstation <<- city
      content <- as.character(tagList(
        tags$strong(paste(city$City, city$State)),
        tags$br(),
        sprintf("Estimated population, %s:", input$year),
        tags$br(),
        prettyNum(city[[popCol()]], big.mark=',')
      ))
      map$showPopup(event$lat, event$lng, content, event$id)
    })
  })
  
  output$desc <- reactive({
    if (is.null(input$map_bounds))
      return(list())
    list(
      lat = mean(c(input$map_bounds$north, input$map_bounds$south)),
      lng = mean(c(input$map_bounds$east, input$map_bounds$west)),
      zoom = input$map_zoom,
      shownCities = nrow(topCitiesInBounds()),
      totalCities = nrow(citiesInBounds())
    )
  })
  
  output$data <- renderTable({
    if (nrow(topCitiesInBounds()) == 0)
      return(NULL)
    
    data.frame(
      City = paste(topCitiesInBounds()$City, topCitiesInBounds()$State),
      Population = topCitiesInBounds()[[popCol()]])
  }, include.rownames = FALSE)
  
  # plot label here
  output$cityTimeSeriesLabel <- renderText({
    if (is.null(selectedstation)) {
      'Total population of visible cities'
    } else {
      paste('Population of ',
            selectedstation$City,
            ', ',
            selectedstation$State,
            sep='')
    }
  })
  
  # plot here
  output$cityTimeSeries <- renderPlot({
    cities <- NULL
    if (!is.null(selectedstation))
      cities <- selectedstation
    else
      cities <- topCitiesInBounds()
    
    popData <- popSeries(cities) / 1000
    df <- data.frame(year = c(2000:2010), pop = popData)
    p <- ggplot(df, aes(x = year, y = pop)) + geom_line()
    p <- p + ylim(c(0, max(popData)))
    p <- p + ylab('Population (thousands)')
    p <- p + scale_x_continuous(breaks = seq(2000, 2010, 2))
    print(p)
  })
})