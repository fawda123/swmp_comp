library(leaflet)
library(shiny)
library(ShinyDash)
shinyUI(fluidPage(
  
  # Application title
  h2("Trends in SWMP parameters across stations"),
  
  h4('Created by Marcus W. Beck,', a('beck.marcus@epa.gov', href = 'mailto:beck.marcus@epa.gov')),
  
  p('This interactive widget provides graphical summaries of trends over time in SWMP parameters across all stations.'),

  # buttons on top
  fluidRow(
    
    column(4, 
      h3('Select parameter'),
     
      selectInput(inputId = "var", label = '',  
        selected = 'nut: po4f',
        choices = list(
          'wq: Temperature (C)' = 'wq: temp',
          'wq: Specific conductivity (mS/cm)' = 'wq: spcond',
          'wq: Salinity (psu)' = 'wq: sal',
          'wq: Dissolved oxyxgen (%)' = 'wq: do_pct',
          'wq: Dissolved oxygen (mg/L)' = 'wq: do_mgl',
          'wq: Depth (m)' = 'wq: depth',
          'wq: Referenced depth (m)' = 'wq: level',
          'wq: pH' = 'wq: ph',
          'wq: Turbidity (NTU)' = 'wq: turb',
          'wq: Chl fluorescence (ug/L)' = 'wq: chlfluor',
          'met: Air temperature (C)' = 'met: atemp',
          'met: Relative humidity (%)' = 'met: rh',
          'met: Barometric pressure (mb)' = 'met: bp',
          'met: Wind speed (m/s)' = 'met: wspd',
          'met: Max wind speed (m/s)' = 'met: maxwspd',
          'met: Wind direction (degrees)' = 'met: wdir',
          'met: Wind direction (sd, degrees)' = 'met: sdwdir',
          'met: Total PAR (mmol/m2)' = 'met: totpar',
          'met: Total precipitation (mm)' = 'met: totprcp',
          'met: Cumulative precipitation (mm)' = 'met: cumprcp',
          'met: Total solar radiation (watts/m2)' = 'met: totsorad',
          'nut: Orthophosphate (mg/L)' = 'nut: po4f', 
          'nut: Ammonium (mg/L)' = 'nut: nh4f',
          'nut: Nitrite (mg/L)' = 'nut: no2f', 
          'nut: Nitrate (mg/L)' = 'nut: no3f',
          'nut: Nitrite + Nitrate (mg/L)' = 'nut: no23f', 
          'nut: Chlorophyll-a (ug/L)' = 'nut: chla_n'
        )
      )
      
    ),
    
    column(4,
      h3('Select date range'), 
    
      uiOutput("years")
      
    )
  
),
  
  fluidRow(
    
    column(12, 
    div(class="outer",
  
  tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
    includeScript("gomap.js")
  ),

  leafletMap("map", width="100%", height="60%",
    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
    options=list(
      center = c(37.45, -93.85),
      zoom = 5, maxZoom=9,
    maxBounds = list(list(15.961329,-129.92981), list(52.908902,-56.80481)) # Show US only
    )
  ))
    
  ))
))