library(leaflet)
library(ShinyDash)

shinyUI(fluidPage(
  tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
  leafletMap(
    "map", "100%", 400,
    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
    options=list(
      center = c(37.45, -93.85),
      zoom = 4,
      maxBounds = list(list(17, -180), list(59, 180))
    )
  ),
  
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
    column(8, offset=3,
           h2('Trends in estuarine water quality parameters using SWMP'),
           htmlWidgetOutput(
             outputId = 'desc',
             HTML(paste(
               'The map is centered at <span id="lat"></span>, <span id="lng"></span>',
               'with a zoom level of <span id="zoom"></span>.<br/>',
               'Top <span id="shownCities"></span> out of <span id="totalCities"></span> visible cities are displayed.'
             ))
           )
    )
  ),

  hr(),
  fluidRow(
    column(3,
           selectInput('year', 'Year', c(2000:2010), 2010),
           selectInput('maxCities', 'Maximum cities to display', choices=c(
             5, 25, 50, 100, 200, 500, 2000, 5000, 10000, All = 100000
           ), selected = 100)
    ),
    column(4,
           h4('Visible cities'),
           tableOutput('data')
    ),
    column(5,
           h4(id='cityTimeSeriesLabel', class='shiny-text-output'),
           plotOutput('cityTimeSeries', width='100%', height='250px')
    )
  )

))