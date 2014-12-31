library(shiny)
library(leaflet)

load(file = 'data/north_am.RData')
load(file = 'data/meta.RData')

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  h2("Trends in SWMP parameters across stations"),
  
  h4('Created by Marcus W. Beck,', a('beck.marcus@epa.gov', href = 'mailto:beck.marcus@epa.gov')),
  
  p('This interactive widget provides graphical summaries of trends over time in SWMP parameters across all stations.'),
#   p('This interactive widget provides graphical summaries of water quality and weather station data from the System Wide Monitoring Program of the National Estuarine Research Reserve System ', a('(NERRS).', href = 'http://www.nerrs.noaa.gov/', target = '_blank'), 'The drop down menus can be used to select the station, date range, and parameter for plotting. The raw data used for plotting include all SWMP records from the earliest date at each station.  The data were downloaded from the', a('CDMO', href='http://cdmo.baruch.sc.edu/', target = '_blank'), 'on November 25th, 2014 and include observations up to that date.  Plots are based on daily averages for each parameter. See the', a('GitHub repository', href='https://github.com/fawda123/swmp_summary', target = '_blank'), 'for source code.'),
  
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
      
    ) #,
    
#     column(4,
#       h3('Select salinity range'), 
#     
#       selectInput(inputId = "salinity", label = '',  
#         selected = 'all',
#         choices = list(
#           'all' = 'all',
#           '00 - 06' = '00 - 06',
#           '07 - 15' = '07 - 15',
#           '16 - 20' = '16 - 20',
#           '20 - 25' = '20 - 25'
#         )
#       )
      
#     )

  ),
  
  downloadButton('downloadplot', 'Download plot'),
  
  # Show the plot
  leafletOutput('outplot')
  
#   tags$style(type="text/css",
#       ".shiny-output-error { visibility: hidden; }",
#       ".shiny-output-error:before { visibility: hidden; }")
    
))