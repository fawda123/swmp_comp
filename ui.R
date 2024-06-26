library(shiny)
library(leaflet)
shinyUI(fluidPage(
  
  theme = 'styles.css',
  
  leafletOutput("map", "100%", 430),
  
  # title
  fluidRow(
    column(12, offset=0,
      h2('Trends in SWMP parameters'),
      h4(HTML("Created by Marcus W. Beck, <a href='mailto:mbeck@tbep.org'>mbeck@tbep.org</a>, Todd O'Brien, <a href='mailto:todd.obrien@noaa.gov'>todd.obrien@noaa.gov</a>")),
      HTML(paste0('This widget is an interactive tool to explore trends in SWMP data.  Trends are described by an increase or decrease in values over time using a simple linear regression of summarized data.  The regression for each station can be viewed by clicking on a map location.  Trends at each station are plotted as circles that identify the direction and significance of the trend.  The trend direction is blue for decreasing and red for increasing.  The significance is indicated by radius of the circle and color shading where larger points with darkers colors indicate a strong trend.  Original data are available from <a href="http://cdmo.baruch.sc.edu/">http://cdmo.baruch.sc.edu/</a>. See the <a href="https://github.com/fawda123/swmp_comp">GitHub repository</a> for source code. The data include observations through December 2023 (if available) and are current as of June 24th, 2024. Please note that the use of simple regression to identify trends is for exploratory purposes only and may not be appropriate for all datasets.  Trends also do not account for missing observations in each year.  The map is centered at <b>', textOutput('lat', inline = T), '</b>, <b>', textOutput('lng', inline = T), '</b> with a zoom level of <b>', textOutput('zoom', inline = T), '</b>.'))
    )
  ),
  
#   hr(),
  
  # controls
  fluidRow(
      
    column(3, 
  
           # select parameter
           selectInput(inputId = "var", label = h4('Select parameter:'),  
                       selected = 'wq: temp',
                       choices = list(
                         'wq: Temperature (C)' = 'wq: temp',
                         'wq: Specific conductivity (mS/cm)' = 'wq: spcond',
                         'wq: Salinity (psu)' = 'wq: sal',
                         'wq: Dissolved oxyxgen (%)' = 'wq: do_pct',
                         'wq: Dissolved oxygen (mg/L)' = 'wq: do_mgl',
                         'wq: Depth (m)' = 'wq: depth',
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
                         'nut: Orthophosphate (mg/L)' = 'nut: po4f', 
                         'nut: Ammonium (mg/L)' = 'nut: nh4f',
                         'nut: Nitrite (mg/L)' = 'nut: no2f', 
                         'nut: Nitrate (mg/L)' = 'nut: no3f',
                         'nut: Nitrite + Nitrate (mg/L)' = 'nut: no23f', 
                         'nut: Chlorophyll-a (ug/L)' = 'nut: chla_n'
                       )
           ), 
           
           # summarize by...
           selectInput(inputId = "sumby", label = h4('Summarize by:'),  
             selected = 'yrs_summ',
             choices = list('Years: anomalies' = 'yrs_anom', 'Years: averages' = 'yrs_avgs', 'Months: anomalies' = 'mos_anom', 'Months: averages' = 'mos_avgs')
           ),
      
           # select date range...
           uiOutput('years')
           
    ),
    
    column(9, offset = 0,
           h4(id='placeholder', class='shiny-text-output'),
           plotOutput('statid', width='100%', height='300px')
    )
    
  )

))