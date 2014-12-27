library(shiny)

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
      h3('Select data type'),
  
      selectInput("data_type", label = '', 
        choices = list(
          'Water quality' = 'wq', 
          'Meteorology' = 'met', 
          'Nutrients' = 'nut'
          ),
        
        selected = "wq")
      
    ),
    
    column(4,
      h3('Select date range'), 
    
      uiOutput("years")
      
    ),
  
    column(4,
      h3('Select variable'), 
    
      uiOutput("parms")
      
    )
    
  ),
  
#   downloadButton('downloadplot', 'Download plot'),
  
  # Show the plot,
  plotOutput("outplot", width = "100%") #,
  
#   tags$style(type="text/css",
#       ".shiny-output-error { visibility: hidden; }",
#       ".shiny-output-error:before { visibility: hidden; }")
    
))