# packages to use
library(data.table)
library(ggplot2)
library(gridExtra)
library(plyr)

# data to use
load(file = 'data/nut_dat.RData')
load(file = 'data/met_dat.RData')
load(file = 'data/wq_dat.RData')
load(file = 'data/met_dat.RData')
load(file = 'data/north_am.Rdata')
load(file = 'data/meta.RData')

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  # varsiable to select given data type
  output$parms <- renderUI({
  
    vars <- list(
    
      wq = list(
        'Temperature (C)' = 'temp',
        'Specific conductivity (mS/cm)' = 'spcond',
        'Salinity (psu)' = 'sal',
        'Dissolved oxyxgen (%)' = 'do_pct',
        'Dissolved oxygen (mg/L)' = 'do_mgl',
        'Depth (m)' = 'depth',
        'Referenced depth (m)' = 'level',
        'pH' = 'ph',
        'Turbidity (NTU)' = 'turb',
        'Chl fluorescence (ug/L)' = 'chlfluor'
        ),
      
      met = list(
        'Air temperature (C)' = 'atemp',
        'Relative humidity (%)' = 'rh',
        'Barometric pressure (mb)' = 'bp',
        'Wind speed (m/s)' = 'wspd',
        'Max wind speed (m/s)' = 'maxwspd',
        'Wind direction (degrees)' = 'wdir',
        'Wind direction (sd, degrees)' = 'sdwdir',
        'Total PAR (mmol/m2)' = 'totpar',
        'Total precipitation (mm)' = 'totprcp',
        'Cumulative precipitation (mm)' = 'cumprcp',
        'Total solar radiation (watts/m2)' = 'totsorad'
        ), 
      
      nut = list(
        'Orthophosphate (mg/L)' = 'po4f', 
        'Ammonium (mg/L)' = 'nh4f',
        'Nitrite (mg/L)' = 'no2f', 
        'Nitrate (mg/L)' = 'no3f',
        'Nitrite + Nitrage (mg/L)' = 'no23f', 
        'Chlorophyll-a (ug/L)' = 'chla_n'
        )
    
      )
  
    # select appropriate type
    var_sub <- vars[[input$data_type]]

    selectInput(inputId = "var", label = '',  
      choices = var_sub,
      selected = var_sub[[1]]
    )
   
  })
  
  ## get data given data_type and var
  dat <- reactive({
    
    data_type <- input$data_type
    var <- input$var
    dat <- get(paste0(data_type, '_dat'))
    dat <- dat[[var]]
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
    
  plotInput <- reactive({
      
    # input from ui
    years <- input$years
    
    ##
    # preprocessing
    p1 <- summs_fun(dat(), years)

    ##
    # plots
    arrangeGrob(p1)
    
    })
  
  output$outplot <- renderPlot({
    print(plotInput())
    }, height = 600, width = 1100)
  
  output$downloadplot <- downloadHandler(
    filename = function() { paste(input$stat, '.pdf', sep='') },
    content = function(file) {
        device <- function(..., width, height) grDevices::pdf(..., width = 13, height = 7.5)
        ggsave(file, plot = plotInput(), device = device)
    }) 
  
})