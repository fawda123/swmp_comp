# packages to use
library(data.table)
library(ggplot2)
library(gridExtra)
library(plyr)
library(sp)
library(maptools)

# data to use
# load(file = 'data/nut_dat.RData')
# load(file = 'data/met_dat.RData')
# load(file = 'data/wq_dat.RData')
load(file = 'data/all_dat.RData')

source('R/funcs.R')

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
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
    
  plotInput <- reactive({
      
    # input from ui
    years <- input$years
    
    ##
    # preprocessing
    load(file = 'data/meta.RData')
    load(file = 'data/north_am.RData')
    p1 <- summs_fun(dat(), years, meta_in = meta, poly_in = north_am)

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