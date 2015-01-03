# packages to use
library(leaflet)
library(ShinyDash)
library(RColorBrewer)
library(scales)
library(lattice)
library(plyr)
require(maps)

# data to use
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
  
  to_plo <- reactive({
    
    years <- input$years
    
    # processing
    load(file = 'data/meta.RData')
    load(file = 'data/north_am.RData')
    to_plo <- summs_fun(dat(), years, meta_in = meta, poly_in = north_am, noplot = T)
    
    return(to_plo)
    
  })
  
  map <- createLeafletMap(session, "map")
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
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


#   output$plotSummaryLocal <- renderPlot({
#     LOCAL<-countiesInBounds()[,list(NonMiningEmpC=sum(NonMiningEmpC, na.rm=TRUE), EmpCClean21=sum(EmpCClean21, na.rm=TRUE)), by=c("year")]
#     WELLS<-wellsInBounds()[,.N, by=c("year")][order(year)]
#     WELLS<-data.table(cbind("year"=WELLS$year,"N"=cumsum(WELLS$N)))
#     LOCAL<-join(LOCAL,WELLS)
#     if(any(!is.na(LOCAL$EmpCClean21)) & any(!is.na(LOCAL$N))) {
#     par(mfrow=c(1,3),oma=c(0,0,0,0),mar=c(4,1,3,0))
#     
#     plot(LOCAL[,c("year","NonMiningEmpC"),with=F], xlab="Year",main="Non-Mining Sector",type="l",col="red",cex.main=1.5,cex.lab=1.5)
#     plot(LOCAL[,c("year","EmpCClean21"),with=F], xlab="Year",main="Oil and Gas Sector",type="l",col="red",cex.main=1.5,cex.lab=1.5)
#     plot(LOCAL[,c("year","N"),with=F], xlab="Year",main="Number of Wells",type="l",col="red",cex.main=1.5,cex.lab=1.5)
#     }
#     else {
#     
#     paste("Please zoom out")
#     
#     }
# 
# })
# 
# output$plotSummaryMacro <- renderPlot({
# par(mfrow=c(1,3),oma=c(0,0,0,0),mar=c(4,1,3,0))
# ###plot unemployment rate, earnings per worker,
# 
# LOCAL<-countiesInBounds()[,list(indgasprice=sum(indgasprice * overallemp,na.rm=TRUE)/sum(overallemp,na.rm=TRUE), unemploymentrate=100*sum(unemploymentrate*labourforce,na.rm=TRUE)/sum(labourforce,na.rm=TRUE), EarnS=sum(EarnS*labourforce, na.rm=TRUE)/sum(labourforce,na.rm=TRUE)), by=c("year")]
# LOCAL<-join(NATIONAL,LOCAL)
# if(any(!is.na(LOCAL$unemploymentrate))) {
# plot(LOCAL[,c("year","unemploymentrate"),with=F], ylim=c(min(min(LOCAL$unemploymentrate),min(LOCAL$natunemploymentrate)), max(max(LOCAL$unemploymentrate),max(LOCAL$natunemploymentrate))), xlab="Year",main="Unemployment",type="l",col="red",cex.main=1.5,cex.lab=1.5)
# lines(LOCAL[,c("year","natunemploymentrate"),with=F], col="blue", lty=2)
# plot(LOCAL[,c("year","indgasprice"),with=F], ylim=c(min(min(LOCAL$indgasprice),min(LOCAL$natindgasprice)), max(max(LOCAL$indgasprice),max(LOCAL$natindgasprice))), xlab="Year",main="Natural Gas Price",type="l",col="red",cex.main=1.5,cex.lab=1.5)
# lines(LOCAL[,c("year","natindgasprice"),with=F], col="blue", lty=2)
# plot(LOCAL[,c("year","EarnS"),with=F], ylim=c(min(min(LOCAL$EarnS),min(LOCAL$natEarnS)), max(max(LOCAL$EarnS),max(LOCAL$natEarnS))), xlab="Year",main="Monthly Wages",type="l",col="red",cex.main=1.5,cex.lab=1.5)
# lines(LOCAL[,c("year","natEarnS"),with=F], col="blue", lty=2)
# } else {
# paste("Please zoom out")
# 
# }
# })

# ####FOR DATA EXPLORER
# output$datatable <- renderDataTable({
# EMP[, c("year","Area","population","shaleareashare","TPI_CI","EarnS","unemploymentrate","EmpCClean21","NonMiningEmpC","indgasprice","peind"),with=F]
# })

session$onFlushed(once=TRUE, function() {
  paintObs <- observe({

  # Clear existing circles before drawing
  map$clearShapes()
  # Draw in batches of 1000; makes the app feel a bit more responsive
  chunksize <- 1000
  wellplot<-statsInBounds()
    
  if(nrow(wellplot)>0) {

    for (from in seq.int(1, nrow(wellplot), chunksize)) {
      to <- min(nrow(wellplot), from + chunksize)
      chunkplot <- wellplot[from:to,]
    # Bug in Shiny causes this to error out when user closes browser
    # before we get here
      try(
      map$addCircle(
        chunkplot$Latitude, chunkplot$Longitude
        )
      )
    }
  }
  })

# TIL this is necessary in order to prevent the observer from
# attempting to write to the websocket after the session is gone.
session$onSessionEnded(paintObs$suspend)
})

# showWellPopup <- function(event) {
# content <- as.character(paste(event, collapse=","))
# map$showPopup(event$lat, event$lng, content)
# }
# When map is clicked, show a popup with city info
# clickObs <- observe({
# map$clearPopups()
# event <- input$map_shape_click
# if (is.null(event))
# return()
# 
# isolate({
# showWellPopup(event)
# })
# })

# session$onSessionEnded(clickObs$suspend)
})
  
