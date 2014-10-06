#Segment 0.2.0

library(httr)
source("./Libraries/Rtrava.R")
source("./Libraries/stravaUrbANA.R")
source("./Libraries/tidyDataEfforts.R")

segment_id <- 3817376 #Put here a desired segment's id
      
#Recalls tidy_SegmentEfforts from tidyDataEfforts.R
#Requires a valid token already stored in 'stoken'
dataTidy <- tidy_SegmentEfforts(stoken,segment_id)

shinyServer(function(input, output) {
            
      output$efforts_plot <- renderPlot({            
           

            y <- switch(input$Gender, 
                           "Todos" = dataTidy$Elapsed_Time,
                           "Hombres" = dataTidy[!dataTidy$Gender=="F",]$Elapsed_Time,
                           "Mujeres" = dataTidy[dataTidy$Gender=="F",]$Elapsed_Time,
                           "Usuario"= dataTidy[as.character(dataTidy$Name)==input$athlete,]$Elapsed_Time)
            
            x <- c(1:length(dataTidy$Elapsed_Time))
            
            x <- switch(input$Gender, 
                        "Todos" = x,
                        "Hombres" = x[!dataTidy$Gender=="F"],
                        "Mujeres" = x[dataTidy$Gender=="F"],
                        "Usuario"= x[as.character(dataTidy$Name)==input$athlete])
            
            tmax <- max(dataTidy$Elapsed_Time)
            
            plot(x, y,
                 main= paste("Registros del segmento", segment_id), 
                 xlab="Nro de Registro", ylab="Registro [s]",
                 ylim=c(0, tmax), xlim=c(0,length(dataTidy$Elapsed_Time)))


      })
      
      output$efforts_hist <- renderPlot({
            
            x <- c(1:length(dataTidy$Elapsed_Time))
            
            x <- switch(input$Gender, 
                           "Todos" = dataTidy$Elapsed_Time,
                           "Hombres" = dataTidy[!dataTidy$Gender=="F",]$Elapsed_Time,
                           "Mujeres" = dataTidy[dataTidy$Gender=="F",]$Elapsed_Time,
                           "Usuario"= dataTidy[as.character(dataTidy$Name)==input$athlete,]$Elapsed_Time)
            
            tmax <- max(dataTidy$Elapsed_Time)
            
            if(input$Gender=="Usuario"){
                  nbreaks <- 5
                  xmin <- min(x)*0.5
                  xmax <- max(x)*1.5
                  ylim <- NULL
            }
            else{
                  nbreaks <- 20
                  xmin <- 0
                  xmax <- tmax
                  ylim <- c(0,nrow(dataTidy)/2)
            }
            h <-hist(x, breaks=nbreaks,
                 main="Histograma del segmento 4833626 ", sub="",
                 xlab="Registro [s]", ylab="Frecuencia",
                 xlim=c(xmin, xmax), ylim=ylim)
            xfit<-seq(min(x),max(x),length=60) 
            yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
            yfit <- yfit*diff(h$mids[1:2])*length(x) 
            lines(xfit, yfit, col="blue", lwd=2)            
      })
      
      output$ui <- renderUI({

            if (is.null(input$Gender)) return()
            
            # Depending on input$Gender, we'll generate a different
            # UI component and send it to the client.
            switch(input$Gender,
                   "Usuario" = selectInput("athlete", "Usuario(a)",
                                               choices = as.character(sort(dataTidy$Name)),
#                                                selected = "Pedro Villarroel"
                   )
                   )
      })
      
})