#SegmentApp 0.4.0

# library(httr)
# source("Rtrava.R")
# source("stravaUrbANA.R")
# source("tidyDataEfforts.R")
library(lubridate)
source("functions.R")

segments <- dget("./dataTidy/segments.R")

selected <- "Pedro Villarroel"

shinyServer(function(input, output) {
      
      dataSegment <- reactive({
            id <- segments[as.character(segments$name)==input$segment,1]
            #id <- "4079969.R"
            dget(paste("./dataTidy/",id,".R",sep=""))
      })
      
      dataInput <- reactive({
            dataTidy <- dataSegment()
            datarange <- quantile(dataTidy$Elapsed_Time, input$range/100)
            dataTidy <- dataTidy[dataTidy$Elapsed_Time >= datarange[1] & 
                                       dataTidy$Elapsed_Time <= datarange[2] ,]
            dataTidy$Name <- escaped_unicode(dataTidy$Name)
            dataTidy$Date <- ymd_hms(dataTidy$Date)
            
            dataTidy
      })
      
      output$efforts_plot <- renderPlot({
            dataTidy <- dataInput()            
            
            #dates <- ymd_hms(dataTidy$Date)
            tmax <- max(dataTidy$Elapsed_Time)
            tmin <- min(dataTidy$Elapsed_Time)
            with(dataTidy, plot(Date, Elapsed_Time, type="n",
                                main=paste("Registros del segmento",input$segment),
                                xlab="Fecha del registro", ylab="Registro [s]"))
            
            switch(input$Gender,
                   "Todos" = {
                         plot_efforts(dataTidy, KOM = TRUE, Males = TRUE)
                         plot_efforts(dataTidy, KOM = TRUE, Males = FALSE)
                         legend("topright", pch = c(1,19,1,19) , col = c("blue", "blue", "violetred1", "violetred1"), legend = c("Hombres", "KOMs", "Mujeres", "QOMs"))
                                      },
                   "Hombres" = {
                         plot_efforts(dataTidy, KOM = TRUE, Males = TRUE)
                         legend("topright", pch = c(1,19), col = c("blue", "blue"), legend = c("Hombres", "KOMs"))
                   },
                   "Mujeres" = {
                         plot_efforts(dataTidy, KOM = TRUE, Males = FALSE)
                         legend("topright", pch = c(1,19), col = c("violetred1","violetred1"), legend = c("Mujeres", "QOMs"))
                   },
                   "Usuario"= {
                         colores <- c("springgreen4", "red", "blue", "black", "orange", "darkmagenta", "green", "yellow")
                         selected <- chosen()
                         i <- 0
                         for(athlete in selected){
                               i <- i + 1
                               plot_efforts(dataTidy, KOM = TRUE, athlete = athlete, acolor = colores[i])
                               abline(h = mean(dataTidy[dataTidy$Name==athlete,'Elapsed_Time']), lty = 2, col = colores[i])
                         }
                         legend("topright", pch = 1, col = colores[1:length(selected)], legend = selected)
                   }
            )
            #abline(h=mean(dataTidy$Elapsed_Time), lty=2)
            abline(h = mean(dataTidy[!dataTidy$Gender=="F",'Elapsed_Time']), lty = 2, col = "blue")
            abline(h = mean(dataTidy[dataTidy$Gender=="F",'Elapsed_Time']), lty = 2, col = "violetred1")            
            
      })
      
      output$efforts_hist <- renderPlot({
            dataTidy <- dataInput()            
            
            x <- switch(input$Gender, 
                        "Todos" = dataTidy$Elapsed_Time,
                        "Hombres" = dataTidy[!dataTidy$Gender=="F",]$Elapsed_Time,
                        "Mujeres" = dataTidy[dataTidy$Gender=="F",]$Elapsed_Time,
                        "Usuario"= {selected <- chosen()
                                    
                                    usersdata <- logical(length(dataTidy$Name))
                                    i <- 0
                                    for(athlete in selected){
                                          i <- i + 1
                                          user <- dataTidy$Name == athlete
                                          usersdata <- as.logical(usersdata + user)
                                    }
                                    dataTidy[usersdata,]$Elapsed_Time
                        })
            
            tmax <- max(dataTidy$Elapsed_Time)
            
            if(input$Gender=="Usuario"){
                                    #     
                  xmin <- min(x)*0.5
                  xmax <- max(x)*1.5
                  # ylim <- NULL
            }
            else{
                  nbreaks <- 20
                  #                   xmin <- 0
                  #                   xmax <- tmax*3/5
                  xmin <- min(x)*0.5
                  xmax <- max(x)*1.5
                  # ylim <- c(0,nrow(dataTidy)/2)
            }
            h <-hist(x, #breaks=nbreaks,
                     main=paste("Histograma de registros del segmento",input$segment), sub="",
                     xlab="Registro [s]", ylab="Frecuencia",
                     xlim=c(xmin, xmax), col="darkorchid4" #ylim=ylim
            )
            rug(x)
            xfit <- seq(min(x),max(x),length=60) 
            yfit <- dnorm(xfit,mean=mean(x),sd=sd(x)) 
            yfit <- yfit*diff(h$mids[1:2])*length(x) 
            lines(xfit, yfit, col="blue", lwd=2)            
      })
      
      chosen <- reactive({
            
            if (is.null(input$athlete)) return("Pedro Villarroel")
            input$athlete
            
      })
      
      output$ui <- renderUI({
            
            if (is.null(input$Gender)) return()
            selected <- chosen()
            dataTidy <- dataInput()
            if(is.null(selected)){
                  selected <- sort(dataTidy$Name)[[1]]
            }
            # Depending on input$Gender, we'll generate a different
            # UI component and send it to the client.
            if(input$Gender=="Usuario"){
                  choices <- sort(unique(dataTidy$Name))
                  selectInput("athlete", "Usuario(a)",
                              choices = choices,
                              selected = selected,
                              multiple=TRUE, 
                              selectize=TRUE
                  )
            }
      })
      
})