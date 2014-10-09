segments <- dget("./dataTidy/segments.R")

shinyUI(
      fluidPage(
            
            titlePanel("UrbANA Strava 0.1.0"),
            
            sidebarLayout(
                  sidebarPanel(
                        h4("Segment App 0.3.6"),
                        
                        selectInput("segment", strong("Segmento"),
                                    choices = as.character(segments$name)),
                        
                        radioButtons("Gender", strong("Seleccione el objetivo"),
                                     choices= list("Todos", "Hombres", "Mujeres", "Usuario")),
                        
                        uiOutput("ui"),
                        
                        sliderInput("range", strong("Rango:"), min = 0, max = 100, value = c(0,98))
#                         h5("Visualizador de variables"),
#                         verbatimTextOutput("variables")
                  ),
                  mainPanel(plotOutput("efforts_plot"), plotOutput("efforts_hist"))
            )
      )
)
