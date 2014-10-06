
shinyUI(
      fluidPage(
            
            titlePanel("UrbANA Strava 0.1.0"),
            
            sidebarLayout(
                  sidebarPanel(
                        h4("Segment App 0.2.0"),
            
                        radioButtons("Gender", strong("Seleccione el objetivo"),
                                     choices= list("Todos", "Hombres", "Mujeres", "Usuario")),
                        
                        uiOutput("ui")
                  ),
                  mainPanel(plotOutput("efforts_plot"), plotOutput("efforts_hist"))
            )
      )
)
