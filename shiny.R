library(shiny)

ui <- fluidPage(
  selectizeInput(inputId = "code", label = "Choose the airport code", 
                 choices=read.table("airportCode")[,2], 
                 selected=NULL,
                 multiple=T),
  checkboxGroupInput(inputId = "feature", label = "Choose the weather feature", 
                 choices=c("temp_f","temp_c","wind_mph","wind_kt","wind_dir"), 
                 selected=NULL),
  mainPanel(
    tabsetPanel(
      tabPanel("Airport Information", tableOutput("data_table")),
      tabPanel("Plot for Weather Information",
               fluidRow(
                 column(width=10,
                        plotOutput("plot_temp_c"),
                        plotOutput("plot_temp_f"),
                        plotOutput("plot_wind_mph"),
                        plotOutput("plot_wind_kt"),
                        plotOutput("plot_wind_dir")
                        )
               )
      )
    )
  )
) 

server <- function(input, output) {
  output$data_table <- renderTable({
    current_weather_more(as.character(input$code), input$feature)
  })
  output$plot_temp_c <- renderPlot({
    if("temp_c" %in% input$feature){
      plot_temp_c(input$code)
    }
  })
  output$plot_temp_f <- renderPlot({
    if("temp_f" %in% input$feature){
      plot_temp_f(input$code)
    }
  })
  output$plot_wind_mph <- renderPlot({
    if("wind_mph" %in% input$feature){
      plot_wind_mph(input$code)
    }
  })
  output$plot_wind_kt <- renderPlot({
    if("wind_kt" %in% input$feature){
      plot_wind_kt(input$code)
    }
  })
  output$plot_wind_dir <- renderPlot({
    if("wind_dir" %in% input$feature){
      plot_wind_dir(input$code)
    }
  })
}

shinyApp(ui = ui, server = server)    