library(shiny)

ui <- fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  fluidRow(
    column(width = 5, wellPanel(
      selectizeInput(inputId = "code", label = "Choose the airport code", 
                     ###Read airpor code data from your computer
                     ###Will change the path in the package
                     choices=read.table("Box/airportCode")[,2], 
                     selected=NULL,
                     multiple=T)
    )),
    column(width = 5, wellPanel(
      checkboxGroupInput(inputId = "feature", label = "Choose the weather feature", 
                         choices=c("temp_f","temp_c","wind_mph","wind_kt","wind_dir"), 
                         selected=NULL)
    ))
  ),

  plotOutput("plot_map", click = clickOpts(id = "plot_click")),
  tableOutput("click_info")

)  
 

server <- function(input, output) {
 output$plot_map <- renderPlot({
   plot_map(input$code)
 })
 output$click_info <- renderTable({
   data <- current_weather_more(input$code,input$feature)
   subset(data,latitude >= input$plot_click$y-0.5 & latitude <= input$plot_click$y+0.5 & 
            longitude >= input$plot_click$x-0.5 & longitude <= input$plot_click$x+0.5)
 })
}

shinyApp(ui = ui, server = server)    
