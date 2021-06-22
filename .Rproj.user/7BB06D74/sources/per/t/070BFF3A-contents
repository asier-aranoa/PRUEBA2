library(shiny)
install.packages("shiny")
ui <- fluidPage(
  textInput("nombre", "Introduce tu nombre:"),
  textOutput("pregunta")
)


server <- function(input, output){
  output$pregunta <- renderText({
    paste("Hola, ", input$nombre)
  })
}


shinyApp(ui = ui, server = server)
