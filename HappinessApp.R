library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(shinyWidgets)
options(OutDec = ',')

# Carga de datos ----------------------------------------------------------

directorio = ''
setwd(directorio)

df <-  read.csv('2019.csv', header = TRUE)

str(df)

col_sel <- names(df)[-(1:2)] #nos quedamos con los nombres de todas las columnas numericas

#g <- plot_ly(df,
            #x=df[['GDP.per.capita']],
            #y=df[['Score']],
            #type='scatter',
            #mode='markers',
            #text=~Country.or.region)
#g

# Definimos ui y server ---------------------------------------------------

ui <- fluidPage(
  titlePanel("Happiness"),
  sidebarLayout(
    sidebarPanel(
     pickerInput('variable_x', 'Selecciona la variable x:', choices=col_sel, selected=col_sel[2]),
     pickerInput('variable_y', 'Selecciona la variable y:', choices=col_sel, selected=col_sel[1])
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Grafico', plotlyOutput('dispersion')),
        tabPanel('Tabla', DTOutput('tabla'))
      )
     )
    )
  )

server <- function(input, output){
  output$dispersion <- renderPlotly(
    plot_ly(df,
            x=df[[input$variable_x]],
            y=df[[input$variable_y]],
            type='scatter',
            mode='markers',
            text=~Country.or.region)
  )
  
  output$tabla <- renderDT({
    df
  })
}


shinyApp(ui = ui, server = server)
