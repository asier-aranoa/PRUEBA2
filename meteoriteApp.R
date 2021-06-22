library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(shinyWidgets)
library(shinythemes)

options(scipen=999)#para que no salgan numeros con formato cientifico
#https://rstudio.github.io/leaflet/markers.html

# Carga de datos ----------------------------------------------------------
directorio_in <- 'G:/.shortcut-targets-by-id/17RLyiFCEqM5dibFCKwnx8Yk7Ao1Msqea/GradoBDATA/2020_2021_BDATA1/M331_visualizacion/materia/Reto4/meteorite'
archivo_in <- 'meteorite_coma.csv'

df <- read.csv(file.path(directorio_in,archivo_in), sep=';', dec = ',',header = TRUE)
df <- read.csv('meteorite_coma.csv', sep=';', dec = ',',header = TRUE)
str(df)


# ui and server -----------------------------------------------------------

ui <- fluidPage(
  titlePanel("Meteorite landings"),
  #themeSelector(),
  theme = shinytheme('paper'),
  sidebarLayout(
    sidebarPanel(
      sliderInput('year_range', label='Selecciona el periodo temporal:', min=min(df$year), max=max(df$year), value = c(2010,2012), step=1),
      pickerInput('recclass_select', 'Selecciona recclass:', choices=as.character(unique(df$recclass)), selected=as.character(unique(df$recclass)),options = list('actions-box' = TRUE,'deselectAllText' = FALSE), multiple = TRUE),
      radioButtons('fall_select', 'Selecciona:', choices=unique(df$fall))
    ),
    mainPanel(      
      tabsetPanel(
        tabPanel('Mass',plotly::plotlyOutput('boxplot')),
        tabPanel('Mass Table', DT::DTOutput('tabla_mass')),
        tabPanel('Map', leafletOutput('mapa_mass') )
      )
    )
  )
)

server <- function(input, output, session){
  tabla <- reactive({
    filter(df, 
           year>= input$year_range[1] &
           year<= input$year_range[2] &
           recclass %in% input$recclass_select &
           fall == input$fall_select
          )
  })
  
  output$boxplot <- plotly::renderPlotly(
    plot_ly(tabla(), y=~mass, type='box', text=~id)
  )
  
  output$tabla_mass <- DT::renderDT({
    tabla()
    
  })
  
  output$mapa_mass <- renderLeaflet({
    
  pal <- colorNumeric(
    palette = "YlGnBu",
    domain = tabla()$mass
  )
  
  leaflet(tabla()) %>% 
    addTiles() %>% 
    addCircleMarkers(~reclong, ~reclat, color = ~pal(mass))%>%
    addLegend("bottomright", pal = pal, values = ~mass, title = "mass")
  
})
}


shinyApp(ui = ui, server = server)

