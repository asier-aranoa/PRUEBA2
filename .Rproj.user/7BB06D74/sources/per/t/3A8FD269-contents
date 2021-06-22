library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)#tablas y graficos plotly
library(shinyWidgets)#inputs shiny
library(shinythemes)#themes
library(gridExtra)
library(ggthemes)

# Carga de datos ----------------------------------------------------------

directorio_in <- 'G:/.shortcut-targets-by-id/17RLyiFCEqM5dibFCKwnx8Yk7Ao1Msqea/GradoBDATA/2020_2021_BDATA1/M331_visualizacion/materia/Reto4/ejercicios/olimpiadas/'
archivo_in <- 'olympics.csv'

df <- read.csv('olympics.csv', header = TRUE)

str(df)

df$Sport <- as.factor(df$Sport)#pasamos a factor para luego poder utilizar levels(df$Sport)
df$Team <- as.factor(df$Team)
  



# ui and server -----------------------------------------------------------

ui <- fluidPage(
  titlePanel("Olympic Games"),
  sidebarLayout(
    sidebarPanel(
      sliderInput('year_range', 'Selecciona el periodo temporal:', min=min(df$Year), max=max(df$Year), value=c(2002,2014), step=1),
      pickerInput('team_select', 'Selecciona el team:', choices=levels(df$Team), selected='Norway'),
      pickerInput('sport_select', 'Selecciona sport:', choices=levels(df$Sport), selected=levels(df$Sport), multiple=TRUE, options = list('actions-box' = TRUE))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Evolucion medallas', plotOutput('evolucion')),
        tabPanel('Peso vs altura', plotlyOutput('dispersion')),
        tabPanel('Boxplot deportes', plotlyOutput('boxplot')),
        tabPanel('Tabla', DTOutput('medallas_tabla'))
      )
    )
  )
)

server <- function(input, output, session){
  tabla <- reactive({
    filter(df,
           Year >= input$year_range[1] &
           Year <= input$year_range[2] &
           Team == input$team_select &
           Sport %in% input$sport_select)
  })
  
  
  
  output$evolucion <- renderPlot({
   ggplot(tabla(),aes(x=Year, color = Medal))+
      stat_count(geom='line')+ 
      scale_x_continuous(limits=c(input$year_range[1], input$year_range[2]), breaks=seq(input$year_range[1], input$year_range[2], 1))
    
  })
  
  output$dispersion <- renderPlotly({
    plot_ly(tabla(), y=~Weight, x= ~Height, color=~Medal,  type = "scatter", text=~Name)
      
  })
  
  output$boxplot <- renderPlotly(
    plot_ly(tabla(), y=~Weight, x= ~Sport, color =~Sport, type = "box", text=~Name)
  )
  
  output$medallas_tabla <- renderDT({
    select(tabla(), c('Name', 'Sport', 'Year', 'Medal'))
  })
  
  
}

shinyApp(ui = ui,server = server) 
