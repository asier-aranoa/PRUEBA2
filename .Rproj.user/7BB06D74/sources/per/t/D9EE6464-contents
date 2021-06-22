library(shiny)
library(ggplot2)
library(plotly)
library(DT)#tablas y graficos plotly
library(shinyWidgets)#inputs shiny
library(shinythemes)#themes
library(gridExtra)


# Carga datos -------------------------------------------------------------
directorio_in <- ''
archivo_in <- 'temperatura.csv'

df <- read.csv('temperatura.csv',header = TRUE)
table(df$City)

df$City <-  as.factor(df$City)
# ui & server ----------------------------------------------------------

ui <- fluidPage(
  titlePanel("Temperatura"),
  sidebarLayout(
    sidebarPanel(
      pickerInput('ciudad', 'Ciudad',choices = levels(df$City), selected = 'Bilbao'),
      pickerInput('mes', 'Mes', choices = unique(df$Month), selected = 1 ),
      sliderInput('time_range', 'Selecciona el periodo temporal:',  min=min(df$Year), max=max(df$Year), value=c(2015,2019), step=1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Evolucion', plotOutput('plot_evolucion')),
        tabPanel('Histogramas', plotlyOutput('plot_histogramas')),
        tabPanel('Tabla', DTOutput('table')),
        tabPanel('Boxplot', plotlyOutput('plot_boxplot'))
        
      )
    )
  )
)

server <- function(input, output, session){
  tabla <- reactive({
    filter(df,
           Year >= input$time_range[1]&
           Year <= input$time_range[2]&
           City == input$ciudad &
           Month == input$mes)
  })
  
  output$plot_evolucion <- renderPlot({
    ggplot(tabla(), aes(x=Day, y=AvgTemperature))+
      stat_summary(fun.y = 'mean', geom = 'line', color = 'red')+
      scale_x_continuous(limits=c(1,31), breaks = seq(1,31,1))
  })
  
  output$plot_histogramas <- renderPlotly(
    ggplotly(
      ggplot(tabla(), aes(AvgTemperature))+
        geom_histogram(alpha=0.6, color='black', fill='navyblue')
    )
  )
  
  output$table <- renderDT({
    select(tabla(), c('Day', 'Year', 'AvgTemperature'))
  })
  
  output$plot_boxplot <- renderPlotly(
    plot_ly(tabla(), y=~AvgTemperature, type="box", text=~Day)
  )
}

shinyApp(ui = ui, server = server)


