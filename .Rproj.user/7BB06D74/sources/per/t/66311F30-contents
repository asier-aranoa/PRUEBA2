library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)


# Carga de datos ----------------------------------------------------------


df <- read.csv('Global_Mobility_Report.csv',header = TRUE)
df$date <- ymd(df$date)

str(df)

table(is.na(df$sub_region_1))
table(is.na(df$sub_region_2))

df <- select(df, -sub_region_2)
#spain <- filter(df, country_region == 'Spain')

#table(df$country_region)

df


# Definimos ui y server ---------------------------------------------------

ui <- fluidPage(
  titlePanel("Mobility Report"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId ='country', label="Selecciona un pais:", choices=c('Spain','Italy','Germany')),
      dateRangeInput(inputId = 'date_range', label = 'Selecciona el periodo temporal', min=min(df$date, na.rm=TRUE), max=max(df$date, na.rm=TRUE), start='2020-04-01', end='2020-04-30')
    ),
    mainPanel(
      plotOutput(outputId = 'grafico_evolucion'),
      plotOutput(outputId = 'grafico_evolucion2')
    )
  )
)

server <- function(input, output){
  
  tabla <- reactive({filter(df, 
                            country_region == input$country & 
                              sub_region_1 == "" & 
                              date >= input$date_range[1] &
                              date <= input$date_range[2])
  })
  
  output$grafico_evolucion <- renderPlot({
    ggplot(tabla(), aes(date, retail_and_recreation_percent_change_from_baseline))+
      geom_line(color = "royalblue")+
      theme_bw()+
      theme(panel.grid.major = element_line(color = 'black', linetype = 'dotted'),
            axis.text.x = element_text(face= 'bold', size=12),
            axis.text.y = element_text(face= 'bold', size=12),
            axis.title.x = element_blank())
    
  })
  
  output$grafico_evolucion2 <- renderPlot({
    ggplot(tabla(), aes(date, grocery_and_pharmacy_percent_change_from_baseline))+
      geom_line(color = "royalblue")+
      theme_bw()+
      theme(panel.grid.major = element_line(color = 'black', linetype = 'dotted'),
            axis.text.x = element_text(face= 'bold', size=12),
            axis.text.y = element_text(face= 'bold', size=12),
            axis.title.x = element_blank())
  })
  
}

shinyApp(ui = ui, server = server)
