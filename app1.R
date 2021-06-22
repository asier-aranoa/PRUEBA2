library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)

options(scipen=999)#para que no salgan numeros con formato cientifico

# Carga de datos ----------------------------------------------------------
directorio <- ''
setwd(directorio)

df <- read.csv('Global_Mobility_Report.csv',header = TRUE)

str(df)
df$date <- ymd(df$date)

table(is.na(df$sub_region_1))
table(is.na(df$sub_region_2))

df <- select(df,-sub_region_2)

prueba <- filter(df, sub_region_1 =="")


# Definimos ui y server ---------------------------------------------------

ui <- fluidPage(
  selectInput(inputId = 'country', label="Selecciona un país:", choices = c('Spain', 'Italy', 'Germany')), 
  plotOutput(outputId = 'grafico_evolucion')
)

server <- function(input,output){
  
  
  
  output$grafico_evolucion <- renderPlot({
    df2 <- filter(df, country_region == input$country & sub_region_1 =="")
    ggplot(df2, aes(date,retail_and_recreation_percent_change_from_baseline)) +
      geom_line(color = "royalblue")+
      theme_bw()+
      theme(panel.grid.major = element_line(color = 'black', linetype = 'dotted'),
            axis.text.x = element_text(face = "bold", size=12),
            axis.text.y = element_text(face = "bold", size=12),
            axis.title.x = element_blank())
    
    
  })
}
  
  shinyApp(ui = ui, server = server)
