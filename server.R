## Meteoland service server

# libraries
library(shiny)
library(leaflet)
library(sp)
library(htmltools)
library(dygraphs)
library(xts)

function(input, output, session) {
  
  output$data <- renderDygraph({
    fake_data <- data.frame(
      Date = seq(Sys.Date() - 4, Sys.Date() + 5, 1),
      Tmax = round(rnorm(10, 30, 2), 2),
      Tmin = round(rnorm(10, 15, 3), 2),
      RH = round(rnorm(10, 65, 10), 2),
      Precev = c(0,0,0,0,0,1,0,1,0,0),
      Precam = c(0,0,0,0,0,225,0,350,0,0)
    )
    
    dygraph(as.xts(fake_data[,input$var_sel],
                   order.by = fake_data$Date))
  })
  
}
