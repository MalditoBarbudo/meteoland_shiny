## Meteoland service server

# libraries
library(shiny)
library(leaflet)
library(sp)
library(htmltools)

# load needed data

# server logic
function(input, output) {
  
  # First we need the dinamic UI to show the input options corresponding to the
  # selected mode
  
  output$dinamic_inputs <- renderUI({
    
    # check for null inputs (that can freeze the app)
    if (is.null(input$mode_sel)) {
      return()
    }
    
    # Initialize list of inputs. This is necessary because renderUI returns
    # the result of evaluating an expression. If we want to show several inputs
    # we must store them in an inputs list that is then returned by the expression.
    inputTagList <- tagList()
    
    # historical mode inputs
    if (input$mode_sel == 'Historical') {
      
      # data range input
      date_range <- dateRangeInput(
        'date_range',
        label = 'Select the date or date range (yyyy-mm-dd)',
        start = Sys.Date(), end = Sys.Date()
      )
      
      # update tag list
      inputTagList <- tagAppendChild(inputTagList, date_range)
    }
    
    # projection mode inputs
    if (input$mode_sel == 'Projection') {
      
      # climate scenario selector. We divide it in two parts, the regional
      # climatic model and the representative concentration pathway
      rcm <- div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        selectInput(
          'rcm',
          label = 'Regional Climate Model',
          choices = c(
            '',
            'CCLM4-8-17',
            'RCA4'
          )
        )
      )
      
      rcp <- div(
        style = "display: inline-block;vertical-align:top; width: 160px;",
        selectInput(
          'rcp',
          label = 'Representative Concentration Pathway',
          choices = c(
            '',
            'RCP_4.5',
            'RCP_8'
          )
        )
      )
      
      # update tag list
      inputTagList <- tagAppendChild(inputTagList, rcm)
      inputTagList <- tagAppendChild(inputTagList, rcp)
    }
    
    # Return updated list of inputs
    inputTagList
  })
  
  # map output
  
  ## colors for AEMET and SCM stations
  pal <- colorFactor(c("#F22613", "#F7CA18"), domain = c("SMC", "AEMET"))
  
  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -0.02, lat1 = 43,
                lng2 = 3.68, lat2 = 40) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      # addMarkers(data = foo, icon = ~icon_set[st_network])
      addCircleMarkers(data = foo,
                       radius = 3, color = ~pal(st_network),
                       label = ~htmlEscape(paste(st_network, St_Id,
                                                 sep = ' - ')))
  })
}
