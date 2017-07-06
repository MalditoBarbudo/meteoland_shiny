## Meteoland service server

# libraries
library(shiny)
library(leaflet)

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
      # climatic model and the representative concentration model
      rcg <- div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        selectInput(
          'rcg',
          label = 'RCG',
          choices = c(
            'CCLM4-8-17',
            'RCA4'
          )
        )
      )
      
      rcp <- div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        selectInput(
          'rcp',
          label = 'RCP',
          choices = c(
            'RCP_4.5',
            'RCP_8'
          )
        )
      )
      
      # update tag list
      inputTagList <- tagAppendChild(inputTagList, rcg)
      inputTagList <- tagAppendChild(inputTagList, rcp)
    }
    
    # Return updated list of inputs
    inputTagList
  })
  
  # map output
  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -0.02, lat1 = 43,
                lng2 = 3.68, lat2 = 40) %>%
      addProviderTiles(providers$Esri.WorldImagery)
  })
}
