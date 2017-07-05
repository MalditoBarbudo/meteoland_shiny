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
    
    # Initialize list of inputs
    inputTagList <- tagList()
    
    # Latitude and longitude. To be able to show both in the same line we must
    # to rely in some html/css magic ;)
    lat <- div(style = "display: inline-block;vertical-align:top; width: 150px;",
               numericInput(
                 'latitude',
                 label = 'Latitude',
                 value = NA))
    
    lon <- div(style = "display: inline-block;vertical-align:top; width: 150px;",
               numericInput(
                 'longitude',
                 label = 'Longitude',
                 value = NA))
    
    # update tag list
    inputTagList <- tagAppendChild(inputTagList, lat)
    inputTagList <- tagAppendChild(inputTagList, lon)
    
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
      
      # climate scenario selector
      clim_scen <- selectInput(
        'climate_scenario',
        'Select the desired climate scenario',
        choices = c(
          'CCLM4-8-17_rcp4.5',
          'CCLM4-8-17_rcp8.5',
          'RCA4_rcp4.5',
          'RCA4_rcp8.5'
        ),
        selected = 'CCLM4-8-17_rcp4.5'
      )
      
      # update tag list
      inputTagList <- tagAppendChild(inputTagList, clim_scen)
    }
    
    # Return updated list of inputs
    inputTagList
  })
  
  # map output
  # output$map <- renderLeaflet({
  #   
  # })
}
