## Meteoland service server

# libraries
library(shiny)
library(leaflet)
library(sp)
library(htmltools)
library(dygraphs)
library(xts)
library(ncdf4)

# load needed data and functions
load('Data/stations_data.RData')
source('global.R')

# server logic
function(input, output, session) {
  
  # objects needed to reactiveEvents and so on
  
  # 1. empty coordinates data frame, to be able to add clicks in the map and
  #    manual inputs in the case of more than one coordinate pair
  user_coords <- reactiveValues()
  
  user_coords$df <- data.frame(
    lat = numeric(0),
    lng =  numeric(0)
  )
  
  # First we need the dinamic UI to show the input options corresponding to the
  # selected mode
  
  output$dinamic_inputs <- renderUI({

    # check for null inputs (that can freeze the app)
    if (is.null(input$mode_sel)) {
      return()
    }

    # Initialize list of inputs. This is necessary because renderUI returns
    # the result of evaluating an expression. If we want to show several inputs
    # we must store them in an input list that is then returned by the expression.
    inputTagList <- tagList()

    # historical mode inputs
    if (input$mode_sel == 'Historical') {

      # data range input
      date_range_historical <- dateRangeInput(
        'date_range_historical',
        label = 'Select the date or date range',
        start = NA, end = NA
      )

      # update tag list
      inputTagList <- tagAppendChild(inputTagList, date_range_historical)
    }
    
    # current mode inputs
    if (input$mode_sel ==  'Current') {
      
      # data range input (limited to the current year)
      date_range_current <- dateRangeInput(
        'date_range_current',
        label = 'Select the date or date range (only current year)',
        start = NA, end = NA,
        max = Sys.Date(),
        # min limit to select the date for current mode is the start of the
        # current year:
        min = as.Date(paste0(format(Sys.Date(), '%Y'), '-01', '-01'))
      )
      
      # update tag list
      inputTagList <- tagAppendChild(inputTagList, date_range_current)
    }

    # projection mode inputs
    if (input$mode_sel == 'Projection') {

      # climate scenario selector. We divide it in two parts, the regional
      # climatic model and the representative concentration pathway
      rcm <- div(
        style = "display: inline-block;vertical-align:top; width: 130px;",
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
        style = "display: inline-block;vertical-align:top; width: 130px;",
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
  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -0.02, lat1 = 43,
                lng2 = 3.68, lat2 = 40) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') %>%
      addProviderTiles(providers$OpenStreetMap,
                       group = 'OSM') %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addCircleMarkers(data = stations_data,
                       radius = 3, color = 'yellow',
                       label = ~htmlEscape(paste(st_network, St_Id,
                                                 sep = ' - ')),
                       group = 'Stations') %>%
      addLayersControl(
        baseGroups = c('Imagery', 'OSM', 'Toner Lite'),
        overlayGroups = c('Stations'),
        position = 'bottomright',
        options = layersControlOptions(collapse = FALSE)
      ) %>%
      hideGroup("Stations")
  })
  
  # temperature panel
  output$temperature <- renderDygraph({
    # erase the plot if the reset coordinates button is clicked
    input$reset_coord_button
    
    # get the data
    interpolated_df <- interpolated_data()@data[[1]] #### OJO SOLO ESTOY ENSEÑANDO UN PUNTO, HAY QUE MEJORARLO
    interpolated_df$Date <- interpolated_data()@dates
    
    # plot the data
    dygraph(as.xts(interpolated_df[,c('MeanTemperature', 'MaxTemperature', 'MinTemperature')],
                   order.by = interpolated_df$Date))
  })
  
  # humidity panel
  output$humidity <- renderDygraph({
    # erase the plot if the reset coordinates button is clicked
    input$reset_coord_button
    
    # get the data
    interpolated_df <- interpolated_data()@data[[1]] #### OJO SOLO ESTOY ENSEÑANDO UN PUNTO, HAY QUE MEJORARLO
    interpolated_df$Date <- interpolated_data()@dates
    
    # plot the data
    dygraph(as.xts(interpolated_df[,c('MeanRelativeHumidity', 'MaxRelativeHumidity', 'MinRelativeHumidity')],
                   order.by = interpolated_df$Date))
  })
  
  # precipitation and PET panel
  output$prec_and_pet <- renderDygraph({
    # erase the plot if the reset coordinates button is clicked
    input$reset_coord_button
    
    # get the data
    interpolated_df <- interpolated_data()@data[[1]] #### OJO SOLO ESTOY ENSEÑANDO UN PUNTO, HAY QUE MEJORARLO
    interpolated_df$Date <- interpolated_data()@dates
    
    # plot the data
    dygraph(as.xts(interpolated_df[,c('PET', 'Precipitation')],
                   order.by = interpolated_df$Date))
  })
  
  ##### Current mode coordinates selection #####
  
  # observe event to record the map clicks and append the coordinates clicked
  # to a data frame of coordinates
  observeEvent(
    eventExpr = input$map_click,
    handlerExpr = {
      # collect only the coordinates on mouse click
      coord_clicked <- as.data.frame(input$map_click)[,1:2]
      # we need to limit the coord list to 10:
      if (length(user_coords$df[,1]) < 10) {
        user_coords$df <<- rbind(user_coords$df, coord_clicked)
      }
    }
  )
  
  # observe event to record coordinates manually introduced
  observeEvent(
    eventExpr = input$append_coord_button,
    handlerExpr = {
      if (!is.na(input$latitude) & !is.na(input$longitude)) {
        if (length(user_coords$df[,1]) < 10) {
          coords_manual <- data.frame(lat = input$latitude,
                                      lng = input$longitude)
          user_coords$df <<- rbind(user_coords$df, coords_manual)
        }
      }
    }
  )
  
  # output to show the user selected coordinates
  output$user_coords <- renderTable(
    expr = {user_coords$df},
    digits = 4
  )
  
  # logic for the reset button
  observeEvent(
    eventExpr = input$reset_coord_button,
    handlerExpr = {
      user_coords$df <<- NULL
    }
  )
  
  # logic for the process button
  interpolated_data <- eventReactive(
    eventExpr = input$process_button,
    valueExpr = {
      # current points method
      if (input$mode_sel == 'Current' & input$point_grid_sel == 'Points') {
        interpolated_data <- current_points_mode_process(
          user_df = user_coords$df,
          user_dates = input$date_range_current
        )
      }
      
      # return the interpolated data
      return(interpolated_data)
    }
  )
  
  # debug
  # output$clicked <- renderPrint(as.data.frame(input$map_click))
  # output$lat_debug <- renderPrint(input$latitude)
  # output$long_debug <- renderPrint(input$longitude)
  # output$dates_debug <- renderPrint(input$date_range_current)
  
}
