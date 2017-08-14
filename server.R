## Meteoland service server

# libraries
library(shiny)
library(leaflet)
library(sp)
library(htmltools)
library(dygraphs)
library(xts)
library(ncdf4)
# library(mapview)
library(rgeos)

# load needed data and functions
load('Data/stations_data.RData')
# load('Data/calibrations.RData')
# load('Data/grid_as_points_topography.RData')
source('global.R')

# server logic
function(input, output, session) {
  
  ## garbage collector to free memory at 1000 
  observe({
    invalidateLater(1000, session)
    for (i in 1:10) {gc()}
  })
  
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
        start = NA, end = NA,
        min = '1976-01-01', max = '2016-12-31',
        startview = 'decade'
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
        max = Sys.Date() - 1,
        # min limit to select the date for current mode is the start of the
        # current year:
        min = as.Date(paste0(format(Sys.Date(), '%Y'), '-01', '-01'))
      )
      
      # update tag list
      inputTagList <- tagAppendChild(inputTagList, date_range_current)
    }

    # projection mode inputs
    if (input$mode_sel == 'Projection') {
      
      label_proj <- p('Select the future scenario:')

      # climate scenario selector. We divide it in two parts, the regional
      # climatic model and the representative concentration pathway
      rcm <- div(
        style = "display: inline-block;vertical-align:top; width: 130px;",
        selectInput(
          'rcm',
          label = 'Reg. Clim. Model',
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
          label = 'Rep. Conc. Pathway',
          choices = c(
            '',
            'rcp4.5',
            'rcp8.5'
          )
        )
      )

      # update tag list
      inputTagList <- tagAppendChild(inputTagList, label_proj)
      inputTagList <- tagAppendChild(inputTagList, rcm)
      inputTagList <- tagAppendChild(inputTagList, rcp)
    }

    # Return updated list of inputs
    inputTagList
  })
  
  #### map output ####
  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(lng1 = -0.02, lat1 = 43,
                lng2 = 3.68, lat2 = 40) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'Imagery') %>%
      addProviderTiles(providers$OpenStreetMap,
                       group = 'OSM') %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addCircleMarkers(data = user_coords$df,
                       radius = 3, color = 'red',
                       label = 'User coordinates',
                       group = 'User') %>%
      addCircleMarkers(data = stations_data,
                       radius = 3, color = 'yellow',
                       label = ~htmlEscape(paste(st_network, St_Id,
                                                 sep = ' - ')),
                       group = 'Stations') %>%
      addLayersControl(
        baseGroups = c('Imagery', 'OSM', 'Toner Lite'),
        overlayGroups = c('User', 'Stations'),
        position = 'bottomright',
        options = layersControlOptions(collapse = FALSE)
      ) %>%
      hideGroup("Stations") #%>%
      # addMouseCoordinates(style = 'basic')
  })
  
  #### interpolated_data() ####
  # logic for the process button
  interpolated_data <- eventReactive(
    eventExpr = input$process_button,
    valueExpr = {
      # progress object
      # progress bar logic
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Processing data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      updateProgress <- function(value = NULL, detail = NULL,
                                 n_coords = NULL, max_val = progress$getMax()) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + ((max_val - value) / n_coords)
        }
        
        progress$set(value = value, detail = detail)
      }
      
      # current points method
      if (input$mode_sel == 'Current' & input$point_grid_sel == 'Points') {
        
        # requirements to met for this mode (i.e. inputs needed)
        req(user_coords$df[1,1], user_coords$df[1,2],
            input$date_range_current[1], input$date_range_current[1])
        
        # interpolated data
        interpolated_data <- current_points_mode_process(
          user_df = user_coords$df,
          user_dates = input$date_range_current,
          updateProgress = updateProgress
        )
      }
      
      # historical points mode
      if (input$mode_sel == 'Historical' & input$point_grid_sel == 'Points') {
        
        # requirements to met for this mode (i.e. inputs needed)
        req(user_coords$df[1,1], user_coords$df[1,2],
            input$date_range_historical[1], input$date_range_historical[2])
        
        interpolated_data <- historical_points_mode_process(
          user_df = user_coords$df,
          user_dates = input$date_range_historical,
          updateProgress = updateProgress
        )
      }
      
      # projection points mode
      if (input$mode_sel == 'Projection' & input$point_grid_sel == 'Points') {
        
        # requirements to met for this mode (i.e. inputs needed)
        req(user_coords$df[1,1], user_coords$df[1,2],
            input$rcm, input$rcp)
        
        interpolated_data <- projection_points_mode_process(
          user_df = user_coords$df,
          rcm = input$rcm,
          rcp = input$rcp,
          updateProgress = updateProgress
        )
      }
      
      # current grid mode
      if (input$mode_sel == 'Current' & input$point_grid_sel == 'Grid') {
        
        # requirements to met for this mode (i.e. inputs needed)
        req(input$longitude, input$longitude_bottom, input$latitude,
            input$latitude_bottom, input$date_range_current)
        
        interpolated_data <- current_grid_mode_process(
          user_coords = data.frame(
            x = c(input$longitude, input$longitude_bottom),
            y = c(input$latitude, input$latitude_bottom)
          ),
          user_dates = input$date_range_current,
          updateProgress = updateProgress
        )
      }
      
      # projection grid mode
      if (input$mode_sel == 'Projection' & input$point_grid_sel == 'Grid') {
        
        # requirements to met for this mode (i.e. inputs needed)
        req(input$longitude, input$longitude_bottom, input$latitude,
            input$latitude_bottom, input$rcm, input$rcp)
        
        interpolated_data <- projection_grid_mode_process(
          user_coords = data.frame(
            x = c(input$longitude, input$longitude_bottom),
            y = c(input$latitude, input$latitude_bottom)
          ),
          rcm = input$rcm,
          rcp = input$rcp,
          updateProgress
        )
      }
      
      # historical grid mode
      if (input$mode_sel == 'Historical' & input$point_grid_sel == 'Grid') {
        
        # requirements to met for this mode (i.e. inputs needed)
        req(input$longitude, input$longitude_bottom, input$latitude,
            input$latitude_bottom,
            input$date_range_historical[1], input$date_range_historical[2])
        
        interpolated_data <- historical_grid_mode_process(
          user_coords = data.frame(
            x = c(input$longitude, input$longitude_bottom),
            y = c(input$latitude, input$latitude_bottom)
          ),
          user_dates = input$date_range_historical,
          updateProgress
        )
      }
      
      # return the interpolated data
      return(interpolated_data)
    }
  )
  
  # dygraph outputs
  # coord_pair selector update
  observe({
    # input to trigger the update
    if (input$process_button > 0) {
      
      # In case of points, we update the radioButtons input for coordinate
      # selection
      if (input$point_grid_sel == 'Points') {
        
        # requirements to met for this mode (i.e. inputs needed)
        req(user_coords$df)
        
        # update the selector. We use isolate to avoid the crash of the app if the
        # user go back and press the reset button
        isolate({
          
          # change the active tab to the output tab
          updateTabsetPanel(
            session,
            inputId = 'shiny_tabs',
            selected = 'Data output'
          )
          
          updateRadioButtons(
            session,
            inputId = 'coord_vis',
            label = 'Select a coordinate pair to previsualize the data',
            choiceNames = paste(round(user_coords$df$lat, 2),
                                round(user_coords$df$lng, 2), sep = ' / '),
            choiceValues = row.names(user_coords$df)
          )
        })
      }
      
      if (input$point_grid_sel == 'Grid') {
        # in case of grid we have to update the selectors for date and variable
        # depending also on the projection/historical/current mode
        
        # current
        if (input$mode_sel == 'Current') {
          isolate({
            
            # change the active tab to the output tab
            updateTabsetPanel(
              session,
              inputId = 'shiny_tabs',
              selected = 'Data output'
            )
            
            # variable names in the processed data
            grid_var_names <- names(interpolated_data()@data[[1]][-1])
            grid_dates <- as.character(interpolated_data()@dates)
            
            # update the var selector
            updateSelectInput(
              session,
              inputId = 'grid_var_sel',
              label = 'Select a variable to visualize',
              choices = grid_var_names
            )
            
            # update the date selector
            updateDateInput(
              session,
              inputId = 'grid_date_sel',
              value = grid_dates[1],
              min = grid_dates[1], max = tail(grid_dates, 1)
            )
          })
        }
        
        # projection
        if (input$mode_sel == 'Projection' && !is.character(interpolated_data())) {
          isolate({
            
            # change the active tab to the output tab
            updateTabsetPanel(
              session,
              inputId = 'shiny_tabs',
              selected = 'Data output'
            )
            
            # get the variables names
            grid_var_names <- names(interpolated_data()$res_list)
            
            # update the var selector
            updateSelectInput(
              session,
              inputId = 'grid_var_sel_proj',
              label = 'Select a variable to visualize',
              choices = grid_var_names
            )
            
            # update the date selector
            updateSelectizeInput(
              session,
              inputId = 'grid_date_sel_proj',
              label = 'Select a date to visualize',
              choices = seq(as.Date('2006-01-01'), as.Date('2100-12-01'),
                            by = 'month')
            )
          })
        }
        
        # historical
        if (input$mode_sel == 'Historical' && !is.character(interpolated_data())) {
          isolate({
            # change the active tab to the output tab
            updateTabsetPanel(
              session,
              inputId = 'shiny_tabs',
              selected = 'Data output'
            )
            
            # get the variables names
            grid_var_names <- names(interpolated_data()$res_list)
            
            # update the var selector
            updateSelectInput(
              session,
              inputId = 'grid_var_sel_hist',
              label = 'Select a variable to visualize',
              choices = grid_var_names
            )
            
            # update the date selector
            updateDateInput(
              session,
              inputId = 'grid_date_sel_hist',
              value = input$date_range_historical[1],
              min = input$date_range_historical[1],
              max = input$date_range_historical[2]
            )
          })
        }
      }
    }
  })
  
  #### Download button logic ####
  output$download_btn <- downloadHandler(
    filename = filename_function(input, interpolated_data()),
    content = function(file) {
      content_function(input, interpolated_data(), file)
    }
  )
  
  
  # temperature panel
  output$temperature <- renderDygraph({
    # get the data
    interpolated_df <- interpolated_data()@data[[as.numeric(input$coord_vis)]]
    interpolated_df$Date <- interpolated_data()@dates
    
    # plot the data
    dygraph(as.xts(interpolated_df[,c('MeanTemperature', 'MaxTemperature', 'MinTemperature')],
                   order.by = interpolated_df$Date), group = 'results') %>%
      dyOptions(colors = c('#26A65B', '#F22613', '#1F3A93')) %>%
      dyLegend(show = 'onmouseover', hideOnMouseOut = TRUE, width = 600) %>%
      dyAxis("y", label = "Temperature [°C]")
  })
  
  # humidity panel
  output$humidity <- renderDygraph({
    # get the data
    interpolated_df <- interpolated_data()@data[[as.numeric(input$coord_vis)]]
    interpolated_df$Date <- interpolated_data()@dates
    
    # plot the data
    dygraph(as.xts(interpolated_df[,c('MeanRelativeHumidity', 'MaxRelativeHumidity', 'MinRelativeHumidity')],
                   order.by = interpolated_df$Date), group = 'results') %>%
      dyOptions(colors = c('#26A65B', '#F22613', '#1F3A93')) %>%
      dyLegend(show = 'onmouseover', hideOnMouseOut = TRUE, width = 700) %>%
      dyAxis("y", label = "Rel. Humidity [%]")
  })
  
  # precipitation and PET panel
  output$prec_and_pet <- renderDygraph({
    # get the data
    interpolated_df <- interpolated_data()@data[[as.numeric(input$coord_vis)]]
    interpolated_df$Date <- interpolated_data()@dates
    
    # plot the data
    dygraph(as.xts(interpolated_df[,c('PET', 'Precipitation')],
                   order.by = interpolated_df$Date), group = 'results') %>%
      dyOptions(colors = c('#F22613', '#1F3A93')) %>%
      dyLegend(show = 'onmouseover', hideOnMouseOut = TRUE, width = 400) %>%
      dySeries("Precipitation", axis = 'y2') %>%
      dyAxis("y2", label = "Precipitation [mm]") %>%
      dyAxis("y", label = "PET [??]")
  })
  
  # topography info
  output$topo_info <- renderUI({
    
    # get the topo
    topography <- getTopographyObject(user_coords$df)@data[as.numeric(input$coord_vis),]
    
    # get the text
    topo_text <- list(paste0('Elevation = ',
                             round(topography$elevation, 2), ' m.'),
                      paste0('Slope = ',
                             round(topography$slope, 2), ' degrees.'),
                      paste0('Aspect = ',
                             round(topography$aspect, 2), ' degrees from North.'))
    
    # return the texts
    lapply(topo_text, tags$p)
  })
  
  # output for grid and current mode
  output$grid_plot <- renderPlot({
    
    # plot
    spplot(interpolated_data(),
           which(interpolated_data()@dates == as.character(input$grid_date_sel)), # date
           input$grid_var_sel) # variable
  })
  
  # output for grid and projection
  output$grid_plot_proj <- renderPlot({
    
    # if the grid is too large interpolated data is character and no grid plot
    # is draw
    if (is.character(interpolated_data())) {
      return()
    }
    
    date_index <- which(
      seq(as.Date('2006-01-01'), as.Date('2100-12-01'), by = 'month') == input$grid_date_sel_proj
    )
    
    # get the variable values array
    var_values <- interpolated_data()$res_list[[input$grid_var_sel_proj]][,,date_index]
    
    # create the data frame, but be careful, we need to invert the order in which
    # the y coordinate is filled
    data_df <- data.frame(
      var = as.numeric(var_values[,ncol(var_values):1])
    )
    names(data_df) <- input$grid_var_sel_proj
    
    data_list <- list(one = data_df)
    names(data_list) <- input$grid_date_sel_proj
    
    grid_sel <- points2grid(interpolated_data()$points_sel)
    
    grid_meteo <- SpatialGridMeteorology(
      grid_sel,
      data = data_list,
      dates = as.Date(input$grid_date_sel_proj)
    )
    
    spplot(grid_meteo, input$grid_date_sel_proj, input$grid_var_sel_proj)
  })
  
  # output for grid historical
  output$grid_plot_hist <- renderPlot({
    
    # if the grid is too large or more than 5 years
    # interpolated data is character and no grid plot is draw
    if (is.character(interpolated_data())) {
      return()
    }
    
    date_index <- which(
      seq(as.Date(input$date_range_historical[1]),
          as.Date(input$date_range_historical[2]),
          by = 'day') == input$grid_date_sel_hist
    )
    
    # get the variable values array
    var_values <- interpolated_data()$res_list[[input$grid_var_sel_hist]][,,date_index]
    
    # create the data frame, but be careful, we need to invert the order in which
    # the y coordinate is filled
    data_df <- data.frame(
      var = as.numeric(var_values[,ncol(var_values):1])
    )
    names(data_df) <- input$grid_var_sel_hist
    
    data_list <- list(one = data_df)
    names(data_list) <- input$grid_date_sel_hist
    
    grid_sel <- points2grid(interpolated_data()$points_sel)
    
    grid_meteo <- SpatialGridMeteorology(
      grid_sel,
      data = data_list,
      dates = as.Date(input$grid_date_sel_hist)
    )
    
    spplot(grid_meteo, 1, 1)
  })
  
  # observe event to record the map clicks and append the coordinates clicked
  # to a data frame of coordinates
  observeEvent(
    eventExpr = input$map_click,
    handlerExpr = {
      
      # points mode
      if (input$point_grid_sel == 'Points') {
        # collect only the coordinates on mouse click
        coord_clicked <- as.data.frame(input$map_click)[,1:2]
        # we need to limit the coord list to 10:
        if (length(user_coords$df[,1]) < 10) {
          user_coords$df <<- rbind(user_coords$df, coord_clicked)
        }
      }
      
      # grid mode
      if (input$point_grid_sel == 'Grid') {
        # collect only the coordinates on mouse click
        coord_clicked <- as.data.frame(input$map_click)[,1:2]
        user_coords$df <<- rbind(user_coords$df, coord_clicked)
        # now we update the inputs for latitude and longitude (upper and
        # bottom)
        if (length(user_coords$df[,1]) < 3) {
          # update Latitude
          updateNumericInput(
            session,
            inputId = 'latitude',
            label = 'Latitude',
            value = round(user_coords$df$lat[1], 4)
          )
          
          # update Longitude
          updateNumericInput(
            session,
            inputId = 'longitude',
            label = 'Longitude',
            value = round(user_coords$df$lng[1], 4)
          )
          
          # update Latitude bottom
          updateNumericInput(
            session,
            inputId = 'latitude_bottom',
            label = 'Latitude bottom right',
            value = round(user_coords$df$lat[2], 4)
          )
          
          # update Longitude bottom
          updateNumericInput(
            session,
            inputId = 'longitude_bottom',
            label = 'Longitude bottom right',
            value = round(user_coords$df$lng[2], 4)
          )
        }
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
  
  # output to show the user selected coordinates in the user input tab
  output$user_coords <- renderTable(
    expr = {user_coords$df},
    digits = 4
  )
  
  # logic for the reset button
  observeEvent(
    eventExpr = input$reset_coord_button,
    handlerExpr = {
      user_coords$df <<- data.frame(
        lat = numeric(0),
        lng =  numeric(0)
      )
    }
  )
  
  # observeEvent for the modal dialogs about grid sizes and data ranges not
  # allowed
  observeEvent(
    eventExpr = {
      interpolated_data()
    },
    
    handlerExpr = {
      
      # if there is an error due to grid too large or time span too long,
      # show the modal
      if (is.character(interpolated_data())) {
        
        # grid too large in projection
        if (interpolated_data() == 'proj_grid_too_large') {
          showModal(modalDialog(
            title = "Oooops!",
            p("Grid too large to process (must be less than 2500 km2)."),
            p("Please reload the app and try again with an smaller grid"),
            easyClose = TRUE
          ))
        }
        
        # grid too large in historical
        if (interpolated_data() == 'hist_grid_too_large') {
          showModal(modalDialog(
            title = "Oooops!",
            p("Grid too large to process (must be less than 5000 km2)."),
            p("Please reload the app and try again with an smaller grid"),
            easyClose = TRUE
          ))
        }
        
        # time span too long in historical grid
        if (interpolated_data() == 'hist_time_span_too_large') {
          showModal(modalDialog(
            title = "Oooops!",
            p("Time span limit is 5 years"),
            p("Please reload the app and try again limiting the time span"),
            easyClose = TRUE
          ))
        }
        
      }
    }
  )
  
  #### modal for inputs missing ####
  # observer for modal dialogs when some input is missing
  observe({
    if (input$mode_sel == 'Current' & input$point_grid_sel == 'Points' & input$process_button > 0) {
      
      if (any(!isTruthy(user_coords$df[1,1]), !isTruthy(user_coords$df[1,2]))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Coordinates haven't being selected"),
          p("Please provide at least one pair of coordinates"),
          easyClose = TRUE
        ))
      }
      
      if (any(!isTruthy(input$date_range_current[1]),
              !isTruthy(input$date_range_current[2]))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Starting and ending dates must be provided."),
          p("Please select the desired date range."),
          easyClose = TRUE
        ))
      }
    }
    
    if (input$mode_sel == 'Historical' & input$point_grid_sel == 'Points' & input$process_button > 0) {
      
      if (any(!isTruthy(user_coords$df[1,1]), !isTruthy(user_coords$df[1,2]))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Coordinates haven't being selected"),
          p("Please provide at least one pair of coordinates"),
          easyClose = TRUE
        ))
      }
      
      if (any(!isTruthy(input$date_range_historical[1]),
              !isTruthy(input$date_range_historical[2]))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Starting and ending dates must be provided."),
          p("Please select the desired date range."),
          easyClose = TRUE
        ))
      }
    }
    
    if (input$mode_sel == 'Projection' & input$point_grid_sel == 'Points' & input$process_button > 0) {
      
      if (any(!isTruthy(user_coords$df[1,1]), !isTruthy(user_coords$df[1,2]))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Coordinates haven't being selected"),
          p("Please provide at least one pair of coordinates"),
          easyClose = TRUE
        ))
      }
      
      if (any(!isTruthy(input$rcm), !isTruthy(input$rcp))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Regional Climate Model and Reproducible Concentration Pathway must be provided"),
          p("Please select the desired RCM and RCP"),
          easyClose = TRUE
        ))
      }
    }
    
    if (input$mode_sel == 'Current' & input$point_grid_sel == 'Grid' & input$process_button > 0) {
      
      if (any(!isTruthy(input$longitude), !isTruthy(input$longitude_bottom),
              !isTruthy(input$latitude), !isTruthy(input$latitude_bottom))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Coordinates haven't being selected"),
          p("Please provide upper left and bottom left coordinates"),
          easyClose = TRUE
        ))
      }
      
      if (any(!isTruthy(input$date_range_current[1]),
              !isTruthy(input$date_range_current[2]))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Starting and ending dates must be provided."),
          p("Please select the desired date range."),
          easyClose = TRUE
        ))
      }
    }
    
    if (input$mode_sel == 'Projection' & input$point_grid_sel == 'Grid' & input$process_button > 0) {
      
      if (any(!isTruthy(input$longitude), !isTruthy(input$longitude_bottom),
              !isTruthy(input$latitude), !isTruthy(input$latitude_bottom))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Coordinates haven't being selected"),
          p("Please provide upper left and bottom left coordinates"),
          easyClose = TRUE
        ))
      }
      
      if (any(!isTruthy(input$rcm), !isTruthy(input$rcp))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Regional Climate Model and Reproducible Concentration Pathway must be provided"),
          p("Please select the desired RCM and RCP"),
          easyClose = TRUE
        ))
      }
    }
    
    if (input$mode_sel == 'Historical' & input$point_grid_sel == 'Grid' & input$process_button > 0) {
      
      if (any(!isTruthy(input$longitude), !isTruthy(input$longitude_bottom),
              !isTruthy(input$latitude), !isTruthy(input$latitude_bottom))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Coordinates haven't being selected"),
          p("Please provide upper left and bottom left coordinates"),
          easyClose = TRUE
        ))
      }
      
      if (any(!isTruthy(input$date_range_historical[1]),
              !isTruthy(input$date_range_historical[2]))) {
        showModal(modalDialog(
          title = "Oooops!",
          p("Starting and ending dates must be provided."),
          p("Please select the desired date range."),
          easyClose = TRUE
        ))
      }
    }
  })
  
  
  # debug
  # output$clicked <- renderPrint(as.data.frame(input$map_click))
  # output$lat_debug <- renderPrint(input$latitude)
  # output$long_debug <- renderPrint(input$longitude)
  # output$dates_debug <- renderPrint(input$date_range_current)
  # output$interpolated_df_debug <- renderPrint(
  #   # class(interpolated_data()@data[[input$coord_vis]])
  #   paste(class(interpolated_data()@data),
  #         length(interpolated_data()@data),
  #         names(interpolated_data()@data),
  #         sep = ' - ')
  # )
  # output$debug_date_sel <- renderPrint({
  #   interpolated_df <- interpolated_data()
  #   interpolated_dates <- as.character(interpolated_df@dates)
  #   
  #   which(interpolated_dates == as.character(input$grid_date_sel))
  # })
  # output$debug_date_sel_proj <- renderPrint(input$grid_date_sel_proj)
  
  
}
