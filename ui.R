## Meteoland service UI
## Design and development by Miquel de Cáceres, Antoine Cabon and Víctor Granda

# libraries
library(shiny)
library(leaflet)
library(dygraphs)
library(shinythemes)

# Navbar page layout. This is intended to offer a quick glance of the app, the
# package, examples and manuals, and finally, an app to use the package with
# data from Catalonia

navbarPage(
  title = 'meteoland R package',
  id = 'navbar_meteoland',
  theme = shinytheme('simplex'),
  
  # About tab (tabwith r package description, disclaimer and so on...)
  tabPanel(
    title = 'About', icon = icon('info-circle'),
    
    # RMardown document
    includeMarkdown('Docs/Disclaimer.Rmd')
  ),
  
  # meteoland R package manual tab
  tabPanel(
    title = 'User Guide', icon = icon('book'),
    
    # Html document (generated from an Rmd)
    includeMarkdown('Docs/User_Guide.Rmd')
  ),
  
  # meteoland R package examples
  tabPanel(
    title = 'Examples', icon = icon('flask'),
    
    ## TO DO add some examples of using meteoland with real data. Probably
    ## an html document generated from an Rmd.
    includeMarkdown('Docs/Examples.Rmd')
  ),
  
  tabPanel(
    title = 'Shiny App', icon = icon('television'),
    
    # in order to obtain two tabs (map and data) we need to nest a tabsetPanel here
    tabsetPanel(
      type = 'pills', id = 'shiny_tabs',
      
      # app guide tab
      tabPanel(
        title = 'App user guide', icon = icon('book'),
        
        # alittle space
        br(),
        
        # a nice document explaining how to use the app
        includeMarkdown('Docs/app_user_guide.Rmd')
      ),
      
      # user input tab
      tabPanel(
        title = 'User input', icon = icon('keyboard-o'),
        
        # a little space
        br(),
        
        sidebarLayout(
          sidebarPanel(
            # sidebar width
            width = 3,
            
            # panel for fixed inputs (mode and point/grid)
            wellPanel(
              # Mode selector
              radioButtons(
                inputId = 'mode_sel',
                label = 'Please select the desired mode:',
                choices = c('Historical', 'Current', 'Projection'),
                inline = TRUE, selected = 'Historical'
              ),
              
              # point/grid selector
              radioButtons(
                inputId = 'point_grid_sel',
                label = 'Points (up to 10) or Grid?',
                choices = c('Points', 'Grid'),
                inline = TRUE, selected = 'Points'
              )
            ),
            
            # Dinamic ui to show inputs and buttons depending on the mode selected
            wellPanel(
              uiOutput(
                outputId = 'dinamic_inputs'
              )
            ),
            
            # latitude and longitude selector. To be able to show both in the same
            # line we must to rely in some html/css magic ;)
            div(style = "display: inline-block;vertical-align:top; width: 135px;",
                numericInput(
                  'latitude',
                  label = 'Latitude',
                  value = NA)),
            
            div(style = "display: inline-block;vertical-align:top; width: 135px;",
                numericInput(
                  'longitude',
                  label = 'Longitude',
                  value = NA)),
            
            # conditional panel to show in case of grid. In this case we need
            # two different sets of coordinates, the upper left and the bottom
            # right coordinates of the boundary box desired by the user
            conditionalPanel(
              condition = "input.point_grid_sel == 'Grid'",
              
              div(style = "display: inline-block;vertical-align:top; width: 135px;",
                  numericInput(
                    'latitude_bottom',
                    label = 'Latitude bottom right',
                    value = NA)),
              
              div(style = "display: inline-block;vertical-align:top; width: 135px;",
                  numericInput(
                    'longitude_bottom',
                    label = 'Longitude bottom right',
                    value = NA)),
              
              p("Grid mode selected."),
              p("Please provide the upper right coordinates and the bottom right coordinates of the desired grid.")
            ),
            
            # selected coordinates output, we need a fluid row to put inline
            # the selected coordinates and the clear button. All of this is in
            # a conditional panel to show only if points are selected
            
            conditionalPanel(
              condition = "input.point_grid_sel == 'Points'",
              
              # Append coordinates button
              actionButton(
                inputId = 'append_coord_button',
                label = 'Append coords',
                icon = icon('bullseye')
              ),
              
              # a little space and a header
              br(), br(),
              h5('Selected points:'),
              
              fluidRow(
                
                # coord column
                column(
                  width = 6,
                  br(),
                  tableOutput('user_coords')
                ),
                
                # reset button column
                column(
                  width = 6,
                  br(), br(),
                  actionButton(
                    inputId = 'reset_coord_button',
                    label = 'Reset coords',
                    icon = icon('eraser')
                  )
                )
              )
              
              # debug
              # textOutput('clicked'),
              # textOutput('lat_debug'),
              # textOutput('long_debug')
              # textOutput('dates_debug')
              # ,textOutput('interpolated_df_debug')
            ),
            
            # a little space
            br(), br(),
            
            # Action button to activate the process
            actionButton(
              inputId = 'process_button',
              label = 'Go!',
              icon = icon('play')
            )
          ),
          
          mainPanel(
            # main panel width
            width = 9,
            
            # map output
            leafletOutput('map', height = 600)
          )
        )
      ),
      
      # data download tab
      tabPanel(
        title = 'Data output', icon = icon('area-chart'),
        
        # a little space
        br(),
        
        # fluid row to show the panels in one column and the coord selector and
        # download button in other column
        fluidRow(
          
          # panels column
          column(
            width = 9,
            
            # conditional panel in case of points (with points we show the
            # dygraphs for temp, hum and prec)
            conditionalPanel(
              condition = "input.point_grid_sel == 'Points'",
              
              # dygraphs
              ## temperature
              wellPanel(
                h4('Temperature'),
                dygraphOutput('temperature', height = '350px')
              ),
              ## humidity
              wellPanel(
                h4('Relative Humidity'),
                dygraphOutput('humidity', height = '350px')
              ),
              ## prec & PET
              wellPanel(
                h4('Precipitation & PET'),
                dygraphOutput('prec_and_pet', height = '350px')
              )
            ),
            
            # conditional panel in case of grid (with grid we show the grid
            # for the selected day and variable)
            conditionalPanel(
              condition = "input.point_grid_sel == 'Grid'",
              
              # spplot output
              wellPanel(
                h4('Grid Plot'),
                plotOutput('grid_plot', height = '400px')
              )
            )
            
          ),
          
          # button and selector column
          column(
            width = 3,
            # download button
            downloadButton(
              'download_btn',
              'Download',
              icon = icon('download')
            ),
            
            # a little space
            br(), br(),
            
            # Conditional panel for points (with points we show the
            # radioButtons to select the coordinates to previsualize, and the 
            # topo info)
            conditionalPanel(
              condition = "input.point_grid_sel == 'Points'",
              
              # coordinate pair selector code
              radioButtons(
                inputId = 'coord_vis',
                label = 'Select a coordinate pair to previsualize the data',
                choices = character(0), # empty until user select coordinates
                inline = TRUE,
                selected = character(0)
              ),
              
              # topography info
              h4('Selected point topographic info:'),
              
              htmlOutput('topo_info')
            ),
            
            # conditional panel for grid (with grid we show a variable selector
            # and a date selector)
            conditionalPanel(
              condition = "input.point_grid_sel == 'Grid'",
              
              # variable input
              selectInput(
                inputId = 'grid_var_sel',
                label = 'Select a variable to visualize',
                choices = character(0) # empty until grid is processed
              ),
              
              # date input (empty until grid is processed)
              dateInput(
                inputId = 'grid_date_sel',
                label = 'Select a date to visualize'
              )
            )
          )
        )
      )
    )
  )
)
