## Meteoland service UI
## Design and development by Miquel de Cáceres, Antoine Cabon and Víctor Granda

# libraries
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
library(shinythemes)

# Navbar page layout. This is intended to offer a quick glance of the app, the
# package, examples and manuals, and finally, an app to use the package with
# data from Catalonia

navbarPage(
  title = 'meteoland R package',
  id = 'navbar_meteoland',
  theme = shinytheme('sandstone'),
  
  #
  # Welcome tab ####
  tabPanel(
    title = 'Welcome', icon = icon('pagelines'),
    
    # RMardown document
    # includeMarkdown('Docs/Welcome.md')
    
    # welcome page in html
    div(
      id = 'welcome',
      style = 'width:100%',
      
      # custom css
      includeCSS("www/custom.css"),
      
      # a little space
      br(),
      
      # logo with link to CRAN
      a(
        id = 'logo',
        href = "https://cran.r-project.org/package=meteoland",
        img(style = 'display:inline-block',
            src = 'package_logo.png', alt = 'meteoland link')
      ),
      
      # subtitle
      h2(id = 'subtitle',
         'Landscape Meteorology Tools'),
      
      # a little space
      br(),
      
      # left div
      column(
        6,
        br(),
        div(
          id = 'leftdiv',
          style = 'display:inline-block;text-align:justify',
          br(),
          p(style = 'margin: 10px 30px 10px 30px; font-size:16px',
            strong('meteoland'),
            'package provides functions to estimate daily weather variables ',
            '(temperature, realtive humidity, precipitation...) over landscapes, by means of ',
            'interpolation and statistical corrections.'),
          br()
        )
      ),
      
      # right div
      div(
          id = 'rightdiv',
          style = 'display:inline-block;width:50%;text-align:justify',
          p(style = 'margin: 10px 30px 10px 30px; font-size:16px',
            'See package documentation at',
            a(href='https://vegmod.github.io/rpackage/meteoland/', 'our website')),
          p(style = 'margin: 10px 30px 10px 30px; font-size:16px',
            'Here an interactive shiny app is provided to illustrate the package interpolation capabilities, ',
            'using Catalonia as an example.'),
          p(style = 'margin: 10px 30px 10px 30px; font-size:16px',
            downloadButton('appguide_dwn', label = 'App. user guide')),
          br()
      )
    ),

    # an space
    br(),br(), br(),
    
    # logos row
    div(
      style = 'text-align:center',
      fluidRow(
        column(
          4,
          a(
            id = 'logo_grupo',
            href = "http://vegmod.ctfc.cat/",
            img(style = 'display:inline-block',
                src = 'LOGO_Group_scaled.png')
          )
        ),
        column(
          4,
          a(
            id = 'logo_ctfc',
            href = "http://www.ctfc.cat/",
            img(style = 'display:inline-block',
                src = 'logo_ctfc_scaled.png')
          )
        ),
        column(
          4,
          a(
            id = 'logo_creaf',
            href = "http://www.creaf.cat/",
            img(style = 'display:inline-block',
                src = 'logo_creaf_scaled.png')
          )
        )
      )
    )
  ),
  
  # meteoland R package manual tab
  # tabPanel(
  #   title = 'User Guide', icon = icon('book'),
  #   
  #   # a little space
  #   br(),
  #   
  #   # a nice document explaining how to use the app
  #   # includeMarkdown('Docs/app_user_guide.Rmd')
  #   tags$iframe(style = "height:600px; width:80%; scrolling=yes", 
  #               src = "_main.pdf#zoom=120")
  # ),
  # 
  # # Doc and Vignettes ####
  # tabPanel(
  #   title = 'Installation & Documentation', icon = icon('book'),
  #   fluidPage(
  #     h2('Package installation'),
  #     br(),
  #     fluidRow(
  #       # RMardown document
  #       column(
  #         8,
  #         includeMarkdown('Docs/Installation.Rmd')
  #       )
  #     )
  #   ),
  #   # a little space
  #   br(),br(),
  #   
  #   fluidPage(
  #     h2('Package vignettes'),
  #     h4('Tutorials explaining the main functions of the package'),
  #     br(),
  # 
  #     fluidRow(
  #       #render(system.file("doc/UserGuide.Rmd", package = "meteoland"), html_document(toc = TRUE), output_dir = "Docs/")
  #       column(
  #         2,
  #         downloadButton('userguide_dwn', label = 'User guide')
  #       ),
  # 
  #       column(
  #         3,
  #         p('User guide to start using package meteoland')
  #       ),
  # 
  #       column(
  #         2, offset = 1,
  #         downloadButton('advancedguide_dwn', label = 'Advanced user guide')
  #       ),
  # 
  #       column(
  #         3,
  #         p('Detailed description of package functions and mathematical',
  #           'calculations of each procedure')
  #       )
  #     ),
  # 
  #     br(), br(), br(),
  # 
  #     fluidRow(
  #       column(
  #         2,
  #         downloadButton('appguide_dwn', label = 'App. user guide')
  #       ),
  # 
  #       column(
  #         3,
  #         p('A short guide to learn to use the shiny app included in this',
  #           'web page')
  #       )
  #     )
  #   ),
  #   # a little space
  #   br(),br(),
  #   fluidPage(
  #     h2('Citation'),
  #     h4('If you use the package or interpolated data from this website, please use the following citation of our work:'),
  #     br(),
  #     fluidRow(
  #       column(
  #         12,
  #         p('De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating daily meteorological data and downscaling
  # climate models over landscapes.',
  #         'Environmental Modelling and Software 108: 186-196.')
  #       )
  #     )
  #   )
  # ),
  # meteoland R package examples (this will be included in the package manual)
  # tabPanel(
  #   title = 'Examples', icon = icon('flask'),
  #   
  #   ## TO DO add some examples of using meteoland with real data. Probably
  #   ## an html document generated from an Rmd.
  #   includeMarkdown('Docs/Examples.Rmd')
  # ),
  
  #
  # Shiny app tab ####
  tabPanel(
    title = 'Shiny App', icon = icon('television'),
    
    # in order to obtain two tabs (map and data) we need to nest a tabsetPanel here
    tabsetPanel(
      type = 'pills', id = 'shiny_tabs',
      
      # user input tab ####
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
            div(style = "display: inline-block;vertical-align:top; width: 145px;",
                numericInput(
                  'latitude',
                  label = 'Latitude',
                  value = NA)),
            
            div(style = "display: inline-block;vertical-align:top; width: 145px;",
                numericInput(
                  'longitude',
                  label = 'Longitude',
                  value = NA)),
            
            # conditional panel to show in case of grid. In this case we need
            # two different sets of coordinates, the upper left and the bottom
            # right coordinates of the boundary box desired by the user
            conditionalPanel(
              condition = "input.point_grid_sel == 'Grid'",
              
              div(style = "display: inline-block;vertical-align:top; width: 145px;",
                  numericInput(
                    'latitude_bottom',
                    label = 'Latitude bottom right',
                    value = NA)),
              
              div(style = "display: inline-block;vertical-align:top; width: 145px;",
                  numericInput(
                    'longitude_bottom',
                    label = 'Longitude bottom right',
                    value = NA)),
              
              p("Grid mode selected."),
              p("Please provide the upper left coordinates and the bottom right coordinates of the desired grid.")
            ),
            
            p('Coordinates input must be in latitude/logitude Mercator ',
              'projection, in decimal format'),
            
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
      
      # data download tab ####
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
            
            # conditional panel in case of grid  and current (with grid we show the grid
            # for the selected day and variable)
            conditionalPanel(
              condition = "input.point_grid_sel == 'Grid' && input.mode_sel == 'Current'",
              
              # spplot output
              wellPanel(
                h4('Grid Plot'),
                plotOutput('grid_plot', height = '400px')
              )
            ),
            
            # conditional in case of grid and projection
            conditionalPanel(
              condition = "input.point_grid_sel == 'Grid' && input.mode_sel == 'Projection'",
              
              # spplot output
              wellPanel(
                h4('Grid Plot'),
                plotOutput('grid_plot_proj', height = '400px')
              )
            ),
            
            # conditional in case of grid and historical
            conditionalPanel(
              condition = "input.point_grid_sel == 'Grid' && input.mode_sel == 'Historical'",
              
              # spplot output
              wellPanel(
                h4('Grid Plot'),
                plotOutput('grid_plot_hist', height = '400px')
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
              
              # date input (empty until grid is processed) and conditional on mode
              # current
              conditionalPanel(
                condition = "input.mode_sel == 'Current'",
                # variable input
                selectInput(
                  inputId = 'grid_var_sel',
                  label = 'Select a variable to visualize',
                  choices = character(0) # empty until grid is processed
                ),
                # date input
                dateInput(
                  inputId = 'grid_date_sel',
                  label = 'Select a date to visualize'
                )
              ),
              # projection
              conditionalPanel(
                condition = "input.mode_sel == 'Projection'",
                # variable input
                selectInput(
                  inputId = 'grid_var_sel_proj',
                  label = 'Select a variable to visualize',
                  choices = character(0) # empty until grid is processed
                ),
                # date input
                selectizeInput(
                  inputId = 'grid_date_sel_proj',
                  label = 'Select a date to visualize',
                  choices = character(0),
                  options = list(maxOptions = 1200)
                )
              ),
              # historical
              conditionalPanel(
                condition = "input.mode_sel == 'Historical'",
                # variable input
                selectInput(
                  inputId = 'grid_var_sel_hist',
                  label = 'Select a variable to visualize',
                  choices = character(0) # emtpy until grid is processed
                ),
                # date input
                dateInput(
                  inputId = 'grid_date_sel_hist',
                  label = 'Select a date to visualize'
                )
              )
            )
            
            # debug
            # textOutput('debug_date_sel'),
            # textOutput('debug_date_sel_proj')
          )
        )
      ),
      
      #
      # App guide tab ####
      # tabPanel(
      #   title = 'App user guide', icon = icon('book'),
      #   
      #   # a little space
      #   br(),
      #   
      #   # a nice document explaining how to use the app
      #   # includeMarkdown('Docs/app_user_guide.Rmd')
      #   tags$iframe(style = "height:600px; width:80%; scrolling=yes", 
      #               src = "app_user_guide.pdf#zoom=120")
      # ),
      
      # Quality assesment tab ####
      tabPanel(
        title = 'Quality assesment', icon = icon('bug'),
        
        # a little space
        br(),
        
        # decription text
        p('These are the cross-validation results for the process of interpolating',
          ' daily meteorological data. Validation was done by making predictions for',
          ' the location of each metereological station after excluding its data from',
          ' the model. Cross-validation was conducted for each year in the 1976-2016',
          ' period separately.'),
        
        # a little space
        br(),
        
        fluidRow(
          
          # inputs
          column(
            2,
            wellPanel(
              br(), br(),
              selectInput('qa_year', 'Year:', 1976:2017, 1976),
              br(),br(),br(),br(),br(),
              br(),br(),br(),br(),br(),
              br(),br(),br(),br(),br(),
              br(),br(),br(),br(),br(),
              selectInput('qa_var', 'Variable:', qa_vars),
              selectInput('qa_stat', 'Statistic:', qa_statistics, 'MAE'),
              selectInput('qa_map_var', 'Map Variable:',
                          c("MinTemperature.Bias", "MinTemperature.MAE",
                            "MaxTemperature.Bias", "MaxTemperature.MAE",
                            "TemperatureRange.Bias", "TemperatureRange.MAE",
                            "RelativeHumidity.Bias", "RelativeHumidity.MAE",
                            "Radiation.Bias", "Radiation.MAE", "PrecFreq.Obs",
                            "PrecFreq.Pred", "PrecDays.Bias", "PrecDays.RelBias",
                            "TotalPrec.Obs", "TotalPrec.Pred", "TotalPrec.Bias",
                            "TotalPrec.RelBias"), 'MinTemperature.MAE')
            )
          ),
          
          # outputs
          column(
            10,
            # table
            h3('Year Statistics Table:'),
            DT::dataTableOutput('qa_stats_table', width = '75%'),
            
            # a little space
            br(),
            
            fluidRow(
              column(
                6,
                # stat variation plot
                h3('Statistic Temporal Variation:'),
                plotOutput('qa_temporal_var', width = '100%')
              ),
              
              column(
                6,
                # error map
                h3('Stations Bubble Plot by Statistic and Variable'),
                plotOutput('qa_maps', width = '100%')
              )
            )
          )
        )
        
        # # inputs
        # wellPanel(
        #   splitLayout(
        #     cellWidths = c('50%', '25%', '25%'),
        #     selectInput('qa_year', 'Year:', 1976:2016, 1976, width = '20%'),
        #     selectInput('qa_var', 'Variable:', qa_vars, width = '80%'),
        #     selectInput('qa_stat', 'Statistic:', qa_statistics, 'MAE', width = '80%')
        #   )
        # ),
        # 
        # # outputs
        # fluidRow(
        #   
        #   column(
        #     6,
        #     h3('Year Statistics Table:'),
        #     DT::dataTableOutput('qa_stats_table', width = '95%')
        #   ),
        #   
        #   column(
        #     6,
        #     h3('Statistic Temporal Variation:'),
        #     plotOutput('qa_temporal_var')
        #   )
        # ),
        # 
        # # a little space
        # br(),
        # 
        # fluidRow(
        #   
        #   column(
        #     12,
        #     h3('Stations Bubble Plots by Statistic'),
        #     plotOutput('qa_maps')
        #   )
        # )
      )
    )
  ),
  
  #
  # About tab ####
  # (tabwith r package description, disclaimer and so on...) 
  tabPanel(
    title = 'About', icon = icon('info-circle'),
    
    # RMardown document
    includeMarkdown('Docs/Disclaimer.Rmd'),
    
    # an space
    br(), br(),
    
    div(
      style = 'text-align:center',
      fluidRow(
        column(
          4,
          a(
            id = 'logo_grupo',
            href = "http://vegmod.ctfc.cat/",
            img(style = 'display:inline-block',
                src = 'LOGO_Group_scaled.png')
          )
        ),
        column(
          4,
          a(
            id = 'logo_ctfc',
            href = "http://www.ctfc.cat/",
            img(style = 'display:inline-block',
                src = 'logo_ctfc_scaled.png')
          )
        ),
        column(
          4,
          a(
            id = 'logo_creaf',
            href = "http://www.creaf.cat/",
            img(style = 'display:inline-block',
                src = 'logo_creaf_scaled.png')
          )
        )
      )
    )
  )
)
