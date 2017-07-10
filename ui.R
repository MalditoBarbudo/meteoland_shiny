## Meteoland service UI
## Design and development by Miquel de Cáceres, Antoine Cabon and Víctor Granda

# libraries
library(shiny)
library(leaflet)
library(dygraphs)

# Navbar page layout. This is intended to offer a quick glance of the app, the
# package, examples and manuals, and finally, an app to use the package with
# data from Catalonia

navbarPage(
  title = 'meteoland R package',
  id = 'navbar_meteoland',
  
  # About tab (tabwith r package description, disclaimer and so on...)
  tabPanel(
    title = 'About', icon = icon('info-circle'),
    
    # RMardown document
    includeMarkdown('Docs/Disclaimer.Rmd')
  ),
  
  # meteoland R package manual tab
  tabPanel(
    title = 'meteoland User Guide', icon = icon('book'),
    
    # Html document (generated from an Rmd)
    includeMarkdown('Docs/User_Guide.Rmd')
  ),
  
  # meteoland R package examples
  tabPanel(
    title = 'meteoland Examples', icon = icon('flask'),
    
    ## TO DO add some examples of using meteoland with real data. Probably
    ## an html document generated from an Rmd.
    includeMarkdown('Docs/Examples.Rmd')
  ),
  
  tabPanel(
    title = 'Shiny App', icon = icon('television'),
    
    # in order to obtain two tabs (map and data) we need to nest a tabsetPanel here
    tabsetPanel(
      type = 'pills', id = 'shiny_tabs',
      
      # user input tab
      tabPanel(
        title = 'User input', icon = icon('keyboard-o'),
        
        # a little space
        br(),
        
        sidebarLayout(
          sidebarPanel(
            # sidebar width
            width = 3,
            
            # panel for fixed inputs
            wellPanel(
              
              # Mode selector
              selectInput(
                inputId = 'mode_sel',
                label = 'Please select the desired mode:',
                choices = c('Historical', 'Projection')
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
                    value = NA))
            ),
            
            # Dinamic ui to show inputs and buttons depending on the mode selected
            uiOutput(
              outputId = 'dinamic_inputs'
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
        
        # dygraph
        dygraphOutput('data'),
        
        # a little spaces
        br(), br(), br(),
        
        # fluid row to show the download button  and the variable selector
        fluidRow(
          column(
            9,
            radioButtons(
              'var_sel',
              'Select the variable to visualize',
              choices = c(
                'Tmax', 'Tmin', 'RH', 'Precev', 'Precam'
              ),
              selected = 'Tmax',
              inline = TRUE
            )
          ),
          
          column(
            3,
            actionButton(
              'download_btn',
              'Download',
              icon = icon('download')
            )
          )
        )
      )
    )
  )
)
