## Meteoland service UI

# libraries
library(shiny)
library(leaflet)
library(dygraphs)

# fluid page ui
fluidPage(
  
  # application title
  titlePanel('Meteoland Service'),
  
  # sidebar layout. this will have a dynamic UI, where selecting between historical and
  # projection modes will change the inputs in the sidebar
  sidebarLayout(
    
    # sidebar code
    sidebarPanel(
      
      # sidebar width (as default value, 4, maybe is too large)
      width = 3,
      
      # panel for fixed inputs (those that will not change regarding the mode)
      wellPanel(
        
        # mode selector. The user can select between historical and projected
        selectInput(
          inputId = 'mode_sel',
          label = 'Please select the desired mode:',
          choices = c('Historical', 'Projection')
        ),
        
        # latitude and longitude selector. To be able to show both in the same
        # line we must to rely in some html/css magic ;)
        div(style = "display: inline-block;vertical-align:top; width: 140px;",
            numericInput(
              'latitude',
              label = 'Latitude',
              value = NA)),
        
        div(style = "display: inline-block;vertical-align:top; width: 140px;",
            numericInput(
              'longitude',
              label = 'Longitude',
              value = NA))
      ),
      
      # a little space
      br(),
      
      # Dinamic ui to show inputs and buttons depending on the mode selected
      uiOutput(
        outputId = 'dinamic_inputs'
      ),
      
      # Action button to start the interpolation
      actionButton(
        'ready_btn',
        'Ready',
        icon = icon('check')
      )
    ),
    
    # Main panel. Here will be several tabsets (map, data, manual, examples,
    # disclaimer...)
    mainPanel(
      
      # map tabset
      tabsetPanel(
        
        # general info/aspect of tabset panel
        id = 'main_tab_panel',
        type = 'pills',
        
        # map
        tabPanel(
          title = 'Map',
          icon = icon('globe'),
          leafletOutput('map', height = 600)
        ),
        
        # data
        tabPanel(
          title = 'Data',
          icon = icon('area-chart'),
          
          # dygraph
          dygraphOutput('data'),
          
          # a little space
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
        ),
        
        # manual
        tabPanel(
          title = 'Manual',
          icon = icon('book')
          ## TO DO select an output to show the manual (Rmd?)
        ),
        
        # examples
        tabPanel(
          title = 'Examples',
          icon = icon('flask')
          ## TO DO select an output to show examples (R code or Rmd?)
        ),
        
        # disclaimer
        tabPanel(
          title = 'Disclaimer',
          icon = icon('info-circle'),
          includeMarkdown('Docs/Disclaimer.Rmd')
        )
      )
    )
    
  )
  
)
