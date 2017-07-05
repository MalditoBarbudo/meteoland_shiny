## Meteoland service UI

# libraries
library(shiny)
library(leaflet)
library(DT)

# fluid page ui
fluidPage(
  
  # application title
  titlePanel('Meteoland Service'),
  
  # sidebar layout. this will be a dynamic UI, where selecting between historical and
  # projection modes will change the inputs in the sidebar
  sidebarLayout(
    
    # sidebar code
    sidebarPanel(
      
      # sidebar width (as default value, 4, maybe is too large)
      width = 3,
      
      # mode selector. The user can select between projected or historical
      wellPanel(
        selectInput(
          inputId = 'mode_sel',
          label = 'Please select the desired mode:',
          choices = c('Historical', 'Projection')
        )
      ),
      
      # a little spae
      br(),
      
      # Dinamic ui to show selectors and buttons
      uiOutput(
        outputId = 'dinamic_inputs'
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
          leafletOutput('map')
        ),
        
        # data
        tabPanel(
          title = 'Data',
          icon = icon('table'),
          DT::dataTableOutput('data')
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
