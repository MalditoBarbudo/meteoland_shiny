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
    includeHTML('Docs/User_Guide.html')
  ),
  
  # meteoland R package examples
  tabPanel(
    title = 'meteoland Examples', icon = icon('flask'),
    
    ## TO DO add some examples of using meteoland with real data. Probably
    ## an html document generated from an Rmd.
    includeHTML('Docs/Examples.html')
  ),
  
  # app tab, with two subsections, a fast guide to use the app and the app
  # itself
  navbarMenu(
    title = 'Shiny app', icon = icon('television'),
    
    # fast user guide submenu
    tabPanel(
      title = 'App user guide', icon = icon('info'),
      
      # user guide will be an html document generated from Rmd
      includeMarkdown('Docs/app_user_guide.Rmd')
    ),
    
    tabPanel(
      title = 'App', icon = icon('television'),
      
      # the real app starts here:
      fluidRow(
        
        # sidebar, but as column
        column(
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
        
        column(
          width = 9,
          
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
            )
          )
        )
      )
    )
  )
)
