###################################
##### minimal example – ui.R #####
###################################
library(shiny) # load shiny at beginning at both scripts
library(shinyGridster)
library(rHighcharts)
source('sources/helper.r')
load('sources/NameClien.RData')
shinyUI(fluidPage(h2(paste('Dashboard combustible rutas',NameCliente)), # standard shiny layout, controls on the
  # left, output on the right
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
    
    # For JustGage, http://justgage.com/
    tags$script(src = 'js/raphael.2.1.0.min.js'),
    tags$script(src = 'js/justgage.1.0.1.min.js'),
    
    # For Highcharts, http://www.highcharts.com/
    tags$script(src = 'js/highcharts.js'),
    
    # For the Shiny output binding for status text and JustGage
    tags$script(src = 'shiny_status_binding.js'),
    tags$script(src = 'justgage_binding.js')
  ),
  
  titlePanel("Detalle por ruta" ), 
  br(),
  sidebarLayout( 
    sidebarPanel(
      h3('Parámetros'),
      p('Seleccione el rango de fechas en el cual desea ver el comportamiento de variables de ruta',align='left')
      , dateRangeInput('dates', min  = DatesOut[1], max= DatesOut[2]
                       ,start  = DatesOut[1], end= DatesOut[2]
                       , h4('Rango de fecha')),
      selectInput('Ruta', 'Rutas', Rutas),
      checkboxGroupInput('Motor', 'Tipo motor', Motor,selected = Motor),
      p('Descargue los datos para el periodo seleccionado'),
      downloadButton('downloadData', 'Descarga'),
      width = 3
    ),
    
    mainPanel(
      navbarPage("Detalle", footer=h6('Equitel BI |JG'),# give the interface a title
                 tabPanel('Resumen rutas',chartOutput("flow"),icon=icon("bar-chart-o")),       
          tabPanel('Detalle rutas',h3(textOutput("RutaSel")),uiOutput('VehicleOut'),icon=icon("tachometer")),
          tabPanel('Análisis rutas',plotOutput("Analisis1"),icon=icon("bar-chart-o")) ,
                 tabPanel('Análisis vehìculos',plotOutput("Analisis2"),icon=icon("bar-chart-o"))         
                 
      )
      
    , width = 9)
  )))