#
# publicatie:
# https://vwmdata.shinyapps.io/kaartHenk/
#

library(data.table)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(DT)
library(shiny)

routes <- readRDS("./data/routes.rds")
werkzaamheden <- readRDS("./data/werkzaamheden.rds")

### test
# routes <- readRDS("./R/app/data/routes.rds")
# werkzaamheden <- readRDS("./R/app/data/werkzaamheden.rds")
### /test


ui <- (fluidPage(
  titlePanel("Stremmingen en omleidingen"),
  sidebarLayout(
    sidebarPanel(
      h2( "Selectie" ),
      # __user input -----
      dateInput( 'dateInput',
                 label     = "Datum", 
                 value     = min(werkzaamheden$van, na.rm = TRUE),
                 min       = min(werkzaamheden$van, na.rm = TRUE),
                 max       = max(werkzaamheden$tot, na.rm = TRUE),
                 format    = "dd-mm-yyyy",
                 startview = "month",
                 weekstart = 1,
                 language  = "nl",
                 autoclose = TRUE )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel( "Kaart", leafletOutput( "leafletMap", height = "700px" ) ), 
        tabPanel("Tabellen", 
                 fluidRow(
                   column(width = 12,
                          uiOutput("pick_col1"),
                          tableOutput("dataTabel")
                   ),
                   column(width = 12,
                          uiOutput("pick_col2"),
                          tableOutput("dataTabel2")
                   )
                 )
        )
      )
    )
  )
))

server <- ( function( input, output, session ) {

  werk.sel <- reactive({
    temp.sel <- werkzaamheden[van <= input$dateInput & tot >= input$dateInput, ]
    ### test
    #temp.sel <- werkzaamheden[van <= as.Date("2022-01-02") & tot >= as.Date("2022-01-02"), ]
    ### /test
    return(temp.sel)
  })
  
  route.sel <- reactive({
    req(werk.sel())
    temp.sel <- routes[routes$groep %in% unique(werk.sel()$groep), ]
    return(temp.sel)
  })
  
  tabelData <- reactive({
    req(werk.sel())
    temp <- copy(werk.sel())
    temp[, van := as.character(van)]
    temp[, tot := as.character(tot)]
    return(temp)
  })
  
  tabelData2 <- reactive({
    req(werkzaamheden)
    temp <- copy(werkzaamheden)
    temp[, van := as.character(van)]
    temp[, tot := as.character(tot)]
    return(temp)
  })
  
  output$dataTabel <- renderTable( tabelData() )
  output$dataTabel2 <- renderTable( tabelData2() )

  output$leafletMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addScaleBar( position = "bottomleft" )
  })

  observe({
    leafletProxy("leafletMap" ) %>%
      clearMarkers() %>%
      clearShapes() %>%
      #addPolylines(data = route.sel()) %>%
      addArrowhead(data = route.sel(),
                   color = ~kleur,
                   fillColor = ~kleur,
                   opacity = 1,
                   options = arrowheadOptions(size = "15px",frequency = "endonly")
      ) %>%
      #opnieuw opbouwen legenda
      clearControls() %>%
      addLayersControl(
        baseGroups = c( "OSM (default)", "PDOK luchtfoto" ),
        overlayGroups = c( "Stremming" ),
        options = layersControlOptions( collapsed = FALSE ) )
  })

})

shinyApp(ui = ui, server = server)