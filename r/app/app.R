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
library(shinyTime)

routes <- readRDS("./data/routes.rds")
werkzaamheden <- readRDS("./data/werkzaamheden.rds")
#maak tijdstempels
werkzaamheden[, `:=`(timestamp_van = as.POSIXct(paste0(van, " ", van_uur), tz = "Europe/Amsterdam"),
                     timestamp_tot = as.POSIXct(paste0(tot, " ", tot_uur), tz = "Europe/Amsterdam"))]


### test
#routes <- readRDS("./R/app/data/routes.rds")
#werkzaamheden <- readRDS("./R/app/data/werkzaamheden.rds")
### /test

ui <- (fluidPage(
  titlePanel("Stremmingen en omleidingen"),
  sidebarLayout(
    sidebarPanel(
      h2( "Selectie" ),
      # __user input -----
      dateInput( 'dateInputVan',
                 label     = "Van", 
                 value     = min(werkzaamheden$van, na.rm = TRUE),
                 min       = min(werkzaamheden$van, na.rm = TRUE),
                 max       = max(werkzaamheden$tot, na.rm = TRUE),
                 format    = "dd-mm-yyyy",
                 startview = "month",
                 weekstart = 1,
                 language  = "nl",
                 autoclose = TRUE ),
      timeInput( inputId      = "tijdInputVan",
                 label        = "",
                 value        = as.ITime("20:00:00"), 
                 seconds      = FALSE,
                 minute.steps = 30L ),
      dateInput( 'dateInputTot',
                 label     = "Tot", 
                 value     = min(werkzaamheden$van, na.rm = TRUE) + 1,
                 min       = min(werkzaamheden$van, na.rm = TRUE),
                 max       = max(werkzaamheden$tot, na.rm = TRUE),
                 format    = "dd-mm-yyyy",
                 startview = "month",
                 weekstart = 1,
                 language  = "nl",
                 autoclose = TRUE ),
      timeInput( inputId      = "tijdInputTot",
                 label        = "",
                 value        = as.ITime("05:00:00"), 
                 seconds      = FALSE,
                 minute.steps = 30L ),
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
    #maak filterwaarden op basis van input
    van.filter <- as.POSIXct(paste0(as.character(input$dateInputVan), "T", 
                                    format(input$tijdInputVan, "%H:%M")), format = "%Y-%m-%dT%H:%M", 
                             tz = "Europe/Amsterdam")
    tot.filter <- as.POSIXct(paste0(as.character(input$dateInputTot), "T", 
                                    format(input$tijdInputTot, "%H:%M")), format = "%Y-%m-%dT%H:%M", 
                             tz = "Europe/Amsterdam")
    temp.sel <- werkzaamheden[timestamp_van <= tot.filter & timestamp_tot >= van.filter, ]
    ### test
    #temp.sel <- werkzaamheden[van <= as.Date("2022-01-02") & tot >= as.Date("2022-01-02"), ]
    ### /test
    return(temp.sel)
  })
  
  # Afdwingen dat eindtijd na de begintijd ligt
  observeEvent({
    input$dateInputVan 
    input$dateInputTot
    input$tijdInputVan
    input$tijdInputTot
    }, {
      if (input$dateInputVan > input$dateInputTot) {
        updateDateInput(session, "dateInputTot", value = input$dateInputVan + 1)
        }
      if (input$dateInputVan == input$dateInputTot & 
          input$tijdInputVan >= input$tijdInputTot) {
        updateTimeInput(session, "tijdInputTot", value = input$tijdInputVan + 1800)
      }
    })
  # selecteer de werkzaamheden die binnen het opgegeven tijdvenster vallen
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