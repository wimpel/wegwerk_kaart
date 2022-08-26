#
# publicatie:
# https://vwmdata.shinyapps.io/kaartHenk/
#

library(data.table)
library(leaflet)
library(leaflet.extras2)
library(leafpop)
library(sf)
library(DT)
library(shiny)
library(shinyTime)
library(htmltools)

werkzaamheden <- readRDS("./data/routes.rds")

### test
#routes <- readRDS("./R/app/data/routes.rds")
# werkzaamheden <- readRDS("./R/app/data/routes.rds")
### /test

ui <- (fluidPage(
  titlePanel("Stremmingen en omleidingen"),
  sidebarLayout(
    sidebarPanel(
      h2( "Selectie" ),
      # __user input -----
      dateInput( 'dateInputVan',
                 label     = "Van", 
                 value     = min(as.Date(werkzaamheden$van, tz = "Europe/Amsterdam"), na.rm = TRUE),
                 min       = min(as.Date(werkzaamheden$van, tz = "Europe/Amsterdam"), na.rm = TRUE),
                 max       = max(as.Date(werkzaamheden$tot, tz = "Europe/Amsterdam"), na.rm = TRUE),
                 format    = "dd-mm-yyyy",
                 startview = "month",
                 weekstart = 1,
                 language  = "nl",
                 autoclose = TRUE ),
      timeInput( inputId      = "tijdInputVan",
                 label        = "",
                 value        = as.POSIXct("20:00:00", "%H:%M:%S", tz = "Europe/Amsterdam"), 
                 seconds      = FALSE,
                 minute.steps = 30L ),
      dateInput( 'dateInputTot',
                 label     = "Tot", 
                 value     = min(as.Date(werkzaamheden$van, tz = "Europe/Amsterdam"), na.rm = TRUE) + 1,
                 min       = min(as.Date(werkzaamheden$van, tz = "Europe/Amsterdam"), na.rm = TRUE),
                 max       = max(as.Date(werkzaamheden$tot, tz = "Europe/Amsterdam"), na.rm = TRUE),
                 format    = "dd-mm-yyyy",
                 startview = "month",
                 weekstart = 1,
                 language  = "nl",
                 autoclose = TRUE ),
      timeInput( inputId      = "tijdInputTot",
                 label        = "",
                 value        = as.POSIXct("05:00:00", "%H:%M:%S", tz = "Europe/Amsterdam"), 
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
                   )#,
                   #column(width = 12,
                  #        uiOutput("pick_col2"),
                  #        tableOutput("dataTabel2")
                  # )
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
    temp.sel <- werkzaamheden[werkzaamheden$van <= tot.filter & werkzaamheden$tot >= van.filter, ]
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

  tabelData <- reactive({
    req(werk.sel())
    temp <- copy(werk.sel())
    temp %>%
      dplyr::mutate(van = as.character(van),
                    tot = as.character(tot)) %>%
      sf::st_drop_geometry()
  })
  
  tabelData2 <- werkzaamheden %>% sf::st_drop_geometry()
  
  output$dataTabel <- renderTable( tabelData() )
  output$dataTabel2 <- renderTable( tabelData2() )

  output$leafletMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addScaleBar( position = "bottomleft" )
  })

  plaatjes <- reactive({
    req(werk.sel())
    werk.sel()$plaatje
  })
  
  observe({
    leafletProxy("leafletMap") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addArrowhead(data = werk.sel(),
                   color = ~kleur,
                   fillColor = ~kleur,
                   opacity = 1,
                   label = ~lapply(label, HTML),
                   popup = leafpop::popupImage(plaatjes(), src = "local", embed = TRUE),
                   options = arrowheadOptions(size = "15px",frequency = "endonly")
      ) %>%
      #opnieuw opbouwen legenda
      clearControls() 
    # %>%
    #   addLayersControl(
    #     baseGroups = c( "OSM (default)", "PDOK luchtfoto" ),
    #     overlayGroups = c( "Stremming" ),
    #     options = layersControlOptions( collapsed = FALSE ) )
  })

})

shinyApp(ui = ui, server = server)