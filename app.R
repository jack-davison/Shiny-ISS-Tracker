
library(shiny)
library(leaflet)
library(curl)
library(bslib)

# PREP
curl <-
  curl::curl_fetch_memory("https://api.wheretheiss.at/v1/satellites/25544")
json <- rawToChar(curl$content)
lst <- jsonlite::fromJSON(json)
iss_df <- as.data.frame(lst)

icon <-
  makeIcon(
    "data/International_Space_Station.svg",
    iconWidth = 100,
    iconHeight = 100,
    iconAnchorX = 50,
    iconAnchorY = 50, 
  )

map <- leaflet() |>
  addProviderTiles(provider = "OpenStreetMap", layerId = "basemap") |>
  addMarkers(data = iss_df, layerId = "iss", icon = icon) |>
  setView(lng = iss_df$longitude,
          lat = iss_df$latitude,
          zoom = 4) |>
  addMiniMap(toggleDisplay = TRUE, position = "topright")

quick_value_box <- function(var, icon, theme = "secondary") {
  value_box(
    title = var,
    theme = value_box_theme(name = theme),
    showcase = bsicons::bs_icon(icon),
    value = textOutput(var)
  )
}

# UI
ui <- bslib::page_sidebar(
  shiny::useBusyIndicators(),
  title = "Where is the ISS?",
  theme = bs_theme(bootswatch = "solar"),
  sidebar = sidebar(
    HTML(
      "This map tracks the international space station (ISS) by querying the <a href='https://wheretheiss.at/w/developer'>'Where is the ISS?'</a> API just over once a second."
    ),
    hr(),
    selectInput(
      "selectBaseMap",
      label = "Base Map",
      choices = c("Vector" = "OpenStreetMap", "Satellite" = "Esri.WorldImagery"),
      selected = "OpenStreetMap"
    ),
    sliderInput(
      "sliderZoom", "Zoom Level", min = 1, max = 18, value = 4
    ),
    p(
      "By default, the map will follow the ISS. Uncheck this box to get free control over the map, including the zoom level."
    ),
    checkboxInput("checkCentre", "Centre ISS", value = TRUE),
    hr(),
    HTML('ISS image source: <p><a href="https://commons.wikimedia.org/wiki/File:International_Space_Station.svg">NASA</a>, Public domain, via Wikimedia Commons</p>'),
    HTML("<p>Developed by <a href='https://jack-davison.github.io/'>Jack Davison</a></p>")
  ),
  layout_column_wrap(
    card(leaflet::leafletOutput("map")),
    bslib::layout_column_wrap(
      heights_equal = "row",
      width = 1,
      quick_value_box("latitude", "arrow-left-right", "primary"),
      quick_value_box("longitude", "arrow-down-up", "primary"),
      quick_value_box("altitude", "ladder"),
      quick_value_box("velocity", "speedometer"),
      quick_value_box("visibility", "eye-fill")
    ),
    width = 1 / 2,
    style = htmltools::css(grid_template_columns = "3fr 1fr")
  )
)


server <- function(input, output, session) {
  output$map <- renderLeaflet(map)
  
  timer <- reactiveTimer(1100, session)
  
  iss_pos <- reactive({
    timer()
    curl <-
      curl::curl_fetch_memory("https://api.wheretheiss.at/v1/satellites/25544")
    json <- rawToChar(curl$content)
    lst <- jsonlite::fromJSON(json)
    as.data.frame(lst)
  })
  
  icon <- reactive({
    makeIcon(
      switch(input$selectBaseMap,
             "OpenStreetMap" = "data/International_Space_Station.svg",
             "Esri.WorldImagery" = "data/International_Space_Station_white.svg"),
      iconWidth = 80*(30/18.9),
      iconHeight = 80,
      iconAnchorX = (80/2)*(30/18.9),
      iconAnchorY = (80/2), 
    )
  })
  
  observeEvent(iss_pos(), {
    leafletProxy("map", session) |>
      removeMarker("iss") |>
      addMarkers(
        data = iss_pos(),
        lat = ~ latitude,
        lng = ~ longitude,
        layerId = "iss",
        icon = icon()
      )
    
    if (input$checkCentre) {
      leafletProxy("map", session) |>
        setView(
          lng = iss_pos()$longitude,
          lat = iss_pos()$latitude,
          zoom = input$sliderZoom
        )
    }
  })
  
  observeEvent(input$selectBaseMap, {
    leafletProxy("map", session) |>
      removeTiles("basemap") |>
      addProviderTiles(provider = input$selectBaseMap, layerId = "basemap")
  })
  
  observeEvent(input$map_zoom, {
    if (!input$checkCentre) {
      updateSliderInput(session, "sliderZoom", value = input$map_zoom)
    }
  })
  
  output$longitude <- renderText({
    dat <- iss_pos()
    dat$longitude
  })
  
  output$latitude <- renderText({
    dat <- iss_pos()
    dat$latitude
  })
  
  output$altitude <- renderText({
    dat <- iss_pos()
    scales::label_comma(accuracy = 0.001, suffix = " km")(dat$altitude)
  })
  
  output$velocity <- renderText({
    dat <- iss_pos()
    scales::label_comma(accuracy = 0.001, suffix = " km/h")(dat$velocity)
  })
  
  output$visibility <- renderText({
    dat <- iss_pos()
    dat$visibility
  })
  
  
}

shinyApp(ui = ui, server = server)