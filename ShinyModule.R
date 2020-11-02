library(move)
library(shiny)
library(leaflet)

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Interactive Leaflet Map - ID selection and time slider"),
    leafletOutput(ns("leafmap"))
  )
}

shinyModuleConfiguration <- function(id, input) {
  ns <- NS(id)
  configuration <- list()
  configuration
}

shinyModule <- function(input, output, session, data) {
  dataObj <- reactive({ data })
  current <- reactiveVal(data)
  
  ID <- namesIndiv(data)
  COL <- rainbow(n=length(ID))
  data_spl <- move::split(data)

  output$leafmap <- renderLeaflet({
    bounds <- as.vector(bbox(extent(dataObj())))
    outl <- leaflet() %>% 
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap",group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial")
      
      for (i in seq(along=ID))
        {
          outl <- outl %>%
            addPolylines(data = coordinates(data_spl[[i]]), color = COL[i], group = ID[i], weight=2) %>%
            addCircles(data = data_spl[[i]], fillOpacity = 0.3, opacity = 0.5, color = COL[i], group = ID[i])
        }
      
      outl <- outl %>%
        addLegend(position= "topright", colors=COL, 
                labels=ID ,opacity = 0.7, title = "Animals") %>%
      addScaleBar(position="bottomleft", 
                  options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = F, updateWhenIdle = TRUE)) %>%
      addLayersControl(
        baseGroups = c("StreetMap", "Aerial"),
        overlayGroups = ID,
        options = layersControlOptions(collapsed = FALSE)
      )
    outl   
  })  
  
  return(reactive({ current() }))
}


# comment: package leaftime could be an option for time slider, but it seems not yet possible to control it with different layers

